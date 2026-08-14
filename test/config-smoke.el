;;; config-smoke.el --- Isolated smoke tests for the Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Load this file in a named daemon after the real configuration has loaded:
;;
;;   emacsclient -s emacs-smoke -e '(me/config-smoke-run)'
;;
;; The tests use temporary files and directories only. They return a plist so
;; callers can inspect the report programmatically, and print a readable report
;; for interactive use.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'project)

(defconst me/config-smoke-language-cases
  '(("bash" ".sh" "#!/usr/bin/env bash\necho smoke\n" bash-ts-mode)
    ("c" ".c" "int main(void) { return 0; }\n" c-ts-mode)
    ("cpp" ".cpp" "int main() { return 0; }\n" c++-ts-mode)
    ("css" ".css" "body { color: red; }\n" css-ts-mode)
    ("go" ".go" "package main\n\nfunc main() {}\n" go-ts-mode)
    ("java" ".java" "class Smoke { public static void main(String[] a) {} }\n" java-ts-mode)
    ("javascript" ".js" "const smoke = true;\n" js-ts-mode)
    ("json" ".json" "{\"smoke\": true}\n" json-ts-mode)
    ("markdown" ".md" "# Smoke\n\nText.\n" markdown-ts-mode)
    ("python" ".py" "def smoke():\n    return True\n\nsmoke()\n" python-ts-mode)
    ("rust" ".rs" "fn main() {}\n" rust-ts-mode)
    ("svelte" ".svelte" "<script>let smoke = true;</script>\n<div>{smoke}</div>\n" svelte-ts-mode)
    ("tex" ".tex" "\\documentclass{article}\n\\begin{document}\nSmoke\n\\end{document}\n" LaTeX-mode)
    ("toml" ".toml" "[smoke]\nvalue = true\n" toml-ts-mode)
    ("typescript" ".ts" "const smoke: boolean = true;\n" typescript-ts-mode)
    ("tsx" ".tsx" "const Smoke = () => <div />;\n" tsx-ts-mode)
    ("web" ".html" "<!doctype html><html><body>Smoke</body></html>\n" mhtml-mode)
    ("yaml" ".yaml" "smoke: true\n" yaml-ts-mode)
    ("zig" ".zig" "pub fn main() void {}\n" zig-ts-mode))
  "Representative temporary files and their expected major modes.")

(defconst me/config-smoke-minor-mode-symbols
  '(eglot-managed-mode pet-mode dtrt-indent-mode treesit-fold-mode
    combobulate-mode beacon-mode dimmer-mode visual-line-mode)
  "Minor modes whose state is useful in the smoke report.")

(defcustom me/config-smoke-open-timeout 5
  "Maximum seconds allowed for opening and fontifying one test file."
  :type 'number
  :group 'me/config-smoke)

(defun me/config-smoke--messages-since (marker)
  "Return messages recorded after MARKER in the `*Messages*` buffer."
  (when-let* ((buffer (get-buffer "*Messages*")))
    (with-current-buffer buffer
      (buffer-substring-no-properties marker (point-max)))))

(defun me/config-smoke--message-marker ()
  "Return the current end marker of the `*Messages*` buffer."
  (if-let* ((buffer (get-buffer "*Messages*")))
      (with-current-buffer buffer (point-max))
    0))

(defun me/config-smoke--minor-mode-state ()
  "Return the configured minor-mode states in the current buffer."
  (mapcar (lambda (mode)
            (cons mode (and (boundp mode) (symbol-value mode))))
          me/config-smoke-minor-mode-symbols))

(defun me/config-smoke--eglot-processes ()
  "Return process names that look like Eglot language servers."
  (cl-remove-if-not
   (lambda (process)
     (let ((name (process-name process)))
       (or (string-match-p "eglot" name)
           (string-match-p "language-server\\|gopls\\|rust-analyzer\\|clangd" name))))
   (process-list)))

(defun me/config-smoke--open-language-file (root case)
  "Open the language CASE below ROOT and return its smoke result."
  (pcase-let ((`(,name ,extension ,contents ,expected-mode) case))
    (let* ((file (expand-file-name (concat name extension) root))
           (message-marker (me/config-smoke--message-marker))
           (before-processes (me/config-smoke--eglot-processes))
           (started (float-time))
           buffer elapsed error-data)
      (with-temp-file file (insert contents))
      (condition-case error-value
          (with-timeout (me/config-smoke-open-timeout
                         (error "Timed out after %.1fs" me/config-smoke-open-timeout))
            (setq buffer (find-file-noselect file))
            (with-current-buffer buffer
              (font-lock-ensure))
            (setq elapsed (- (float-time) started)))
        (error
         (setq error-data (error-message-string error-value)
               elapsed (- (float-time) started))))
      (let ((after-processes (me/config-smoke--eglot-processes)))
        (prog1
            (list :name name
                  :file file
                  :elapsed elapsed
                  :expected-mode expected-mode
                  :actual-mode (and buffer
                                    (buffer-local-value 'major-mode buffer))
                  :minor-modes (and buffer
                                    (with-current-buffer buffer
                                      (me/config-smoke--minor-mode-state)))
                  :eglot-managed (and buffer
                                      (with-current-buffer buffer
                                        (or (and (boundp 'eglot-managed-mode)
                                                 eglot-managed-mode)
                                            (and (boundp 'eglot--managed-mode)
                                                 eglot--managed-mode))))
                  :eglot-process-started
                  (cl-set-difference after-processes before-processes)
                  :error error-data
                  :messages (me/config-smoke--messages-since message-marker))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(defun me/config-smoke--dired-and-path-result (root)
  "Exercise Dired below ROOT and report path-copying availability."
  (let* ((directory (expand-file-name "project" root))
         (file (expand-file-name "sample.txt" directory))
         (message-marker (me/config-smoke--message-marker))
         buffer refresh-error)
    (make-directory (expand-file-name ".git" directory) t)
    (with-temp-file file (insert "smoke\n"))
    (condition-case error-value
        (progn
          (setq buffer (dired-noselect directory))
          (with-current-buffer buffer
            (revert-buffer nil t)))
      (error
       (setq refresh-error (error-message-string error-value))))
    (let ((path-copying
           (if (fboundp 'me/copy-file-path)
               (with-current-buffer buffer
                 (condition-case error-value
                     (progn
                       (dired-goto-file file)
                       (call-interactively #'me/copy-file-path)
                       (list :available t :error nil))
                   (error (list :available t
                                :error (error-message-string error-value)))))
             (list :available nil :reason "me/copy-file-path is not defined"))))
      (prog1
          (list :directory directory
                :refresh-error refresh-error
                :path-copying path-copying
                :messages (me/config-smoke--messages-since message-marker))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun me/config-smoke--close-temporary-buffers (root)
  "Kill any buffers visiting files below ROOT."
  (dolist (buffer (buffer-list))
    (when-let* ((file (buffer-file-name buffer)))
      (when (file-in-directory-p file root)
        (kill-buffer buffer)))))

(defun me/config-smoke-run (&optional print-report)
  "Run the isolated configuration smoke test and return its report.

When PRINT-REPORT is non-nil, print a readable report to the `*config-smoke*`
buffer. The default is non-nil for interactive calls."
  (interactive (list t))
  (let* ((root (make-temp-file "me-config-smoke-" t))
         (started (float-time))
         results dired-result report)
    (unwind-protect
        (progn
          (setq results
                (mapcar (lambda (case)
                          (me/config-smoke--open-language-file root case))
                        me/config-smoke-language-cases))
          (setq dired-result (me/config-smoke--dired-and-path-result root))
          (setq report
                (list :generated-at (current-time-string)
                      :elapsed (- (float-time) started)
                      :root root
                      :language-results results
                      :dired-result dired-result))
          (when print-report
            (with-current-buffer (get-buffer-create "*config-smoke*")
              (erase-buffer)
              (insert (format "Configuration smoke test: %s\n\n"
                              (plist-get report :generated-at)))
              (dolist (result results)
                (insert (format "%s: %.3fs, expected %s, actual %s, Eglot %s\n"
                                (plist-get result :name)
                                (plist-get result :elapsed)
                                (plist-get result :expected-mode)
                                (plist-get result :actual-mode)
                                (if (plist-get result :eglot-managed) "managed" "off")))
                (when-let* ((error-data (plist-get result :error)))
                  (insert (format "  ERROR: %s\n" error-data)))
                (dolist (process (plist-get result :eglot-process-started))
                  (insert (format "  Eglot process started: %s\n"
                                  (process-name process))))
                (dolist (minor-mode (plist-get result :minor-modes))
                  (when (cdr minor-mode)
                    (insert (format "  minor mode: %s\n" (car minor-mode)))))
                (when-let* ((messages (plist-get result :messages)))
                  (insert (format "  messages:\n%s" messages))))
              (insert (format "\nDired refresh error: %s\n"
                              (or (plist-get dired-result :refresh-error) "none")))
              (insert (format "Path copying: %S\n"
                              (plist-get dired-result :path-copying)))
              (display-buffer (current-buffer))))
          report)
      (me/config-smoke--close-temporary-buffers root)
      (delete-directory root t))))

(provide 'config-smoke)

;;; config-smoke.el ends here
