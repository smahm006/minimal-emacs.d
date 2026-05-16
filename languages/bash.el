;;; bash.el --- Bash/shell language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package bash
  :ensure nil
  :mode
  ("\\.sh\\'" . bash-ts-mode)
  ("\\.bash\\'" . bash-ts-mode)
  ("\\.bashrc\\'" . bash-ts-mode)
  ("\\.bash_profile\\'" . bash-ts-mode)
  :interpreter ("bash" . bash-ts-mode)
  :hook
  (bash-ts-mode . eglot-ensure)
  (bash-ts-mode . executable-make-buffer-file-executable-if-script-p)
  (bash-ts-mode . (lambda ()
                    (define-key me/run-map (kbd "r") #'me/bash-run)
                    (define-key me/run-map (kbd "c") #'me/bash-check)
                    (define-key me/run-map (kbd "f") #'me/bash-format)))
  ;; Fallback hooks for sh-mode when bash-ts-mode is unavailable
  (sh-mode . eglot-ensure)
  (sh-mode . executable-make-buffer-file-executable-if-script-p)
  (sh-mode . (lambda ()
               (define-key me/run-map (kbd "r") #'me/bash-run)
               (define-key me/run-map (kbd "c") #'me/bash-check)
               (define-key me/run-map (kbd "f") #'me/bash-format)))
  :preface
  (defun me/bash-run ()
    "Run the current buffer with bash."
    (interactive)
    (compile (format "bash %s"
                     (shell-quote-argument buffer-file-name))))
  (defun me/bash-format ()
    "Format the current buffer using apheleia (shfmt)."
    (interactive)
    (apheleia-format-buffer '(shfmt)))
  (defun me/bash-check ()
    "Check the current buffer with shellcheck."
    (interactive)
    (compile (format "shellcheck %s"
                     (shell-quote-argument buffer-file-name))))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((bash-ts-mode sh-mode) . ("bash-language-server" "start")))))
