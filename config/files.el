;;; files.el --- File, buffer and project management -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Buffer management
(use-package ibuffer
  :ensure nil
  :bind
  (:map me/buffer-map
        ("o" . me/kill-other-buffers))
  (:map ctl-x-map
        ("B" . me/temporary-buffer))
  :init
  (me/protected-buffers)
  :preface
  (defvar me/protected-buffers '("*scratch*" "*Messages*")
    "Buffers that should never be killed.")
  (defun me/protected-buffers ()
    "Protect some buffers from being killed."
    (dolist (buffer me/protected-buffers)
      (with-current-buffer buffer
        (emacs-lock-mode 'kill))))
  (defun me/temporary-buffer ()
    "Create and switch to a timestamped temporary buffer."
    (interactive)
    (switch-to-buffer
     (get-buffer-create
      (concat "tmp-" (format-time-string "%m.%dT%H.%M.%S")))))
  (defun me/kill-other-buffers ()
    "Kill all buffers except the current one and protected buffers."
    (interactive)
    (when (and (featurep 'eglot) (fboundp 'eglot-shutdown-all))
      (eglot-shutdown-all))
    (mapc #'kill-buffer
          (cl-remove-if
           (lambda (x)
             (or (eq x (current-buffer))
                 (member (buffer-name x) me/protected-buffers)))
           (buffer-list)))
    (delete-other-windows))
  :config
  (setq switch-to-buffer-obey-display-actions t)
  (setq switch-to-buffer-in-dedicated-window 'pop))

;;; bufferfile — rename and delete buffers together with their files
(use-package bufferfile
  :bind
  (:map me/buffer-map
        ("r" . bufferfile-rename)
        ("d" . bufferfile-delete)))

;;; Unique buffer names
(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;;; Project management
(use-package project
  :ensure nil
  :bind
  (:map ctl-x-map
        ("C-p" . project-find-file)
        :map project-prefix-map
        ("l" . project-list-buffers)
        ("F" . project-forget-project)
        ("S" . me/project-save-all-buffers)
        ("s" . project-search))
  :preface
  (defun me/project-save-all-buffers (&optional proj arg)
    "Save all modified file-visiting buffers in the current project silently."
    (interactive)
    (let* ((proj (or proj (project-current)))
           (buffers (project-buffers proj)))
      (dolist (buf buffers)
        (with-current-buffer (or (buffer-base-buffer buf) buf)
          (when (and (buffer-file-name buf)
                     (buffer-modified-p buf))
            (let ((buffer-save-without-query t))
              (save-buffer arg)))))))
  :config
  (setq project-buffers-viewer 'project-list-buffers-ibuffer)
  (setq project-kill-buffers-display-buffer-list t)
  (setq project-switch-commands
        '((project-find-file    "Find file"  "f")
          (project-find-dir     "Find dir"   "d")
          (project-dired        "Dired"      "D")
          (consult-ripgrep      "ripgrep"    "r")
          (magit-project-status "Magit"      "m")
          (agent-shell          "Agent"      "a")))
  (setq project-vc-extra-root-markers '(".project")))

(defun me/path-at-point ()
  "Return the file represented by the current buffer or Dired point."
  (if (derived-mode-p 'dired-mode)
      (dired-get-file-for-visit)
    (or buffer-file-name
        (user-error "This command requires a file or Dired entry"))))

(defun me/copy-file-path (arg)
  "Copy a path to the kill ring.
With no prefix, copy a project-relative path.  With `C-u', copy an absolute
path.  With `C-u 0', copy only the basename."
  (interactive "P")
  (let* ((path (file-truename (me/path-at-point)))
         (result
          (cond
           ((equal arg 0) (file-name-nondirectory path))
           ((consp arg) path)
           ((when-let* ((project (project-current)))
              (file-relative-name path (project-root project))))
           (t (user-error "No project found; use C-u for an absolute path")))))
    (kill-new result)
    (message "Copied %s" result)))

;;; Dired — file manager
(use-package dired
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :bind
  (:map dired-mode-map
        ("<tab>"     . dired-find-file)
        ("M-n"       . dired-find-file)
        ("<backtab>" . dired-up-directory)
        ("M-p"       . dired-up-directory)
        ("."         . dired-omit-mode)
        ("C-+"       . dired-create-empty-file)
        ("Q"         . me/dired-kill-all-buffers))
  (:map me/file-map
        ("d" . me/dired-jump-and-revert)
        ("D" . dired)
        ("f" . find-file-at-point)
        ("y" . me/copy-file-path))
  :preface
  (defun me/dired-jump-and-revert ()
    "Open Dired at the current file's directory."
    (interactive)
    (dired-jump))
  (defun me/dired-kill-all-buffers ()
    "Kill all dired and image-dired buffers."
    (interactive)
    (mapc (lambda (buffer)
            (when (or (eq 'dired-mode
                          (buffer-local-value 'major-mode buffer))
                      (string-match-p "^\\*image-dired"
                                      (buffer-name buffer)))
              (kill-buffer buffer)))
          (buffer-list)))
  :custom
  (dired-omit-files (rx (seq bol ".")))
  (dired-listing-switches
   "-goah --group-directories-first --time-style=long-iso -v")
  (dired-kill-when-opening-new-dired-buffer nil)
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-create-destination-dirs 'ask)
  (dired-clean-confirm-killing-deleted-buffers nil))
