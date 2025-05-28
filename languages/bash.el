;;; bash.el --- Bash customization  -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package bash
  :ensure nil
  :mode (("\\.sh\\'"  . bash-ts-mode))
  :interpreter ("bash" . bash-ts-mode)
  :hook
  (bash-ts-mode . eglot-ensure)
  :hook
  (bash-ts-mode . (lambda ()
                    (define-key minimal-emacs/run-map (kbd "r") #'minimal-emacs/bash-run)
                    (define-key minimal-emacs/run-map (kbd "c") #'minimal-emacs/bash-check)
                    (define-key minimal-emacs/run-map (kbd "f") #'minimal-emacs/bash-format)))
  :preface
  (defun minimal-emacs/bash-run ()
    "Compile current buffer file with sh."
    (interactive)
    (compile (format "bash %s" buffer-file-name)))
  (defun minimal-emacs/bash-format ()
    "Format current buffer with shfmt."
    (interactive)
    (let ((output (shell-command-to-string
                   (format "shfmt -w %s" (shell-quote-argument buffer-file-name)))))
      (message "%s" (string-trim output)))
    (minimal-emacs/revert-buffer-no-confirm))
  (defun minimal-emacs/bash-check ()
    "Compile current buffer file with sh."
    (interactive)
    (compile (format "shellcheck %s" buffer-file-name)))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(bash-ts-mode . ("bash-language-server" "start")))))
