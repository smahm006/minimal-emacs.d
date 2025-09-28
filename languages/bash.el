;;; bash.el --- Bash customization  -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package bash
  :ensure nil
  :mode (("\\.sh\\'"  . bash-ts-mode))
  :interpreter ("bash" . bash-ts-mode)
  :hook
  (bash-ts-mode . eglot-ensure)
  :hook
  (bash-ts-mode . (lambda ()
                    (define-key me/run-map (kbd "r") #'me/bash-run)
                    (define-key me/run-map (kbd "c") #'me/bash-check)
                    (define-key me/run-map (kbd "f") #'me/bash-format)))
  :preface
  (defun me/bash-run ()
    "Compile current buffer file with sh."
    (interactive)
    (compile (format "bash %s" buffer-file-name)))
  (defun me/bash-format ()
    "Format current buffer with shfmt."
    (interactive)
    (let ((output (shell-command-to-string
                   (format "shfmt -w %s" (shell-quote-argument buffer-file-name)))))
      (message "%s" (string-trim output)))
    (me/revert-buffer-no-confirm))
  (defun me/bash-check ()
    "Compile current buffer file with sh."
    (interactive)
    (compile (format "shellcheck %s" buffer-file-name)))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(bash-ts-mode . ("bash-language-server" "start")))))
