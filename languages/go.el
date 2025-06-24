;;; go.el --- Golang customization  -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package go
  :ensure nil
  :mode ("\\.go\\'" . go-ts-mode)
  :hook
  (go-ts-mode . eglot-ensure)
  (go-ts-mode . (lambda ()
                  (define-key me/run-map (kbd "r") #'me/go-run)
                  (define-key me/run-map (kbd "c") #'me/go-check)
                  (define-key me/run-map (kbd "f") #'me/go-format)))
  (go-ts-mode . (lambda ()
                  (setq-local eglot-workspace-configuration
							  '((:gopls .
										((staticcheck . t)
										 (matcher . "CaseSensitive")))))))
  :preface
  (defun me/go-run ()
    "Compile current buffer file with go."
    (interactive)
    (compile (format "go run %s" (shell-quote-argument buffer-file-name))))
  (defun me/go-format ()
    "Format current buffer file with goimports."
    (interactive)
    (let ((output (shell-command-to-string (format "goimports -w %s && gofumpt -w %s" (shell-quote-argument buffer-file-name) (shell-quote-argument buffer-file-name)))))
      (message "%s" (string-trim output)))
    (me/revert-buffer-no-confirm))
  (defun me/go-check ()
    "Check current buffer file with goimports."
    (interactive)
    (compile (format "gofumpt -e %s" (shell-quote-argument buffer-file-name))))
  :custom
  (go-ts-mode-indent-offset 4))

(use-package go-eldoc
  :hook
  (go-ts-mode . go-eldoc-setup))
