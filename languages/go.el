;;; go.el --- Go language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

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
    "Run the current Go file or package.
If a go.mod is found, runs the whole package with `go run ./...'.
Otherwise falls back to running the current file directly."
    (interactive)
    (let* ((root (locate-dominating-file buffer-file-name "go.mod"))
           (cmd (if root
                    (let ((default-directory root))
                      "go run ./...")
                  (format "go run %s"
                          (shell-quote-argument buffer-file-name)))))
      (compile cmd)))
  (defun me/go-format ()
    "Format the current buffer using apheleia (goimports + gofumpt)."
    (interactive)
    (apheleia-format-buffer '(goimports gofumpt)))
  (defun me/go-check ()
    "Check the current package with go vet."
    (interactive)
    (let* ((root (locate-dominating-file buffer-file-name "go.mod"))
           (default-directory (or root default-directory)))
      (compile "go vet ./...")))
  :custom
  (go-ts-mode-indent-offset 4)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(go-ts-mode . ("gopls")))))

;;; gotest — run Go tests from Emacs and navigate to failures
(use-package gotest
  :after go
  :hook
  (go-ts-mode . (lambda ()
                  (define-key me/run-map (kbd "t t") #'go-test-current-file)
                  (define-key me/run-map (kbd "t f") #'me/go-test-current-test)
                  (define-key me/run-map (kbd "t a") #'me/go-test-current-projectt)
                  (define-key me/run-map (kbd "t r") #'go-test-current-benchmark))))
