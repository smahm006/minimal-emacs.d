;;; go.el --- Go language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package go
  :ensure nil
  :mode ("\\.go\\'" . go-ts-mode)
  :bind (:map me/go-run-map
              ("r" . me/go-run) ("c" . me/go-check) ("t" . me/go-test))
  :hook
  (go-ts-mode . (lambda ()
                  (setq-local eglot-workspace-configuration
                              '((:gopls .
                                        ((staticcheck . t)
                                         (matcher . "CaseSensitive")))))))
  :preface
  (defun me/go-run ()
    "Run the current Go package or file."
    (interactive)
    (let* ((root (locate-dominating-file buffer-file-name "go.mod"))
           (cmd (if root
                    (let ((default-directory root))
                      "go run ./...")
                  (format "go run %s"
                          (shell-quote-argument buffer-file-name)))))
      (compile cmd)))
  (defun me/go-check ()
    "Check the current package with go vet."
    (interactive)
    (let* ((root (locate-dominating-file buffer-file-name "go.mod"))
           (default-directory (or root default-directory)))
      (compile "go vet ./...")))
  (defun me/go-test ()
    "Run all Go tests in the current module."
    (interactive)
    (let ((root (me/project-root-or-error '("go.mod"))))
      (let ((default-directory root)) (compile "go test ./..."))))
  :custom
  (go-ts-mode-indent-offset 4)
  :config
  (me/enable-run-map go-ts-mode-map me/go-run-map)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(go-ts-mode . ("gopls")))))
