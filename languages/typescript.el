;;; typescript.el --- TypeScript language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package typescript-ts-mode
  :ensure nil
  :mode
  ("\\.ts\\'"  . typescript-ts-mode)
  ("\\.tsx\\'" . tsx-ts-mode)
  :hook
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode        . eglot-ensure)
  (typescript-ts-mode . (lambda ()
                          (define-key me/run-map (kbd "r") #'me/web-run)
                          (define-key me/run-map (kbd "c") #'me/web-check)
                          (define-key me/run-map (kbd "f") #'me/web-format)))
  (tsx-ts-mode        . (lambda ()
                          (define-key me/run-map (kbd "r") #'me/web-run)
                          (define-key me/run-map (kbd "c") #'me/web-check)
                          (define-key me/run-map (kbd "f") #'me/web-format)))
  :custom
  (typescript-ts-mode-indent-offset 2)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((typescript-ts-mode tsx-ts-mode) .
                   ("typescript-language-server" "--stdio")))))
