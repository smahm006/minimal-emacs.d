;;; typescript.el --- TypeScript language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package typescript-ts-mode
  :ensure nil
  :mode
  ("\\.ts\\'"  . typescript-ts-mode)
  ("\\.tsx\\'" . tsx-ts-mode)
  :bind (:map me/web-run-map ("r" . me/web-run) ("c" . me/web-check))
  :custom
  (typescript-ts-mode-indent-offset 2)
  :config
  (me/enable-run-map typescript-ts-mode-map me/web-run-map)
  (me/enable-run-map tsx-ts-mode-map me/web-run-map)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((typescript-ts-mode tsx-ts-mode) .
                   ("typescript-language-server" "--stdio")))))
