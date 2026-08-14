;;; javascript.el --- JavaScript language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package javascript
  :ensure nil
  :mode
  ("\\.js\\'"  . js-ts-mode)
  ("\\.mjs\\'" . js-ts-mode)
  ("\\.cjs\\'" . js-ts-mode)
  ("\\.jsx\\'" . jsx-ts-mode)
  :bind (:map me/web-run-map ("r" . me/web-run) ("c" . me/web-check))
  :custom
  (js-indent-level 2)
  :config
  (me/enable-run-map js-ts-mode-map me/web-run-map)
  (me/enable-run-map jsx-ts-mode-map me/web-run-map)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((js-ts-mode jsx-ts-mode) .
                   ("typescript-language-server" "--stdio")))))
