;;; javascript.el --- JavaScript language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package javascript
  :ensure nil
  :mode
  ("\\.js\\'"  . js-ts-mode)
  ("\\.mjs\\'" . js-ts-mode)
  ("\\.cjs\\'" . js-ts-mode)
  ("\\.jsx\\'" . jsx-ts-mode)
  :hook
  (js-ts-mode  . eglot-ensure)
  (jsx-ts-mode . eglot-ensure)
  (js-ts-mode  . (lambda ()
                   (define-key me/run-map (kbd "r") #'me/web-run)
                   (define-key me/run-map (kbd "c") #'me/web-check)
                   (define-key me/run-map (kbd "f") #'me/web-format)))
  (jsx-ts-mode . (lambda ()
                   (define-key me/run-map (kbd "r") #'me/web-run)
                   (define-key me/run-map (kbd "c") #'me/web-check)
                   (define-key me/run-map (kbd "f") #'me/web-format)))
  :custom
  (js-indent-level 2)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((js-ts-mode jsx-ts-mode) .
                   ("typescript-language-server" "--stdio")))))
