;;; web.el --- HTML and CSS configuration with shared web functions -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Shared web functions
;; Defined here so javascript.el, typescript.el and svelte.el can reference them.
(defun me/web-run ()
  "Run the dev server for the current project."
  (interactive)
  (let ((default-directory
         (or (locate-dominating-file default-directory "package.json")
             default-directory)))
    (compile "pnpm run dev")))

(defun me/web-format ()
  "Format the current buffer using apheleia (prettier)."
  (interactive)
  (apheleia-format-buffer '(prettier)))

(defun me/web-check ()
  "Check the current buffer with eslint."
  (interactive)
  (compile (format "npx eslint %s"
                   (shell-quote-argument buffer-file-name))))

;;; HTML — mhtml-mode for HTML files
(use-package mhtml-mode
  :ensure nil
  :mode
  ("\\.html?\\'" . mhtml-mode)
  :hook
  (mhtml-mode . eglot-ensure)
  (mhtml-mode . emmet-mode)
  (mhtml-mode . add-node-modules-path)
  (mhtml-mode . (lambda ()
                  (define-key me/run-map (kbd "r") #'me/web-run)
                  (define-key me/run-map (kbd "c") #'me/web-check)
                  (define-key me/run-map (kbd "f") #'me/web-format)))
  :custom
  (sgml-basic-offset 2)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(mhtml-mode . ("vscode-html-language-server" "--stdio")))))

;;; emmet-mode — expand CSS-style abbreviations in HTML
(use-package emmet-mode
  :custom
  (emmet-indent-after-insert nil)
  (emmet-indentation 2))

;;; CSS — css-ts-mode for stylesheet files
(use-package css-mode
  :ensure nil
  :mode
  ("\\.css\\'"  . css-ts-mode)
  ("\\.scss\\'" . css-ts-mode)
  :hook
  (css-ts-mode . eglot-ensure)
  (css-ts-mode . add-node-modules-path)
  (css-ts-mode . (lambda ()
                   (define-key me/run-map (kbd "f") #'me/web-format)))
  :custom
  (css-indent-offset 2)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(css-ts-mode . ("vscode-css-language-server" "--stdio")))))

;;; add-node-modules-path — add node_modules/.bin to PATH in all web buffers
;; Uses both local and workspace-root bin dirs for monorepo support.
(use-package add-node-modules-path
  :custom
  (add-node-modules-path-command '("pnpm bin" "pnpm bin -w"))
  :hook
  (js-ts-mode        . add-node-modules-path)
  (jsx-ts-mode       . add-node-modules-path)
  (typescript-ts-mode . add-node-modules-path)
  (tsx-ts-mode       . add-node-modules-path)
  (svelte-ts-mode    . add-node-modules-path)
  (mhtml-mode        . add-node-modules-path)
  (css-ts-mode       . add-node-modules-path))


;;; pnpm-mode — run pnpm scripts from Emacs across all web modes
;; Declared here once rather than per-language file.
(use-package pnpm-mode
  :hook
  (js-ts-mode         . pnpm-mode)
  (jsx-ts-mode        . pnpm-mode)
  (typescript-ts-mode . pnpm-mode)
  (tsx-ts-mode        . pnpm-mode)
  (svelte-ts-mode     . pnpm-mode)
  (mhtml-mode         . pnpm-mode)
  (css-ts-mode        . pnpm-mode))
