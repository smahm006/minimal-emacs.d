;;; web.el --- HTML and CSS configuration with shared web functions -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Shared web functions
;; Defined here so javascript.el, typescript.el and svelte.el can reference them.
(defun me/web-run ()
  "Run the dev server for the current project."
  (interactive)
  (let ((root (me/project-root-or-error '("package.json"))))
    (let ((default-directory root)) (compile "pnpm run dev"))))

(defun me/web-project-tool (tool)
  "Return the local TOOL, or a global one."
  (let* ((root (me/project-root-or-error '("package.json")))
         (local (expand-file-name (concat "node_modules/.bin/" tool) root)))
    (if (file-executable-p local) local tool)))

(defun me/web-check ()
  "Check the current buffer with eslint."
  (interactive)
  (let ((file (me/buffer-file-or-error))
        (root (me/project-root-or-error '("package.json"))))
    (let ((default-directory root))
      (compile (format "%s %s" (me/web-project-tool "eslint")
                       (shell-quote-argument file))))))

;;; HTML — mhtml-mode for HTML files
(use-package mhtml-mode
  :ensure nil
  :mode
  ("\\.html?\\'" . mhtml-mode)
  :hook
  (mhtml-mode . emmet-mode)

  :bind (:map me/web-run-map ("r" . me/web-run) ("c" . me/web-check))
  :custom
  (sgml-basic-offset 2)
  :config
  (me/enable-run-map mhtml-mode-map me/web-run-map)
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
  :custom
  (css-indent-offset 2)
  :config
  (me/enable-run-map css-ts-mode-map me/web-run-map)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(css-ts-mode . ("vscode-css-language-server" "--stdio")))))
