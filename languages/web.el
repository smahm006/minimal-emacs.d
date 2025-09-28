;;; web.el --- Web / Svelte development config  -*- no-byte-compile: t; lexical-binding: t; -*-

;; -------------------------
;; Shared run/check/format functions
;; -------------------------
(defun me/web-run ()
  "Run dev server or build for project."
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory "package.json")))
    (compile "npm run dev")))

(defun me/web-format ()
  "Format current buffer with prettier."
  (interactive)
  (let* ((file (shell-quote-argument buffer-file-name))
         (output (shell-command-to-string (format "npx prettier --write %s" file))))
    (message "%s" (string-trim output)))
  (me/revert-buffer-no-confirm))

(defun me/web-check ()
  "Check current buffer file with eslint."
  (interactive)
  (let ((file (shell-quote-argument buffer-file-name)))
    (compile (format "npx eslint %s" file))))

;; -------------------------
;; MHTML Mode (HTML/CSS/JS)
;; -------------------------
(use-package mhtml-mode
  :ensure nil
  :mode (("\\.html?\\'" . mhtml-mode)
         ("\\.css\\'" . mhtml-mode)
         ("\\.js\\'" . mhtml-mode)
         ("\\.mjs\\'" . mhtml-mode)
         ("\\.jsx\\'" . mhtml-mode))
  :hook
  (mhtml-mode . (lambda ()
                  (define-key me/run-map (kbd "r") #'me/web-run)
                  (define-key me/run-map (kbd "c") #'me/web-check)
                  (define-key me/run-map (kbd "f") #'me/web-format)))
  :custom
  (sgml-basic-offset 2)
  (css-indent-offset 2)
  (js-indent-level 2))

;; Enable emmet-mode for HTML/CSS/JS
(use-package emmet-mode
  :ensure t
  :hook (mhtml-mode . emmet-mode)
  :custom
  (emmet-indent-after-insert nil)
  (emmet-indentation 2))

;; -------------------------
;; TypeScript Mode
;; -------------------------
(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :hook
  ((typescript-mode . eglot-ensure)
   (typescript-mode . (lambda ()
                        (define-key me/run-map (kbd "r") #'me/web-run)
                        (define-key me/run-map (kbd "c") #'me/web-check)
                        (define-key me/run-map (kbd "f") #'me/web-format))))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(typescript-mode . ("typescript-language-server" "--stdio"))))
  :custom
  (typescript-indent-level 2))

;; -------------------------
;; Svelte Mode
;; -------------------------
(use-package svelte-mode
  :ensure t
  :mode "\\.svelte\\'"
  :hook
  ((svelte-mode . eglot-ensure)
   (svelte-mode . (lambda ()
                    (define-key me/run-map (kbd "r") #'me/web-run)
                    (define-key me/run-map (kbd "c") #'me/web-check)
                    (define-key me/run-map (kbd "f") #'me/web-format))))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(svelte-mode . ("svelteserver" "--stdio")))))
