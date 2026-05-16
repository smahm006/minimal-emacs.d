;;; svelte.el --- Svelte language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package svelte-ts-mode
  :ensure (:host github :repo "leafOfTree/svelte-ts-mode")
  :mode ("\\.svelte\\'" . svelte-ts-mode)
  :hook
  (svelte-ts-mode . eglot-ensure)
  (svelte-ts-mode . add-node-modules-path)
  (svelte-ts-mode . pnpm-mode)
  (svelte-ts-mode . (lambda ()
                      (define-key me/run-map (kbd "r") #'me/web-run)
                      (define-key me/run-map (kbd "c") #'me/web-check)
                      (define-key me/run-map (kbd "f") #'me/web-format)))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(svelte-ts-mode . ("svelteserver" "--stdio")))))
