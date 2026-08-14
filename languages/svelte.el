;;; svelte.el --- Svelte language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package svelte-ts-mode
  :ensure (:host github :repo "leafOfTree/svelte-ts-mode")
  :mode ("\\.svelte\\'" . svelte-ts-mode)
  :bind (:map me/web-run-map ("r" . me/web-run) ("c" . me/web-check))
  :config
  (me/enable-run-map svelte-ts-mode-map me/web-run-map)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(svelte-ts-mode . ("svelteserver" "--stdio")))))
