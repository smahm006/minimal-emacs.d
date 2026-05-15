;;; development.el --- Development customization  -*- no-byte-compile: t; lexical-binding: t; -*-

;; A client for Language Server Protocol servers
(use-package eglot
  :ensure nil
  :after project
  :hook
  (eglot-managed-mode . me/eglot-capf)
  (eglot-managed-mode . (lambda ()
                          (setq eldoc-documentation-strategy
                                'eldoc-documentation-compose-eagerly)))
  :bind
  (:map me/lsp-map
        ("l" . eglot)
        ("f" . eglot-format-buffer)
        ("R" . eglot-reconnect)
        ("f" . eglot-find-declaration)
        ("i" . eglot-find-implementation)
        ("k" . eglot-shutdown)
        ("o" . eglot-code-action-organize-imports)
        ("q" . eglot-code-action-quickfix)
        ("r" . eglot-rename))
  :preface
  (defun me/eglot-capf ()
    (setq-local completion-at-point-functions
                (cons (cape-capf-super
                       #'eglot-completion-at-point
                       #'tempel-complete)
                      completion-at-point-functions)))
  :custom
  ;; Filter list of all possible completions with Orderless
  ;; https://github.com/minad/corfu/wiki#configuring-corfu-for-eglot
  (completion-category-defaults nil)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 3)
  :config
  ;; Continuously update the candidates using cape cache buster
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; Don't log every event
  (fset #'jsonrpc--log-event #'ignore))

;; Boost eglot using lsp-booster.
(use-package eglot-booster
  :after eglot
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :branch "main")
  :init (eglot-booster-mode))

;; Emacs documentation support.
(use-package eldoc
  :ensure nil
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :bind
  (:map me/lsp-map
        ("d" . eldoc-doc-buffer))
  :config
  (eldoc-add-command-completions "paredit-"))

(use-package eldoc-box
  :after eglot
  :bind
  (:map me/lsp-map
        ("D" . eldoc-box-hover-at-point-mode))
  :config
  (with-eval-after-load 'pixel-scroll
    (add-to-list 'eldoc-box-self-insert-command-list #'pixel-scroll-precision)
    (add-to-list 'eldoc-box-self-insert-command-list #'pixel-scroll-start-momentum)))

;; Syntax checker
(use-package flymake
  :ensure nil
  :after project
  :custom
  (flymake-show-diagnostics-at-end-of-line 'fancy)
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-start-on-flymake-mode t)
  (flymake-no-changes-timeout nil)
  (flymake-start-on-save-buffer t)
  (flymake-no-changes-timeout 0.1)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-wrap-around nil)
  (flymake-mode-line-format
   '("" flymake-mode-line-exception flymake-mode-line-counters))
  (flymake-mode-line-counter-format
   '(" " flymake-mode-line-error-counter
     flymake-mode-line-warning-counter
     flymake-mode-line-note-counter "")))
