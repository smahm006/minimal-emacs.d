;;; lsp.el --- LSP, diagnostics and formatting -*- no-byte-compile: t; lexical-binding: t; -*-

;;; treesit-auto — auto-install tree-sitter grammars and remap modes
;; Must load before eglot and language files so grammars are available
;; when major modes and LSP servers start up.
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs
        '(bash
          c
          cpp
          go
          java
          json
          python
          zig
          rust
          toml
          typescript
          tsx
          javascript
          css
          yaml
          dockerfile))
  (treesit-auto-add-to-auto-mode-alist treesit-auto-langs)
  (global-treesit-auto-mode))

;;; Eglot — built-in LSP client
(use-package eglot
  :ensure nil
  :after project
  :hook
  (eglot-managed-mode . me/eglot-capf)
  (eglot-managed-mode . (lambda ()
                          (setq-local eldoc-documentation-strategy
                                      'eldoc-documentation-compose-eagerly)))
  :bind
  (:map me/lsp-map
        ("l" . eglot)
        ("R" . eglot-reconnect)
        ("k" . eglot-shutdown)
        ("f" . eglot-format-buffer)
        ("i" . eglot-find-implementation)
        ("o" . eglot-code-action-organize-imports)
        ("q" . eglot-code-action-quickfix)
        ("r" . eglot-rename))
  :preface
  (defun me/eglot-capf ()
    "Merge eglot and tempel completions into a single capf."
    (setq-local completion-at-point-functions
                (cons (cape-capf-super
                       #'eglot-completion-at-point
                       #'tempel-complete)
                      completion-at-point-functions)))
  :custom
  (completion-category-defaults nil)  ; let orderless handle eglot candidates
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 3)
  :config
  ;; Continuously update candidates using cape cache buster
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; Suppress verbose jsonrpc event logging
  (fset #'jsonrpc--log-event #'ignore))

;;; eglot-booster — speed up eglot using emacs-lsp-booster
(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode 1))

;;; Eldoc — documentation in the echo area
(use-package eldoc
  :ensure nil
  :bind
  (:map me/lsp-map
        ("d" . eldoc-doc-buffer))
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

;;; Eldoc-box — show eldoc in a floating child frame
(use-package eldoc-box
  :after eglot
  :bind
  (:map me/lsp-map
        ("D" . eldoc-box-hover-at-point-mode))
  :config
  (with-eval-after-load 'pixel-scroll
    (add-to-list 'eldoc-box-self-insert-command-list #'pixel-scroll-precision)
    (add-to-list 'eldoc-box-self-insert-command-list #'pixel-scroll-start-momentum)))

;;; Flymake — on-the-fly syntax checking
(use-package flymake
  :ensure nil
  :after project
  :bind
  (:map me/goto-map
        ("n" . flymake-goto-next-error)
        ("p" . flymake-goto-prev-error))
  :custom
  (flymake-show-diagnostics-at-end-of-line 'fancy)
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-start-on-flymake-mode t)
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

;;; Apheleia — async code formatting on save
;; Runs formatters asynchronously, preserves cursor position, and only
;; applies changes if the buffer is unmodified after formatting.
(use-package apheleia
  :bind
  (:map me/lsp-map
        ("F" . apheleia-format-buffer))
  :config
  ;; Ensure apheleia uses the correct formatters per mode.
  ;; Languages not listed here use apheleia's built-in defaults.
  ;; Extra Formatters
  (setf (alist-get 'ruff-fix apheleia-formatters)
        '("ruff" "check" "--fix" "--stdin-filename" filepath "-"))
  (setf (alist-get 'yamlfmt apheleia-formatters)
        '("yamlfmt" "-in"))
  (setf (alist-get 'zig-fmt apheleia-formatters)
        '("zig" "fmt" "--stdin"))
  ;; Relevant Modes
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist)
        '(goimports gofumpt))
  (setf (alist-get 'bash-ts-mode apheleia-mode-alist)
        '(shfmt))
  (setf (alist-get 'sh-mode apheleia-mode-alist)
        '(shfmt))
  (setf (alist-get 'google-java-format apheleia-formatters)
        '("google-java-format" "--aosp" "-"))
  (setf (alist-get 'java-ts-mode apheleia-mode-alist)
        '(google-java-format))
  (setf (alist-get 'rust-ts-mode apheleia-mode-alist)
        '(rustfmt))
  (setf (alist-get 'c-ts-mode apheleia-mode-alist)
        '(clang-format))
  (setf (alist-get 'c++-ts-mode apheleia-mode-alist)
        '(clang-format))
  (setf (alist-get 'zig-ts-mode apheleia-mode-alist)
        '(zig-fmt))
  (setf (alist-get 'js-ts-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'jsx-ts-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'svelte-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'mhtml-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'css-ts-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'yaml-ts-mode apheleia-mode-alist)
        '(yamlfmt)))


(use-package treesit-fold
  :commands (treesit-fold-close
             treesit-fold-close-all
             treesit-fold-open
             treesit-fold-toggle
             treesit-fold-open-all
             treesit-fold-mode
             global-treesit-fold-mode
             treesit-fold-open-recursively
             treesit-fold-line-comment-mode)
  :custom
  (treesit-fold-line-count-show t)
  (setq treesit-fold-line-count-format " <%d lines> ")
  :hook
  (prog-mode . treesit-fold-mode)
  (conf-mode . treesit-fold-mode)
  (text-mode . treesit-fold-mode)
  :bind
  (:map me/treesit-map
        ("t" . treesit-fold-toggle)
        ("o" . treesit-fold-open)
        ("O" . treesit-fold-open-all)
        ("c" . treesit-fold-close)
        ("C" . treesit-fold-close-all)
        ("r" . treesit-fold-open-recursively))
  :config
  (set-face-attribute 'treesit-fold-replacement-face nil
                      :foreground "#808080"
                      :box nil
                      :weight 'bold))
