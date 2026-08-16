;;; lsp.el --- LSP, diagnostics and formatting -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Eglot — built-in LSP client
(use-package eglot
  :ensure nil
  :after project
  :hook
  (eglot-managed-mode . me/eglot-capf)
  (eglot-managed-mode . me/eglot-configure-eldoc)
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
    "Merge eglot and tempel completion."
    (setq-local completion-at-point-functions
                (cons (cape-capf-super
                       #'eglot-completion-at-point
                       #'tempel-complete)
                      completion-at-point-functions)))

  (defun me/eglot-configure-eldoc ()
    "Use combined Eldoc docs in managed buffers."
    (setq-local eldoc-documentation-strategy
                'eldoc-documentation-compose-eagerly))
  :custom
  (completion-category-defaults nil)  ; let orderless handle eglot candidates
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.5)
  (eglot-events-buffer-config '(:size 2000000 :format full))
  :config
  ;; Continuously update candidates using cape cache buster
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; Keep the standard JSON-RPC event logger available for diagnosing failed
  ;; servers.  The event buffer is bounded by `eglot-events-buffer-config'.
  (setq jsonrpc-event-hook '(jsonrpc--log-event)))

;;; Eldoc — documentation in the echo area
(use-package eldoc
  :ensure nil
  :bind
  (:map me/lsp-map
        ("d" . eldoc-doc-buffer))
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

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
  (flymake-no-changes-timeout 0.5)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-wrap-around nil)
  (flymake-mode-line-format
   '("" flymake-mode-line-exception flymake-mode-line-counters))
  (flymake-mode-line-counter-format
   '(" " flymake-mode-line-error-counter
     flymake-mode-line-warning-counter
     flymake-mode-line-note-counter "")))

;;; Apheleia — manual asynchronous formatting
(use-package apheleia
  :bind
  (:map me/run-map
        ("f" . me/format-buffer))
  :preface
  (defun me/apheleia-formatter-executable (formatter)
    "Return FORMATTER's executable, or nil for Emacs Lisp."
    (let ((definition (alist-get formatter apheleia-formatters)))
      (when (and (listp definition)
                 (stringp (car definition)))
        (car definition))))

  (defun me/format-buffer ()
    "Format the buffer with Apheleia."
    (interactive)
    (require 'apheleia-formatters)
    (let* ((formatters (apheleia--get-formatters))
           (missing
            (seq-filter
             (lambda (formatter)
               (when-let* ((executable
                            (me/apheleia-formatter-executable formatter)))
                 (not (executable-find executable))))
             formatters))
           (label (mapconcat #'symbol-name formatters " -> ")))
      (unless formatters
        (user-error "No formatter configured for %s" major-mode))
      (when missing
        (user-error "Formatter %s unavailable: %s"
                    label
                    (mapconcat
                     (lambda (formatter)
                       (me/apheleia-formatter-executable formatter))
                     missing ", ")))
      (message "Formatting %s with %s" (buffer-name) label)
      (apheleia-format-buffer
       formatters nil
       :callback
       (lambda (&key error)
         (if error
             (message "Formatting with %s failed: %s" label error)
           (message "Formatted %s with %s" (buffer-name) label))))))
  :config
  ;; Custom formatter definitions.
  (setf (alist-get 'ruff-fix apheleia-formatters)
        '("ruff" "check" "--fix" "--stdin-filename" filepath "-"))
  (setf (alist-get 'yamlfmt apheleia-formatters)
        '("yamlfmt"))
  (setf (alist-get 'zig-fmt apheleia-formatters)
        '("zig" "fmt" "--stdin"))
  (setf (alist-get 'google-java-format apheleia-formatters)
        '("google-java-format" "--aosp" "-"))
  ;; One reviewed mode-to-formatter mapping.  Entries override Apheleia's
  ;; defaults only where this configuration has an explicit workflow.
  (dolist (entry '((python-ts-mode  ruff-isort ruff)
                   (go-ts-mode      goimports gofumpt)
                   (bash-ts-mode    shfmt)
                   (sh-mode         shfmt)
                   (java-ts-mode    google-java-format)
                   (rust-ts-mode    rustfmt)
                   (c-ts-mode       clang-format)
                   (c++-ts-mode     clang-format)
                   (zig-ts-mode     zig-fmt)
                   (js-ts-mode      prettier)
                   (jsx-ts-mode     prettier)
                   (typescript-ts-mode prettier)
                   (tsx-ts-mode     prettier)
                   (svelte-ts-mode  prettier)
                   (mhtml-mode      prettier)
                   (css-ts-mode     prettier)
                   (yaml-ts-mode    yamlfmt)
                   (json-ts-mode    jq)))
    (setf (alist-get (car entry) apheleia-mode-alist)
          (cdr entry))))
