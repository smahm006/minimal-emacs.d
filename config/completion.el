;;; completion.el --- In-buffer completion -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Corfu — in-buffer completion popup
(use-package corfu
  :hook
  (elpaca-after-init . global-corfu-mode)
  (elpaca-after-init . corfu-popupinfo-mode)
  (elpaca-after-init . corfu-history-mode)
  ;; Disable auto-completion in the terminal where it is intrusive
  (eat-mode . (lambda ()
                (setq-local corfu-auto nil)
                (corfu-mode)))
  :bind
  (:map corfu-map
        ("C-M-n" . corfu-next)
        ("C-M-p" . corfu-previous)
        ("C-M-i" . corfu-insert)
        ("C-M-b" . corfu-first)
        ("C-M-f" . corfu-last)
        ("SPC"   . corfu-insert-separator)
        ("<escape>" . corfu-quit))
  :custom
  (tab-always-indent t)           ; do not use TAB for completion
  (corfu-cycle t)                 ; wrap around candidate list
  (corfu-auto t)                  ; enable auto completion
  (corfu-quit-no-match t)         ; quit if no match
  (corfu-preview-current t)       ; preview current candidate
  (corfu-auto-prefix 2)           ; complete after 2 characters
  :config
  (keymap-unset corfu-map "RET")
  (define-key corfu-map [remap previous-line] nil)
  (define-key corfu-map [remap next-line] nil)
  (define-key corfu-map [remap beginning-of-buffer] nil)
  (define-key corfu-map [remap end-of-buffer] nil))

;;; Cape — completion-at-point extensions
;; Note: file is added last so it appears first in the list (add-to-list prepends).
;; Scoped backends (eglot, tempel) are added per-mode in lsp.el and completion.el.
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-file))

;;; Tempel — lightweight template/snippet system
(use-package tempel
  :hook
  (conf-mode . me/tempel-setup-capf)
  (prog-mode . me/tempel-setup-capf)
  (text-mode . me/tempel-setup-capf)
  :bind
  (("M-+" . tempel-expand)
   ("M-*" . tempel-insert)
   :map tempel-map
   ("C-M-n" . tempel-next)
   ("C-M-p" . tempel-previous))
  :preface
  (defun me/tempel-setup-capf ()
    "Prepend tempel-complete to completion-at-point-functions."
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))
  :custom
  (tempel-path (format "%s/snippets/*.eld" minimal-emacs-user-directory)))

;;; Hippie-expand — fallback text expansion
(use-package hippie-expand
  :ensure nil
  :bind ([remap dabbrev-expand] . hippie-expand)
  :custom
  (hippie-expand-verbose t)
  (hippie-expand-dabbrev-skip-space t)
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol
     try-expand-list
     try-expand-line
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs)))
