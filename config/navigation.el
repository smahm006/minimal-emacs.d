;;; navigation.el --- Navigation and jumping -*- no-byte-compile: t; lexical-binding: t; -*-

;;; avy — jump to visible text via char-based decision tree
(use-package avy
  :bind
  ("C-o" . avy-goto-char-timer)
  :preface
  (defun me/avy-action-embark (pt)
    "Run embark-act on the avy candidate at PT."
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  :custom
  (avy-timeout-seconds 0.3)
  (avy-single-candidate-jump nil)
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'me/avy-action-embark))

;;; dumb-jump — xref backend for jumping to definitions without an LSP
;; Acts as a fallback when eglot is not active (shell scripts, config files, etc).
(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read))
