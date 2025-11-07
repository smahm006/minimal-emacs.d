;;; navigation.el --- Navigation configuration  -*- no-byte-compile: t; lexical-binding: t; -*-

;; Jump to previous locations stored in the mark ring
(setq set-mark-command-repeat-pop t)

;; Jump to visible text using a char-based decision tree
(use-package avy
  :bind
  ("C-." . avy-goto-char-timer)
  :preface
  (defun avy-action-embark (pt)
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
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(defun me/move-to-middle ()
  (interactive)
  (let* ((begin (line-beginning-position))
         (end (line-end-position))
         (middle (/ (+ end begin) 2)))
    (goto-char middle)))


;; Bind custom functions
(global-set-key (kbd "C-z") #'me/move-to-middle)
(global-set-key (kbd "M-z") #'move-to-window-line-top-bottom)
