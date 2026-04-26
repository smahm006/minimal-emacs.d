;;; navigation.el --- Navigation configuration  -*- no-byte-compile: t; lexical-binding: t; -*-

;; Jump to previous locations stored in the mark ring
(setq set-mark-command-repeat-pop t)

;; Window Management
(use-package ace-window
  :autoload ace-display-buffer
  :init
  (winner-mode)
  :bind
  (("M-o" . ace-window)
   ("M-O" . me/ace-window-prefix)
   ("M-u" . me/toggle-fullscreen-window)
   ([remap split-window-right] . me/hsplit-last-window)
   ([remap split-window-below] . me/vsplit-last-window)
   (:map me/window-map
         ("b" . balance-windows)
         ("c" . recenter-top-bottom)
         ("l" . enlarge-window-horizontally)
         ("i" . enlarge-window)
         ("j" . shrink-window-horizontally)
         ("k" . shrink-window)
         ("u" . winner-undo)
         ("r" . winner-redo)
         ("s" . switch-window-then-swap-buffer)
         ("-" . text-scale-decrease)
         ("+" . text-scale-increase)
         ("=" . (lambda () (interactive) (text-scale-increase 0)))))
  :preface
  (defun me/hsplit-last-window ()
    "Focus to the last created horizontal window."
    (interactive)
    (split-window-horizontally)
    (other-window 1))
  (defun me/vsplit-last-window ()
    "Focus to the last created vertical window."
    (interactive)
    (split-window-vertically)
    (other-window 1))
  (defun me/toggle-fullscreen-window ()
    "Toggle a buffer as fullscreen"
    (interactive)
    (if (= 1 (length (window-list)))
        (jump-to-register '_)
      (progn
        (window-configuration-to-register '_)
        (delete-other-windows))))
  (defun me/ace-window-prefix ()
    "https://karthinks.com/software/emacs-window-management-almanac/#a-window-prefix-command-for-ace-window"
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
         (setq
          window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
          type 'reuse)
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))
  :custom
  ;; Switch to minibuffer as well
  (aw-minibuffer-flag t)
  ;; Make Emacs ask where to place a new buffer
  (display-buffer-base-action
   '((display-buffer-reuse-window
      display-buffer-in-previous-window
      ace-display-buffer)))
  :custom-face
  (aw-leading-char-face ((t (:foreground "red" :weight bold :height 2.0))))
  :config
  (setq window-combination-resize t))

;; Jump to visible text using a char-based decision tree
(use-package avy
  :bind
  ("C-o" . avy-goto-char-timer)
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
