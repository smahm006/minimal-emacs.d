;;; windows.el --- Window management -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Pixel-accurate scrolling
(setq pixel-scroll-precision-interpolate-mice nil) ; disable interpolation (causes jumps)
(setq pixel-scroll-precision-use-momentum t)       ; momentum for large buffers
(when (display-graphic-p)
  (pixel-scroll-precision-mode 1))

;;; ace-window — fast window switching and management
(use-package ace-window
  :autoload ace-display-buffer
  :init
  (winner-mode 1)
  :bind
  (("M-o"                    . ace-window)
   ("M-O"                    . me/ace-window-prefix)
   ("M-u"                    . me/toggle-fullscreen-window)
   ([remap split-window-right] . me/hsplit-last-window)
   ([remap split-window-below] . me/vsplit-last-window)
   :map me/window-map
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
   ("=" . (lambda () (interactive) (text-scale-increase 0))))
  :preface
  (defun me/hsplit-last-window ()
    "Split window horizontally and focus the new window."
    (interactive)
    (split-window-horizontally)
    (other-window 1))
  (defun me/vsplit-last-window ()
    "Split window vertically and focus the new window."
    (interactive)
    (split-window-vertically)
    (other-window 1))
  (defun me/toggle-fullscreen-window ()
    "Toggle the current window as fullscreen, saving/restoring layout."
    (interactive)
    (if (= 1 (length (window-list)))
        (jump-to-register '_)
      (progn
        (window-configuration-to-register '_)
        (delete-other-windows))))
  (defun me/ace-window-prefix ()
    "Use ace-window to select a window for the next command's buffer.
See https://karthinks.com/software/emacs-window-management-almanac/"
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
  (aw-minibuffer-flag t)
  (display-buffer-base-action
   '((display-buffer-reuse-window
      display-buffer-in-previous-window
      ace-display-buffer)))
  :custom-face
  (aw-leading-char-face ((t (:foreground "red" :weight bold :height 2.0))))
  :config
  (setq window-combination-resize t))

;;; Writeroom — distraction-free writing
(use-package writeroom-mode
  :bind
  ("<f3>" . writeroom-mode))
