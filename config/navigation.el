;;; navigation.el --- Navigation configuration  -*- no-byte-compile: t; lexical-binding: t; -*-

;; Jump to previous locations stored in the mark ring
(setq set-mark-command-repeat-pop t)

;; Better emacs functions
;; From https://github.com/bbatsov/crux and my own research
(defun minimal-emacs/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line."
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(defun minimal-emacs/smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))
(defun minimal-emacs/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
(defun minimal-emacs/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(defun minimal-emacs/smart-open-line-below ()
  "Insert an empty line after the current line.
        Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(defun minimal-emacs/smart-open-line-above ()
  "Insert an empty line above the current line.
      Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))
(defun minimal-emacs/smart-kill-line-backwards ()
  "Insert an empty line above the current line.
      Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(global-set-key (kbd "M-p") 'minimal-emacs/move-line-up)
(global-set-key (kbd "M-n") 'minimal-emacs/move-line-down)
(global-set-key (kbd "C-a") 'minimal-emacs/smarter-move-beginning-of-line)
(global-set-key (kbd "C-<return>") 'minimal-emacs/smart-open-line-below)
(global-set-key (kbd "M-<return>") 'minimal-emacs/smart-open-line-above)
(global-set-key (kbd "M-<backspace>") 'minimal-emacs/smart-kill-line-backwards)


;; Jump to visible text using a char-based decision tree
(use-package avy
  :bind (:map minimal-emacs/search-map
              ("a" . avy-goto-char-timer))
  :preface
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))


;; Sidebar project exploration
(use-package treemacs
  :bind
  (("<f1>" . minimal-emacs/treemacs-toggle)
   :map treemacs-mode-map
   ("M-n" . treemacs-root-down)
   ("M-p" . treemacs-root-up))
  :preface
  (defun minimal-emacs/treemacs-toggle ()
    "Toggle treemacs or kill it if already focused."
    (interactive)
    (if (treemacs-is-treemacs-window-selected?)
        (treemacs-quit)
      (treemacs-add-and-display-current-project-exclusively)))
  :custom
  (treemacs-indentation 2)
  (treemacs-indentation-string " ")
  (treemacs-is-never-other-window nil)
  (treemacs-max-git-entries 5000)
  (treemacs-no-png-images nil)
  (treemacs-no-delete-other-windows t)
  (treemacs-project-follow-cleanup nil)
  (treemacs-persist-file (expand-file-name "emacs/treemacs-persist" xdg-cache))
  (treemacs-position 'left)
  (treemacs-recenter-after-project-jump 'always)
  (treemacs-recenter-after-project-expand 'on-distance)
  (treemacs-wide-toggle-width 70)
  (treemacs-width 35)
  (treemacs-width-is-initially-locked t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))
