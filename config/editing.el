;;; editing.el --- Text editing configuration  -*- no-byte-compile: t; lexical-binding: t; -*-

(setq kill-ring-max 128)                                  ; Maximum length of kill ring
(setq mark-ring-max 128)                                  ; Maximum length of mark ring
(delete-selection-mode 1)                                 ; Typing will replace a selected region

;; Default to utf-8 encoding
(setq set-default-coding-systems 'utf-8)
(setq prefer-coding-system 'utf-8)
(setq set-terminal-coding-system 'utf-8)
(setq set-selection-coding-system 'utf-8)

;; Delete trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; The kill ring is rotated after selecting previously killed text.
(setq yank-from-kill-ring-rotate t)

;; Better editing functions
;; From https://github.com/bbatsov/crux and my own research
(defun me/backward-kill-thing ()
  "Delete sexp, symbol, word or whitespace backward depending on the context at point."
  (interactive)
  (let ((bounds (seq-some #'bounds-of-thing-at-point '(sexp symbol word))))
    (cond
     ;; If there are bounds and point is within them, kill the region
     ((and bounds (< (car bounds) (point)))
      (kill-region (car bounds) (point)))
     ;; If there's whitespace before point, delete it
     ((thing-at-point-looking-at "\\([ \n]+\\)")
      (if (< (match-beginning 1) (point))
          (kill-region (match-beginning 1) (point))
        (kill-backward-chars 1)))
     ;; If none of the above, delete one character backward
     (t
      (kill-backward-chars 1)))))

(defun me/kill-region-or-line ()
  "Kill the region if active, otherwise kill the current line.
   With a prefix argument, copy the entire buffer content to the kill-ring."
  (interactive)
  (if current-prefix-arg
      (progn
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (if (use-region-p)
        (kill-region (region-beginning) (region-end) t)
      (kill-region (line-beginning-position) (line-beginning-position 2)))))

(defun me/smart-kill-line-backwards ()
  "Insert an empty line above the current line.
      Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(defun me/smarter-move-beginning-of-line (arg)
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

(defun me/smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

(defun me/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun me/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun me/smart-open-line-below ()
  "Insert an empty line after the current line.
        Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun me/smart-open-line-above ()
  "Insert an empty line above the current line.
      Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

;; Bind custom functions
(global-set-key [remap kill-region] #'me/kill-region-or-line)
(global-set-key (kbd "M-<backspace>") #'me/smart-kill-line-backwards)
(global-set-key (kbd "C-<backspace>") #'me/backward-kill-thing)
(global-set-key (kbd "C-<return>") #'me/smart-open-line-below)
(global-set-key (kbd "M-<return>") #'me/smart-open-line-above)
(global-set-key (kbd "M-p") #'me/move-line-up)
(global-set-key (kbd "M-n") #'me/move-line-down)
(global-set-key (kbd "C-a") #'me/smarter-move-beginning-of-line)

;; Nicer interface to Query-Replace
(use-package visual-replace
  :bind (([remap query-replace] . visual-replace)
         :map isearch-mode-map
         ("M-%" . visual-replace-from-isearch))
  :config
  (define-key visual-replace-mode-map [remap yank] nil)
  (define-key visual-replace-mode-map [remap yank-pop] nil)
  (define-key visual-replace-mode-map (kbd "M-%")
              visual-replace-secondary-mode-map))

;; Edit multiple lines at once
(use-package multiple-cursors
  :bind (:map me/mc-map
              ("<escape>" . mc/keyboard-quit)
              ("r" . mc/mark-all-in-region-regexp)
              ("b" . mc/edit-beginnings-of-lines)
              ("e" . mc/edit-ends-of-lines)
              :repeat-map me/mc-map
              ("n" . mc/mark-next-like-this)
              ("p" . mc/mark-previous-like-this)
              :exit
              ("a" . mc/mark-all-like-this)
              ("m" . mc/edit-lines)))

;; Smarter commenting + decommenting
(use-package smart-comment
  :bind ("M-;" . smart-comment))

;; Parenthesis settings
(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'conf-mode-hook #'electric-pair-mode)
(add-hook 'prog-mode-hook #'show-paren-mode)
(setq show-paren-style 'parenthesis)
(setq show-paren-when-point-in-periphery t)
(setq show-paren-when-point-inside-paren nil)

;; Undo/Redo setings
(use-package vundo
  :bind
  (:map ctl-x-map
        ("u". vundo))
  :config
  (when (display-graphic-p)
    (setq vundo-glyph-alist vundo-unicode-symbols)))

(use-package undo-fu
  :bind (([remap undo] . undo-fu-only-undo)
         ([remap undo-redo] . undo-fu-only-redo))
  :custom
  (undo-fu-allow-undo-in-region t)
  (undo-limit 67108864) ; 64MB.
  (undo-strong-limit 100663296) ; 96MB.
  (undo-outer-limit 1006632960)) ; 960MB
