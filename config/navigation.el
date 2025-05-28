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

;; File/Tag summarizing utility
(use-package speedbar
  :ensure nil
  :hook
  ((speedbar-mode . (lambda()
                      ;; Disable word wrapping in speedbar if you always enable it globally.
                      (visual-line-mode 0)
                      ;; Change speedbar's text size.  May need to alter the icon size if you change size.
                      (text-scale-adjust -1))))
  :bind (:map speedbar-file-key-map
              ("<tab>" . speedbar-expand-line )
              ("<backtab>" . speedbar-contract-line )
              ("M-p" . speedbar-up-directory))
  :custom
  (speedbar-frame-parameters
   '((name . "speedbar")
     (title . "speedbar")
     (minibuffer . nil)
     (border-width . 2)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 10)))
  ;; Increase the indentation for better useability.
  (speedbar-indentation-width 3)
  ;; make speedbar update automaticaly, and dont use ugly icons(images)
  (speedbar-update-flag t)
  (speedbar-use-images nil)
  :config
  ;; list of supported file-extensions
  ;; feel free to add to this list
  (speedbar-add-supported-extension
   (list
    ;; lua and fennel(lisp that transpiles to lua)
    ".lua"
    ".fnl"
    ".fennel"
    ;; shellscript
    ".sh"
    ".bash";;is this ever used?
    ;; web languages
    ;; Hyper-Text-markup-language(html) and php
    ".php"
    ".html"
    ".htm"
    ;; ecma(java/type)-script
    ".js"
    ".json"
    ".ts"
    ;; stylasheets
    ".css"
    ".less"
    ".scss"
    ".sass"
    ;; c/c++ and makefiles
    ".c"
    ".cpp"
    ".h"
    "makefile"
    "MAKEFILE"
    "Makefile"
    ;; runs on JVM, java,kotlin etc
    ".java"
    ".kt";;this is for kotlin
    ".mvn"
    ".gradle" ".properties";; this is for gradle-projects
    ".clj";;lisp on the JVM
    ;; lisps
    ".cl"
    ".el"
    ".scm"
    ".lisp"
    ;; configuration
    ".yaml"
    ".toml"
    ;; json is already in this list
    ;; notes,markup and orgmode
    ".md"
    ".markdown"
    ".org"
    ".txt"
    "README"
    ;; Jupyter Notebooks
    ".ipynb")))

;; Speedbar sidebar
(use-package sr-speedbar
  :preface
  (defun minimal-emacs/sr-speedbar-toggle-smart ()
    "Toggle sr-speedbar or kill it if already focused."
    (interactive)
    (if (string= (buffer-name) "*SPEEDBAR*")
        ;; If currently in *SPEEDBAR*, switch and kill the buffer
        (progn
          (sr-speedbar-toggle)
          (kill-buffer "*SPEEDBAR*"))
      ;; Otherwise, toggle and focus
      (progn
        (sr-speedbar-toggle)
        (sr-speedbar-select-window))))
  :bind
  ("<f2>" . minimal-emacs/sr-speedbar-toggle-smart)
  :custom
  (sr-speedbar-right-side nil))
