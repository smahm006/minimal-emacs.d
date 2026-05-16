;;; editing.el --- Text editing, spellcheck and structural editing -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Basic editing settings
(setq kill-ring-max 128)                ; maximum length of kill ring
(setq mark-ring-max 128)               ; maximum length of mark ring
(setq set-mark-command-repeat-pop t)   ; C-u C-SPC repeats without C-u
(setq yank-from-kill-ring-rotate t)    ; rotate kill ring after yank-pop
(setq reb-re-syntax 'string)           ; use string syntax in re-builder
(delete-selection-mode 1)              ; typing replaces active region

;;; Encoding
(setq prefer-coding-system        'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;;; Whitespace
;; Delete trailing whitespace on save but preserve the cursor column.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;; Parentheses
(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'conf-mode-hook #'electric-pair-mode)
(setq show-paren-style 'parenthesis)
(setq show-paren-when-point-in-periphery t)
(setq show-paren-when-point-inside-paren nil)
(add-hook 'prog-mode-hook #'show-paren-mode)

;;; Custom editing functions
(defun me/backward-kill-thing ()
  "Delete pair, word, whitespace or char backward depending on context."
  (interactive)
  (cond
   ((and (not (bobp))
         (memq (char-syntax (char-before)) '(?\) ?\] ?\} ?\> ?\" ?\' ?\`))
         (not (looking-back "^[[:blank:]]*" (line-beginning-position))))
    (let ((end (point))
          (start (save-excursion (backward-sexp) (point))))
      (message "Deleted paired delimiters")
      (kill-region start end)))
   ((looking-back (rx (char word)) 1)
    (message "Deleted word")
    (backward-kill-word 1))
   ((looking-back (rx (char blank)) 1)
    (message "Deleted whitespace")
    (delete-horizontal-space t))
   (t
    (backward-delete-char 1)
    (message "Deleted character"))))

(defun me/kill-region-or-line ()
  "Kill the region if active, otherwise kill the current line.
With a prefix argument, copy the entire buffer to the kill-ring and clear it."
  (interactive)
  (if current-prefix-arg
      (progn
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (if (use-region-p)
        (kill-region (region-beginning) (region-end) t)
      (kill-region (line-beginning-position) (line-beginning-position 2)))))

(defun me/smart-kill-line-backwards ()
  "Kill from point to the beginning of the line, respecting indentation."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(defun me/smarter-move-beginning-of-line (arg)
  "Move to indentation, or to the true beginning of line if already there."
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
  "Kill the whole line and move point back to indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

(defun me/move-line-up ()
  "Move the current line up by one."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun me/move-line-down ()
  "Move the current line down by one."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun me/smart-open-line-below ()
  "Insert an empty line after the current line and move point there."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun me/smart-open-line-above ()
  "Insert an empty line above the current line and move point there."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

;;; Bind custom editing functions
(global-set-key [remap kill-region]         #'me/kill-region-or-line)
(global-set-key (kbd "M-<backspace>")       #'me/smart-kill-line-backwards)
(global-set-key (kbd "C-<backspace>")       #'me/backward-kill-thing)
(global-set-key (kbd "C-<return>")          #'me/smart-open-line-below)
(global-set-key (kbd "M-<return>")          #'me/smart-open-line-above)
(global-set-key (kbd "M-p")                 #'me/move-line-up)
(global-set-key (kbd "M-n")                 #'me/move-line-down)
(global-set-key (kbd "C-a")                 #'me/smarter-move-beginning-of-line)

;;; Visual replace — nicer query-replace interface
(use-package visual-replace
  :bind
  (([remap query-replace] . visual-replace)
   :map isearch-mode-map
   ("M-%" . visual-replace-from-isearch))
  :config
  (define-key visual-replace-mode-map [remap yank] nil)
  (define-key visual-replace-mode-map [remap yank-pop] nil)
  (define-key visual-replace-mode-map (kbd "M-%")
              visual-replace-secondary-mode-map))

;;; Multiple cursors
(use-package multiple-cursors
  :bind
  (:map me/mc-map
        ("<escape>" . mc/keyboard-quit)
        ("r"        . mc/mark-all-in-region-regexp)
        ("b"        . mc/edit-beginnings-of-lines)
        ("e"        . mc/edit-ends-of-lines)
        :repeat-map me/mc-map
        ("n"        . mc/mark-next-like-this)
        ("p"        . mc/mark-previous-like-this)
        :exit
        ("a"        . mc/mark-all-like-this)
        ("m"        . mc/edit-lines))
  (:map mc/keymap
        ("<return>" . newline)))

;;; Indentation detection
;; Automatically detect and use the indentation style of the current file.
(use-package dtrt-indent
  :hook
  (prog-mode . dtrt-indent-mode)
  (text-mode . dtrt-indent-mode)
  (conf-mode . dtrt-indent-mode))

;;; Combobulate — structured editing via treesitter
(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate")
  :hook
  (python-ts-mode    . combobulate-mode)
  (js-ts-mode        . combobulate-mode)
  (css-ts-mode       . combobulate-mode)
  (yaml-ts-mode      . combobulate-mode)
  (json-ts-mode      . combobulate-mode)
  (toml-ts-mode      . combobulate-mode)
  (typescript-ts-mode . combobulate-mode)
  (tsx-ts-mode       . combobulate-mode)
  (go-ts-mode        . combobulate-mode)
  (rust-ts-mode      . combobulate-mode))

;;; Spellcheck
;; Use jinx (enchant-2) if available, otherwise fall back to flyspell (hunspell).
(use-package jinx
  :if (executable-find "enchant-2")
  :hook (elpaca-after-init . global-jinx-mode)
  :bind
  (:map me/word-map
      ("j" . jinx-correct)
      ("l" . jinx-languages)
      ("u" . upcase-region)
      ("d" . downcase-region)
      ("c" . capitalize-region))
  :custom
  (jinx-languages "en_US"))

(use-package flyspell
  :ensure nil
  :if (and (not (executable-find "enchant-2"))
           (executable-find "hunspell"))
  :hook
  ((text-mode org-mode LaTeX-mode) . flyspell-mode)
  ((prog-mode conf-mode)           . flyspell-prog-mode)
  (ispell-change-dictionary        . me/restart-flyspell-mode)
  :preface
  (defun me/restart-flyspell-mode ()
    "Restart flyspell-mode to pick up a dictionary change."
    (when flyspell-mode
      (flyspell-mode-off)
      (flyspell-mode-on)
      (flyspell-buffer)))
  :custom
  (flyspell-issue-welcome-flag nil)
  (flyspell-issue-message-flag nil))

(use-package ispell
  :ensure nil
  :if (and (not (executable-find "enchant-2"))
           (executable-find "hunspell"))
  :after flyspell
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary "en_US,de_DE")
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,de_DE"))
