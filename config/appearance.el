;;; appearance.el --- Visual and UI configuration -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Terminal (tty) frames
;; Allow copying to the clipboard from terminal frames.
(setq xterm-extra-capabilities '(setSelection modifyOtherKeys))

;;; Theme + fonts — daemon-safe
;; When Emacs runs as a daemon, init happens with no display: face/font setup
;; done at init time never reaches frames that emacsclient creates later, and
;; theme colors get mis-approximated on tty frames. So apply per-frame.
(defun me/apply-appearance (&optional frame)
  "Load the theme and set fonts for FRAME (daemon-safe)."
  (when frame (select-frame frame))
  (load-theme 'almost-mono-black t)
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'default nil :font "SauceCodeProNerdFont 14")
        (set-fontset-font t 'latin "Noto Sans"))
    ;; tty frames can't render the theme's true black — drop the background
    ;; so the terminal's own (translucent) background shows through
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(use-package almost-mono-themes
  :config
  (setf (cdr (assoc 'highlight (cdr (assoc 'black almost-mono-themes-colors)))) "#00ff00")
  (add-to-list 'default-frame-alist '(font . "SauceCodeProNerdFont-14"))
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'me/apply-appearance)
    (me/apply-appearance)))

;;; Icons
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;;; Modeline
(use-package doom-modeline
  :hook (elpaca-after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-percent-position nil)
  (doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-icon t)
  :config
  (setq line-number-mode t)
  (setq column-number-mode t)
  (setq mode-line-position-column-line-format '("%l:%C")))

;;; Line numbers
;; Show line numbers in all editing buffers.
(setq display-line-numbers-grow-only t)
(dolist (hook '(prog-mode-hook conf-mode-hook text-mode-hook))
  (add-hook hook #'display-line-numbers-mode))
;; Org mode is text-mode derived but line numbers add no value there.
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

;;; Line highlighting
;; Enable globally and then disable in buffers where it is distracting.
(global-hl-line-mode 1)
(dolist (hook '(eat-mode-hook
                pdf-view-mode-hook))
  (add-hook hook (lambda () (setq-local global-hl-line-mode nil))))

;;; Rainbow delimiters
;; Color-code nested delimiters where delimiter depth is structurally useful.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
