;;; appearance.el --- Visual and UI configuration -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Terminal frames
;; Allow clipboard use in terminals.
(setq xterm-extra-capabilities '(setSelection modifyOtherKeys))

(defun me/apply-appearance (&optional frame)
  "Apply the theme and fonts to FRAME."
  (when frame (select-frame frame))
  (load-theme 'danneskjold t)
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'default nil :font "SauceCodeProNerdFont 14")
        (set-fontset-font t 'latin "Noto Sans"))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(use-package danneskjold-theme
  :ensure t
  :config
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
;; Show line numbers in editing buffers.
(setq display-line-numbers-grow-only t)
(dolist (hook '(prog-mode-hook conf-mode-hook text-mode-hook))
  (add-hook hook #'display-line-numbers-mode))
;; Org does not need line numbers.
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

;;; Line highlighting
;; Enable globally; disable it in noisy buffers.
(global-hl-line-mode 1)
(dolist (hook '(eat-mode-hook
                pdf-view-mode-hook
                agent-shell-mode-hook))
  (add-hook hook (lambda () (setq-local global-hl-line-mode nil))))

;;; Rainbow delimiters
;; Color nested delimiters.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
