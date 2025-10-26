;;; ui.el --- User Interface customization  -*- no-byte-compile: t; lexical-binding: t; -*-

;; Cursor settings
(setq make-pointer-invisible t)                           ; Hide mouse pointer when typing
(setq cursor-type t)                                      ; Default block shaped cursor
(setq x-stretch-cursor nil)                               ; Don't stretch cursor to the glyph width
(setq blink-cursor-mode t)                                ; Blink cursor

;; Theme settings
(minimal-emacs-load-user-init "themes/tomorrow-night-paradise-theme.el")
(load-theme 'tomorrow-night-paradise t)

;; Font settings
(set-face-attribute 'default nil :font "MapleMonoNormal 14")
(set-fontset-font t 'latin "Noto Sans")

;; Icon settings
(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia vertico
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after nerd-icons corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  :config
  (setq nerd-icons-corfu-mapping
        '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
          (file :fn nerd-icons-icon-for-file :face font-lock-string-face)
          (t :style "cod" :icon "code" :face font-lock-warning-face))))

(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons dired
  :commands (nerd-icons-dired-mode)
  :hook (dired-mode . nerd-icons-dired-mode))

;; Modeline settings
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq line-number-mode t)
  (setq column-number-mode t)
  (setq mode-line-position-column-line-format '("%l:%C"))
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-percent-position nil)
  (doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-icon t))

;; Show line number column
(setq display-line-numbers-grow-only t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'eat-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Highlight current line in prog mode
(add-hook 'prog-mode-hook #'global-hl-line-mode)
(add-hook 'org-mode-hook #'global-hl-line-mode)

;; Line guides
(use-package indent-bars
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python
                               argument_list parameters list list_comprehension dictionary
                               dictionary_comprehension parenthesized_expression subscript)))
  (indent-bars-treesit-scope '((python
                                function_definition class_definition for_statement
                                if_statement with_statement while_statement)))
  (indent-bars-color-by-depth nil)
  (indent-bars-highlight-current-depth '(:face default :blend 0.4))
  (indent-bars-pad-frac 0.1)
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.2)
  :hook
  (prog-mode . indent-bars-mode))
