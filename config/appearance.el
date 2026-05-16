;;; appearance.el --- Visual and UI configuration -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Theme
(use-package almost-mono-themes
  :config
  (setf (cdr (assoc 'highlight (cdr (assoc 'black almost-mono-themes-colors)))) "#00ff00")
  (load-theme 'almost-mono-black t))

;;; Fonts
(set-face-attribute 'default nil :font "SauceCodeProNerdFont 14")
(set-fontset-font t 'latin "Noto Sans")

;;; Icons
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-completion
  :after (marginalia vertico)
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :after (nerd-icons corfu)
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  :config
  (setq nerd-icons-corfu-mapping
        '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
          (file :fn nerd-icons-icon-for-file :face font-lock-string-face)
          (t :style "cod" :icon "code" :face font-lock-warning-face))))

(use-package nerd-icons-dired
  :after (nerd-icons dired)
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :after (nerd-icons ibuffer)
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

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

;;; Indent guides
(use-package indent-bars
  :hook (prog-mode . indent-bars-mode)
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python
                               argument_list parameters list list_comprehension
                               dictionary dictionary_comprehension
                               parenthesized_expression subscript)))
  (indent-bars-treesit-scope '((python
                                function_definition class_definition
                                for_statement if_statement with_statement
                                while_statement)))
  (indent-bars-color-by-depth nil)
  (indent-bars-highlight-current-depth '(:face default :blend 0.4))
  (indent-bars-pad-frac 0.1)
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.2))

;;; Rainbow delimiters
;; Color-code nested delimiters by depth for easier visual orientation.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Highlight TODO/FIXME/HACK/NOTE keywords in code
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-keyword-faces
   '(("TODO"  . "#ff9900")
     ("FIXME" . "#ff0000")
     ("HACK"  . "#dd00dd")
     ("NOTE"  . "#00cc00")
     ("XXX"   . "#ff0000"))))

;;; Beacon — flash the cursor after large movements
(use-package beacon
  :config
  (beacon-mode 1)
  :custom
  (beacon-color "#00ff00")
  (beacon-size 60)
  (beacon-blink-duration 0.3)
  (beacon-blink-when-window-scrolls t)
  (beacon-blink-when-window-changes t)
  (beacon-blink-when-point-moves-vertically 10))

;;; Dimmer — dim inactive windows
(use-package dimmer
  :init
  (defun me/dimmer-config-change-handler ()
    "Only process dimmer if no predicate is truthy."
    (let ((ignore (cl-some (lambda (f) (and (fboundp f) (funcall f)))
                           dimmer-prevent-dimming-predicates)))
      (unless ignore
        (when (fboundp 'dimmer-process-all)
          (dimmer-process-all t)))))
  (defun me/corfu-frame-p ()
    "Return non-nil if the buffer is a corfu popup frame."
    (string-match-p "\\` \\*corfu" (buffer-name)))
  :config
  (advice-add 'dimmer-config-change-handler
              :override #'me/dimmer-config-change-handler)
  (add-to-list 'dimmer-prevent-dimming-predicates #'me/corfu-frame-p)
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-mode 1)
  :custom
  (dimmer-fraction 0.3))
