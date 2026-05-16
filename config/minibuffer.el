;;; minibuffer.el --- Minibuffer completion, search and navigation -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Keybind discovery
(use-package which-key
  :ensure nil
  :hook (elpaca-after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

;;; Vertical completion UI
(use-package vertico
  :hook (elpaca-after-init . vertico-mode)
  :custom
  (vertico-cycle t))

;;; Flexible matching style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; Completion annotations in the minibuffer
(use-package marginalia
  :hook (elpaca-after-init . marginalia-mode))

;;; Context-sensitive actions on completion candidates
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;; Search and navigation commands
(use-package consult
  :bind
  (([remap Info-search]                  . consult-info)
   ([remap bookmark-jump]                . consult-bookmark)
   ([remap load-theme]                   . consult-theme)
   ([remap man]                          . consult-man)
   ([remap switch-to-buffer]             . consult-buffer)
   ([remap switch-to-buffer-other-window]. consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap kill-buffer]                  . kill-current-buffer)
   ([remap recentf-open]                 . consult-recent-file)
   ([remap recentf-open-files]           . consult-recent-file)
   ([remap project-list-buffers]         . consult-project-buffer)
   ([remap yank-pop]                     . consult-yank-pop)
   ([remap goto-line]                    . consult-goto-line)
   ("M-s"                                . consult-line)
   ("M-r"                                . consult-ripgrep)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history)
   :map me/goto-map
   ("e" . consult-compile-error)
   ("f" . consult-flymake)
   ("g" . consult-goto-line)
   ("o" . consult-outline)
   ("m" . consult-mark)
   ("k" . consult-global-mark)
   ("i" . consult-imenu)
   ("I" . consult-imenu-multi)
   :map me/search-map
   ("f" . consult-find)
   ("F" . consult-locate)
   ("g" . consult-grep)
   ("G" . consult-git-grep)
   ("r" . consult-ripgrep)
   ("l" . consult-line)
   ("L" . consult-line-multi)
   ("k" . consult-keep-lines)
   ("u" . consult-focus-lines)
   ("e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e"   . consult-isearch-history)
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip --hidden --glob=!.git"))

;;; Embark + Consult integration
(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; Jump to directories from the minibuffer
(use-package consult-dir
  :bind
  (("C-x C-d" . consult-dir)
   :map minibuffer-local-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file)
   :map me/file-map
   ("j" . consult-dir)))

;;; Edit grep/ripgrep results as a buffer
(use-package wgrep
  :bind
  (:map me/search-map
        ("w" . wgrep-change-to-wgrep-mode))
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;;; Richer help buffers
(use-package helpful
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-symbol]   . helpful-symbol)
   ([remap describe-key]      . helpful-key)
   ([remap describe-command]  . helpful-command)
   ("C-h F" . helpful-function)
   ("C-h ." . helpful-at-point)))
