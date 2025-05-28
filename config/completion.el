;;; completion.el --- Completion customization  -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package which-key
  :ensure nil
  :defer t
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

(use-package consult
  :bind (([remap Info-searpch] . consult-info)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap load-theme] . consult-theme)
         ([remap man] . consult-man)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap kill-buffer] . kill-current-buffer)
         ([remap recentf-open] . consult-recent-file)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap project-list-buffers] . consult-project-buffer)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history)
         :map minimal-emacs/goto-map
         ("e" . consult-compile-error)
         ("f" . consult-flymake)
         ("g" . consult-goto-line)
         ("o" . consult-outline)
         ("m" . consult-mark)
         ("k" . consult-global-mark)
         ("i" . consult-imenu)
         ("I" . consult-imenu-multi)
         :map minimal-emacs/search-map
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
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi))
  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package vertico
  :ensure t
  :defer t
  :commands vertico-mode
  :hook (after-init . vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  :ensure t
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode)
  :hook
  ((after-init . global-corfu-mode)
   (after-init . corfu-popupinfo-mode)
   (after-init . corfu-echo-mode)
   (after-init . corfu-history-mode)
   ;; Disable auto completion for termintal
   (eat-mode . (lambda () (setq-local corfu-auto nil) (corfu-mode))))
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
		("SPC" . corfu-insert-separator)
        ("C-/" . corfu-insert)
        ("<escape>"  . corfu-quit))
  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function
  (text-mode-ispell-word-completion nil)
  (tab-always-indent t)               ; Do not use TAB for completion
  (corfu-cycle t)                     ; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                      ; Enable auto completion
  (corfu-quit-no-match t)             ; Quit auto complete if there is no match
  (corfu-preview-current t)           ; Disable current candidate preview
  :config
  (keymap-unset corfu-map "RET")
  (define-key corfu-map [remap previous-line] nil)
  (define-key corfu-map [remap next-line] nil)
  (define-key corfu-map [remap corfu-first] nil)
  (define-key corfu-map [remap corfu-last] nil))

;; Completion At Point Extensions
(use-package cape
  :init
  ;; Note: The order matters! File is first
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Better dabbrev expand
;; https://www.masteringemacs.org/article/text-expansion-hippie-expand
(use-package hippie-expand
  :ensure nil
  :bind ([remap dabbrev-exand] . hippie-expand)
  :config
  (setq hippie-expand-verbose t)
  (setq hippie-expand-dabbrev-skip-space t)
  (setq hippie-expand-try-functions-list
        '(
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-expand-list
          try-expand-line
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs)))

;; Snippet completion
(use-package tempel
  :hook
  (conf-mode . minimal-emacs/tempel-setup-capf)
  (prog-mode . minimal-emacs/tempel-setup-capf)
  (text-mode . minimal-emacs/tempel-setup-capf)
  :bind (("M-+" . tempel-expand)
         ("M-*" . tempel-insert)
         (:map tempel-map
               ("TAB" . tempel-next)
               ([tab] . tempel-next)
               ("S-TAB" . tempel-previous)
               ([backtab] . tempel-previous)))
  :preface
  ;; Setup completion at point
  (defun minimal-emacs/tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))
  :custom
  (tempel-path (format "%s/snippets/*.eld" user-emacs-directory)))

;; Spell Checking
(use-package ispell
  :ensure nil
  :after flyspell
  :if (executable-find "hunspell")
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary "en_US,de_DE")
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,de_DE"))

(use-package flyspell
  :if (not (executable-find "enchant-2"))
  :ensure nil
  :hook
  (((text-mode org-mode LaTeX-mode) . flyspell-mode)
   ((prog-mode conf-mode) . flyspell-prog-mode)
   (ispell-change-dictionary . restart-flyspell-mode))
  :preface
  (defun my/restart-flyspell-mode ()
    (when flyspell-mode
      (flyspell-mode-off)
      (flyspell-mode-on)
      (flyspell-buffer)))
  :custom
  (flyspell-issue-welcome-flag nil)
  (flyspell-issue-message-flag nil))

(use-package jinx
  :if (executable-find "enchant-2")
  :hook (emacs-startup . global-jinx-mode)
  :bind (([remap ispell-word] . jinx-correct)
         ("C-M-$" . jinx-languages))
  :custom
  (jinx-languages "en_US"))
