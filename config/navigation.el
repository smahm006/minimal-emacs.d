;;; navigation.el --- Navigation configuration  -*- no-byte-compile: t; lexical-binding: t; -*-

;; Jump to previous locations stored in the mark ring
(setq set-mark-command-repeat-pop t)

;; Jump to visible text using a char-based decision tree
(use-package avy
  :bind (:map me/search-map
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

;; File Navigation
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :hook (dired-mode . dired-omit-mode)
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-find-file)
        ("M-n" . dired-find-file)
        ("<backtab>" . dired-up-directory)
        ("M-p" . dired-up-directory)
        ( "."     . dired-omit-mode)
        ("C-+" . dired-create-empty-file)
        ("Q" . (lambda ()
                 (interactive)
                 (mapc (lambda (buffer)
                         (when (or (eq 'dired-mode (buffer-local-value 'major-mode buffer))
                                   (string-match-p "^\\*image-dired" (buffer-name buffer)))
                           (kill-buffer buffer)))
                       (buffer-list)))))
  (:map me/file-map
        ("d" . (lambda ()
                 (interactive)
                 (dired-jump)
                 (revert-buffer)))
        ("D" . dired)
        ("f" . find-file-at-point))
  :custom
  (dired-omit-files (rx (seq bol ".")))
  (dired-listing-switches "-goah --group-directories-first --time-style=long-iso -v")
  (dired-kill-when-opening-new-dired-buffer nil)
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-create-destination-dirs 'ask)
  (dired-clean-confirm-killing-deleted-buffers nil))


;; Sidebar project exploration
(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :bind
  (("<f1>" . dired-sidebar-toggle-sidebar)
   :map dired-sidebar-mode-map
        ("<tab>" . dired-sidebar-subtree-toggle)
        ("M-n" . dired-sidebar-subtree-toggle)
        ("<backtab>" . dired-subtree-up)
        ("M-p" . dired-subtree-up))
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-theme 'nerd-icons)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))
