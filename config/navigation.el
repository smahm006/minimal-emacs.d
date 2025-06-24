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


;; Sidebar project exploration
(use-package treemacs
  :bind
  (("<f1>" . me/treemacs-toggle)
   ;; More dired like bindings
   :map treemacs-mode-map
   ;; Navigation
   ("n"   . treemacs-next-line)
   ("p"   . treemacs-previous-line)
   ("M-n" . treemacs-root-down)
   ("M-p" . treemacs-root-up)
   ("^"      . treemacs-root-up)
   ("g"      . treemacs-refresh)
   ;; File operations
   ("C"      . treemacs-copy-file)
   ("D"      . treemacs-delete-file)
   ("R"      . treemacs-move-file)
   ("+"      . treemacs-create-dir)
   ("%"      . treemacs-create-file)
   ;; Marking (similar to dired)
   ("m"      . treemacs-mark-or-unmark-path-at-point)
   ("u"      . treemacs-mark-or-unmark-path-at-point)
   ("U"      . treemacs-reset-marks)
   ("."      . treemacs-toggle-show-dotfiles)
   ;; Misc
   ("q"      . treemacs-quit)
   ("?"      . treemacs-common-helpful-hydra)
   ;; Opening files externally or in other windows
   ("x"      . treemacs-visit-node-in-external-application)
   ("o"      . treemacs-visit-node-ace))
  :preface
  (defun me/treemacs-toggle ()
    "Toggle Treemacs or kill it if already focused. When opening, also sync to current file in project."
    (interactive)
    (if (treemacs-is-treemacs-window-selected?)
        (treemacs-quit)
      (let ((origin-window (selected-window)))
        (treemacs-add-and-display-current-project-exclusively)
        ;; Go back to original window before syncing
        (select-window origin-window)
        (treemacs-find-file)
        (treemacs-select-window))))
  :custom
  (treemacs-indentation 2)
  (treemacs-indentation-string " ")
  (treemacs-is-never-other-window nil)
  (treemacs-max-git-entries 5000)
  (treemacs-no-png-images nil)
  (treemacs-no-delete-other-windows t)
  (treemacs-project-follow-cleanup nil)
  (treemacs-persist-file (expand-file-name "emacs/treemacs-persist" xdg-cache))
  (treemacs-position 'left)
  (treemacs-recenter-after-project-jump 'always)
  (treemacs-recenter-after-project-expand 'on-distance)
  (treemacs-wide-toggle-width 70)
  (treemacs-width 35)
  (treemacs-width-is-initially-locked t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))
