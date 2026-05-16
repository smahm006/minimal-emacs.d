;;; sessions.el --- Session persistence, backup and autosave -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Minibuffer history
(use-package savehist
  :ensure nil
  :hook (elpaca-after-init . savehist-mode)
  :custom
  (history-delete-duplicates t)
  (history-length 500)
  (savehist-file (format "%s/emacs/history" xdg-cache))
  (savehist-additional-variables
   '(kill-ring search-ring regexp-search-ring)))

;;; Cursor position persistence
(use-package saveplace
  :ensure nil
  :hook (elpaca-after-init . save-place-mode))

;;; Recently opened files
(use-package recentf
  :ensure nil
  :hook
  (elpaca-after-init . recentf-mode)
  (kill-emacs . recentf-cleanup)
  :bind
  ([remap recentf-open] . consult-recent-file)
  ([remap recentf-open-files] . consult-recent-file)
  (:map me/file-map
        ("r" . recentf-open))
  :custom
  (recentf-keep '(file-remote-p file-readable-p))
  (recentf-max-menu-items 10)
  (recentf-max-saved-items 100)
  (recentf-save-file (format "%s/emacs/recentf" xdg-cache)))

;;; Auto-revert buffers when files change on disk
(use-package autorevert
  :ensure nil
  :hook (elpaca-after-init . global-auto-revert-mode)
  :bind
  (:map me/file-map
        ("x" . revert-buffer))
  :custom
  (auto-revert-interval 1)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling t)
  (auto-revert-verbose nil)
  :config
  (setq revert-without-query '(".*")))

;;; Super-save — autosave on focus/window events
(use-package super-save
  :hook (elpaca-after-init . super-save-mode)
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 5)
  (super-save-remote-files nil)
  (super-save-silent t)
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (setq super-save-exclude '(".gpg")))

;;; Versioned backups
;; Force a fresh backup on every save, not just the first.
(defun me/force-backup-of-buffer ()
  "Force a new backup on every save by resetting the backed-up flag."
  (setq buffer-backed-up nil))
(add-hook 'before-save-hook #'me/force-backup-of-buffer)

(setq auto-save-default nil)          ; super-save handles autosave
(setq backup-by-copying t)            ; don't clobber symlinks
(setq vc-make-backup-files t)         ; backup even VCS-tracked files
(setq version-control t)              ; use numbered backups
(setq make-backup-files t)
(setq kept-new-versions 100)
(setq kept-old-versions 2)
(setq delete-old-versions t)
(setq delete-by-moving-to-trash t)

(defvar me/backup-dir (format "%s/emacs/backups/" xdg-data))
(me/mkdir me/backup-dir)
(setq backup-directory-alist `(("." . ,me/backup-dir)))

;;; Backup walker — step through versioned backups of a file
(use-package backup-walker
  :bind
  (:map me/file-map
        ("b" . backup-walker-start)))
