;;; backup.el --- Backup & autosave configuration -*- no-byte-compile: t; lexical-binding: t; -*-

;;; savehist / saveplace
(use-package savehist
  :ensure nil
  :hook
  (after-init . savehist-mode)
  (after-init . save-place-mode)
  :custom
  (history-delete-duplicates t)
  (history-length 500)
  (savehist-file (format "%s/emacs/history" xdg-cache))
  (savehist-additional-variables
   '(kill-ring search-ring regexp-search-ring)))

;;; recentf
(use-package recentf
  :ensure nil
  :hook
  (after-init . (lambda ()
                  (let ((inhibit-message t))
                    (recentf-mode 1))))
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

;;; auto-revert
(use-package auto-revert
  :ensure nil
  :bind
  (:map me/file-map
        ("x" . revert-buffer))
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 1)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling t)
  (auto-revert-verbose nil)
  :config
  (setq revert-without-query '(".*")))

;;; super-save (modern autosave)
(use-package super-save
  :hook
  (after-init . super-save-mode)
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 5)
  (super-save-remote-files nil)
  (super-save-silent t)
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (setq super-save-exclude '(".gpg")))

;;; Force backup on save
(defun me/force-backup-of-buffer ()
  "Make a backup every time the file is saved."
  (setq buffer-backed-up nil))
(add-hook 'before-save-hook #'me/force-backup-of-buffer)

;;; Backup settings (versioned backups)
(setq auto-save-default nil)
(setq backup-by-copying t)
(setq vc-make-backup-files t)
(setq version-control t)
(setq make-backup-files t)
(setq kept-new-versions 100)
(setq kept-old-versions 2)
(setq delete-old-versions t)
(setq delete-by-moving-to-trash t)

;;; Backup directory
(defvar backup-save-dir (format "%s/emacs/backups/" xdg-data))
(me/mkdir backup-save-dir)

(setq backup-directory-alist
      `(("." . ,backup-save-dir)))

;;; backup-walker
(use-package backup-walker
  :bind
  (:map me/file-map
        ("b" . backup-walker-start)))
