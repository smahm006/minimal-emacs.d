;;; backup.el --- Backup customization  -*- no-byte-compile: t; lexical-binding: t; -*-

;; savehist and saveplace settings
(use-package savehist
  :ensure nil
  :hook
  (after-init . savehist-mode)
  (after-init . save-place-mode)
  :custom
  (history-delete-duplicates t)
  (history-length 500)
  (savehist-file (format "%s/emacs/history" xdg-cache))
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring)))

;; recentf settings
(use-package recentf
  :ensure nil
  :hook
  (after-init . (lambda()
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

;; auto-revert settings
(use-package auto-revert
  :ensure nil
  :bind
  (:map me/file-map
        ("x" . revert-buffer))
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  ;; Revert Dired and other buffers
  (global-auto-revert-non-file-buffers t)
  ;; Avoid polling for changes and rather get notified by the system
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling t))

;; Backup and auto-save settings
(setq backup-by-copying t)                              ; Backs up by copying instead of renaming
(setq vc-make-backup-files t)                           ; Back up version-controlled files
(setq version-control t)                                ; Enable versioned backups
(setq make-backup-files t)                              ; Make backups of saved files
(setq kept-new-versions 100)                            ; Keep 100 newest versions
(setq kept-old-versions 2)                              ; Keep 2 oldest versions
(setq delete-old-versions t)                            ; Delete excess backups
(setq delete-by-moving-to-trash t)                      ; Move backups to trash on delete
(setq auto-save-default t)                              ; Enable auto-saving
(setq auto-save-timeout 30)                             ; Seconds before auto-save triggers
(setq auto-save-interval 300)                           ; Keystrokes before auto-save triggers
(setq auto-save-list-file-prefix nil)                   ; Disable auto-save list files

;; Define directories for backups and autosaves
(defvar backup-session-dir (format "%s/emacs/backups/session/" xdg-data))
(me/mkdir backup-session-dir)
(defvar backup-save-dir (format "%s/emacs/backups/save/" xdg-data))
(me/mkdir backup-save-dir)
(let ((auto-save-dir (format "%s/emacs/auto-save/" xdg-cache)))
  (me/mkdir auto-save-dir)
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t))))

;; Set where Emacs saves regular backups
(setq backup-directory-alist `(("." . ,backup-save-dir)))

;; Define the custom backup function
(defun me/backup-buffer ()
  "Make a session backup at the first save of each emacs session and a save backup on each subsequent save."
  (when (not buffer-backed-up)
    (let ((backup-directory-alist `(("." . ,backup-session-dir)))
          (kept-new-versions 3))
      (backup-buffer)))
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook #'me/backup-buffer)

;; Traverse file backups
(use-package backup-walker
  :bind
  (:map me/file-map
        ("b" . backup-walker-start)))
