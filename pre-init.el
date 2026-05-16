;;; pre-init.el --- Pre-init customization -*- no-byte-compile: t; lexical-binding: t; -*-

;;; User info
(setq user-full-name "Sohaib Mahmood")
(setq user-mail-address "soh.mahmood@fastmail.com")

;;; Elpaca bootstrap
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Enable 'elpaca-no-symlink-mode' on Windows, as symlink creation
;; often fails without Administrator privileges or Developer Mode.
(when (eq system-type 'windows-nt)
  (elpaca-no-symlink-mode 1))

;; Install use-package support and enable :ensure support for Elpaca.
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; Block until all queued Elpaca packages (above) are ready before
;; continuing. This ensures use-package is available for the rest of
;; pre-init.el and all subsequent config files.
(use-package compat :demand t)
(use-package transient :demand t)
(elpaca-wait)

;; Default all use-package declarations to :ensure t so only built-in
;; packages need an explicit :ensure nil to opt out.
(setq use-package-always-ensure t)

;;; Clipboard / kill-ring integration
(setq save-interprogram-paste-before-kill t)
(setq yank-pop-change-selection t)

;;; Keybind prefix maps
;; These are defined here (before any config file loads) so that every
;; use-package :bind in config/* and languages/* can reference them safely.
(defvar-keymap me/org-map       :doc "Prefix map for Org commands.")
(defvar-keymap me/note-map      :doc "Prefix map for note-taking commands.")
(defvar-keymap me/window-map    :doc "Prefix map for window commands.")
(defvar-keymap me/buffer-map    :doc "Prefix map for buffer commands.")
(defvar-keymap me/file-map      :doc "Prefix map for file commands.")
(defvar-keymap me/project-map   :doc "Prefix map for project commands.")
(defvar-keymap me/search-map    :doc "Prefix map for search commands.")
(defvar-keymap me/goto-map      :doc "Prefix map for goto/navigation commands.")
(defvar-keymap me/mc-map        :doc "Prefix map for multiple-cursors commands.")
(defvar-keymap me/vc-map        :doc "Prefix map for version control commands.")
(defvar-keymap me/lsp-map       :doc "Prefix map for LSP/eglot commands.")
(defvar-keymap me/container-map :doc "Prefix map for container commands.")
(defvar-keymap me/run-map       :doc "Prefix map for run/compile commands.")
(defvar-keymap me/treesit-map   :doc "Prefix map for treesitter commands.")
(defvar-keymap me/word-map      :doc "Prefix map for word and text commands.")

(keymap-set global-map "C-c o" me/org-map)
(keymap-set global-map "C-c n" me/note-map)
(keymap-set global-map "C-c w" me/window-map)
(keymap-set global-map "C-c b" me/buffer-map)
(keymap-set global-map "C-c f" me/file-map)
(keymap-set global-map "C-c p" project-prefix-map)
(keymap-set global-map "C-c s" me/search-map)
(keymap-set global-map "C-c g" me/goto-map)
(keymap-set global-map "C-c m" me/mc-map)
(keymap-set global-map "C-c v" me/vc-map)
(keymap-set global-map "C-c l" me/lsp-map)
(keymap-set global-map "C-c d" me/container-map)
(keymap-set global-map "C-c r" me/run-map)
(keymap-set global-map "C-c t" me/treesit-map)
(keymap-set global-map "C-c j" me/word-map)

;;; Utility functions
(defun me/revert-buffer-no-confirm ()
  "Revert the current buffer without asking for confirmation."
  (interactive)
  (revert-buffer nil t t))

(defun me/mkdir (dir)
  "Create DIR and all parent directories if they don't exist."
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defun me/location ()
  "Return the current machine location: \\='home or \\='work."
  (if (string-prefix-p "sm-" (system-name))
      'home
    'work))

;;; Sensitive file mode
;; Disable backups and auto-save for files that should never be written to disk.
(define-minor-mode sensitive-mode
  "For sensitive files, do not write backups or auto-saves."
  :init-value nil
  (if sensitive-mode
      (progn
        (setq-local make-backup-files nil)
        (setq-local auto-save-default nil))
    (setq-local make-backup-files (default-value 'make-backup-files))
    (auto-save-mode (if (default-value 'auto-save-default) 1 -1))))

(setq auto-mode-alist
      (append '(("\\.vcf$" . sensitive-mode)
                ("\\.gpg$" . sensitive-mode))
              auto-mode-alist))
