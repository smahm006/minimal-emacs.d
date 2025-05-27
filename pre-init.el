;;; pre-init.el --- Pre-Init.el  -*- no-byte-compile: t; lexical-binding: t; -*-

;; User configuration
(setq user-full-name "Sohaib Mahmood")
(setq user-mail-address "soh.mahmood@fastmail.com")

;; Merge system's and Emacs' clipboard
(setq select-enable-clipboard t)

;; Keybind settings
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "<f5>")
  (lambda () (interactive)
    (minimal-emacs-load-user-init "init.el")))

;; Useful Functions
(defun minimal-emacs/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun minimal-emacs/mkdir (dir-path)
  "Make directory in DIR-PATH if it doesn't exist."
  (unless (file-exists-p dir-path)
    (make-directory dir-path t)))

(defun minimal-emacs/location ()
  "Return 'home' if system-name starts with 'sm-', otherwise return 'work'."
  (if (string-match-p "^sm-" (system-name))
      "home"
    "work"))

;; Keymaps
(defvar minimal-emacs/org-map (make-sparse-keymap) "key-map for org commands")
(defvar minimal-emacs/note-map (make-sparse-keymap) "key-map for note taking commands")
(defvar minimal-emacs/window-map (make-sparse-keymap) "key-map for window commands")
(defvar minimal-emacs/buffer-map (make-sparse-keymap) "key-map for buffer commands")
(defvar minimal-emacs/file-map (make-sparse-keymap) "key-map for file commands")
(defvar minimal-emacs/search-map (make-sparse-keymap) "key-map for searching")
(defvar minimal-emacs/goto-map (make-sparse-keymap) "key-map for going to places")
(defvar minimal-emacs/mc-map (make-sparse-keymap) "key-map for multiple cursors commands")
(defvar minimal-emacs/vc-map (make-sparse-keymap) "key-map for version control commands")
(defvar minimal-emacs/lsp-map (make-sparse-keymap) "key-map for language server protocol commands")
(defvar minimal-emacs/container-map (make-sparse-keymap) "key-map for container commands")
(defvar minimal-emacs/run-map (make-sparse-keymap) "key-map for running program specific commands")
(define-key mode-specific-map (kbd "o") (cons "org" minimal-emacs/org-map))
(define-key mode-specific-map (kbd "n") (cons "note" minimal-emacs/note-map))
(define-key mode-specific-map (kbd "w") (cons "window" minimal-emacs/window-map))
(define-key mode-specific-map (kbd "b") (cons "buffer" minimal-emacs/buffer-map))
(define-key mode-specific-map (kbd "f") (cons "file" minimal-emacs/file-map))
(define-key mode-specific-map (kbd "p") (cons "project" project-prefix-map))
(define-key mode-specific-map (kbd "s") (cons "search" minimal-emacs/search-map))
(define-key mode-specific-map (kbd "g") (cons "goto" minimal-emacs/goto-map))
(define-key mode-specific-map (kbd "m") (cons "multi" minimal-emacs/mc-map))
(define-key mode-specific-map (kbd "v") (cons "version control" minimal-emacs/vc-map))
(define-key mode-specific-map (kbd "l") (cons "langauge server protocol" minimal-emacs/lsp-map))
(define-key mode-specific-map (kbd "c") (cons "container" minimal-emacs/container-map))
(define-key mode-specific-map (kbd "r") (cons "run" minimal-emacs/run-map))

;; Custom Modes
(define-minor-mode sensitive-mode
  "https://anirudhsasikumar.net/blog/2005.01.21.html

  For sensitive files like password lists.
  It disables backup creation and auto saving.
  With no argument, this command toggles the mode.
  Non-null prefix argument turns on the mode.
  Null prefix argument turns off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Sensitive"
  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-mode)
      (progn
        ;; Disable backups
        (set (make-local-variable 'backup-inhibited) t)
        ;; Disable auto-save
        (if auto-save-default
            (auto-save-mode -1)))
    ;; Resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;; Resort to default auto save setting
    (if auto-save-default
        (auto-save-mode 1))))
