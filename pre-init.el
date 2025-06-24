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
(defun me/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun me/mkdir (dir-path)
  "Make directory in DIR-PATH if it doesn't exist."
  (unless (file-exists-p dir-path)
    (make-directory dir-path t)))

(defun me/location ()
  "Return 'home' if system-name starts with 'sm-', otherwise return 'work'."
  (if (string-match-p "^sm-" (system-name))
      "home"
    "work"))

;; Keymaps
(defvar me/org-map (make-sparse-keymap) "key-map for org commands")
(defvar me/note-map (make-sparse-keymap) "key-map for note taking commands")
(defvar me/window-map (make-sparse-keymap) "key-map for window commands")
(defvar me/buffer-map (make-sparse-keymap) "key-map for buffer commands")
(defvar me/file-map (make-sparse-keymap) "key-map for file commands")
(defvar me/search-map (make-sparse-keymap) "key-map for searching")
(defvar me/goto-map (make-sparse-keymap) "key-map for going to places")
(defvar me/mc-map (make-sparse-keymap) "key-map for multiple cursors commands")
(defvar me/vc-map (make-sparse-keymap) "key-map for version control commands")
(defvar me/lsp-map (make-sparse-keymap) "key-map for language server protocol commands")
(defvar me/container-map (make-sparse-keymap) "key-map for container commands")
(defvar me/run-map (make-sparse-keymap) "key-map for running program specific commands")
(define-key mode-specific-map (kbd "o") (cons "org" me/org-map))
(define-key mode-specific-map (kbd "n") (cons "note" me/note-map))
(define-key mode-specific-map (kbd "w") (cons "window" me/window-map))
(define-key mode-specific-map (kbd "b") (cons "buffer" me/buffer-map))
(define-key mode-specific-map (kbd "f") (cons "file" me/file-map))
(define-key mode-specific-map (kbd "p") (cons "project" project-prefix-map))
(define-key mode-specific-map (kbd "s") (cons "search" me/search-map))
(define-key mode-specific-map (kbd "g") (cons "goto" me/goto-map))
(define-key mode-specific-map (kbd "m") (cons "multi" me/mc-map))
(define-key mode-specific-map (kbd "v") (cons "version control" me/vc-map))
(define-key mode-specific-map (kbd "l") (cons "langauge server protocol" me/lsp-map))
(define-key mode-specific-map (kbd "c") (cons "container" me/container-map))
(define-key mode-specific-map (kbd "r") (cons "run" me/run-map))

;; Custom Modes
(define-minor-mode sensitive-mode
  "Minor mode for sensitive files like password lists.

Disables backups and auto-saving. With no argument, this command
toggles the mode. Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

See more from https://anirudhsasikumar.net/blog/2005.01.21.html"
  :init-value nil
  :lighter " Sensitive"
  :keymap nil
  (if sensitive-mode
      (progn
        ;; Disable backups
        (set (make-local-variable 'backup-inhibited) t)
        ;; Disable auto-save
        (when auto-save-default
          (auto-save-mode -1)))
    ;; Resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;; Restore auto-save if default enabled
    (when auto-save-default
      (auto-save-mode 1))))
