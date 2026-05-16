;;; post-init.el --- Post-init customization -*- no-byte-compile: t; lexical-binding: t; -*-

(setq confirm-kill-emacs 'y-or-n-p)

;;; Byte/native compile installed packages on first load
(use-package compile-angel
  :demand t
  :config
  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

;;; Sync PATH and shell environment from the user's shell
;; Essential for finding LSP servers, formatters, Go tools, etc.
(use-package exec-path-from-shell
  :hook
  (elpaca-after-init . exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-variables '("PATH" "SHELL" "GOPATH")))

;;; GPG / file encryption support
;; Make emacs work with gui pinentry, note this requires setup of my emacs-tty-pinentry script
;; https://stackoverflow.com/questions/60812866/emacs-gpg-pinentry-el-for-authentication
(setq epg-gpg-program "gpg2")
(setq epg-pinentry-mode 'loopback)
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(use-package pinentry
  :init
  (pinentry-start)
  :config
  (setenv "SSH_AUTH_SOCK" (string-chop-newline (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")))
  (setenv "GPG_AGENT_INFO" nil))

;;; Load config modules
;; Order matters for a few dependencies (appearance before everything that
;; might reference faces; sessions early so savehist is active quickly).
(dolist (module '("appearance"
                  "sessions"
                  "minibuffer"
                  "completion"
                  "editing"
                  "undo"
                  "windows"
                  "navigation"
                  "files"
                  "lsp"
                  "vcs"
                  "remote"
                  "containers"
                  "org"
                  "readers"))
  (minimal-emacs-load-user-init (format "config/%s.el" module)))

(dolist (module '("python"
                  "golang"
                  "bash"
                  "java"
                  "rust"
                  "c"
                  "cpp"
                  "web"
                  "javascript"
                  "typescript"
                  "svelte"
                  "yaml"
                  "json"
                  "toml"
                  "tex"))
  (minimal-emacs-load-user-init (format "languages/%s.el" module)))
