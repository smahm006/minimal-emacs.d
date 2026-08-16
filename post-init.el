;;; post-init.el --- Post-init customization -*- no-byte-compile: t; lexical-binding: t; -*-

(setq confirm-kill-emacs 'y-or-n-p)

;;; Restart the Emacs daemon
;; Run the restart in a unit that survives the old service stopping.
(defconst me/restart-emacs-timeout 60
  "Seconds to wait for the daemon.")

(defun me/restart-emacs--script (sh systemctl timeout emacsclient frames)
  "Return a script that restarts Emacs and opens FRAMES frames."
  (concat
   (format "%s --user restart emacs.service || true\n" systemctl)
   ;; `-e t' proves the server socket is live, unlike unit state.
   (format "%s %d %s -c 'until %s -e t >/dev/null 2>&1; do sleep 1; done' || exit 1\n"
           timeout me/restart-emacs-timeout sh emacsclient)
   ;; -n so the helper exits instead of waiting on the frame.
   (mapconcat (lambda (_) (format "%s -c -n\n" emacsclient))
              (number-sequence 1 frames)
              "")))

(defun me/restart-emacs-service ()
  "Restart Emacs and reopen its graphical frames."
  (interactive)
  (let ((systemd-run (executable-find "systemd-run"))
        (systemctl (executable-find "systemctl"))
        (emacsclient (executable-find "emacsclient"))
        (sh (executable-find "sh"))
        (timeout (executable-find "timeout"))
        (frames (length (seq-filter #'display-graphic-p (frame-list))))
        (wayland-display (getenv "WAYLAND_DISPLAY")))
    (unless (and systemd-run systemctl)
      (user-error "No systemd-run/systemctl found; is this a systemd system?"))
    (unless (and emacsclient sh timeout)
      (user-error "Cannot find emacsclient, sh or timeout"))
    (when (y-or-n-p "Restart emacs.service (all frames will close)? ")
      (save-some-buffers t)
      (let ((status
             (apply #'call-process systemd-run nil nil nil
                    (append
                     (list "--user" "--collect" "--quiet"
                           (format "--unit=emacs-restart-%s"
                                   (format-time-string "%s")))
                     ;; Belt-and-braces: keeps the new frame on the right display.
                     (when wayland-display
                       (list (format "--setenv=WAYLAND_DISPLAY=%s"
                                     wayland-display)))
                     (list sh "-c"
                           (me/restart-emacs--script
                            sh systemctl timeout emacsclient frames))))))
        (unless (eq status 0)
          (user-error "systemd-run failed (exit %s)" status))))))

(keymap-global-set "C-x C-r" #'me/restart-emacs-service)

;;; Pin the interactive shell to zsh
;; The daemon gets bash from systemd. Use zsh instead.
(setq shell-file-name "/usr/bin/zsh")
(setq explicit-shell-file-name "/usr/bin/zsh")
(setenv "SHELL" shell-file-name)

;;; Sync PATH and shell environment from the user's shell
(use-package exec-path-from-shell
  :hook
  (elpaca-after-init . exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-shell-name "/usr/bin/zsh")
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-variables '("PATH" "GOPATH")))

;;; GnuPG / Yubikey integration
;; The daemon does not read zsh startup files.
(setenv "GNUPGHOME"
        (expand-file-name "gnupg" (or (getenv "XDG_CONFIG_HOME")
                                      (expand-file-name "~/.config"))))
(call-process "gpgconf" nil nil nil "--launch" "gpg-agent")
(setenv "SSH_AUTH_SOCK"
        (string-chop-newline (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")))
(setenv "GPG_AGENT_INFO" nil)

(setq epg-gpg-program "gpg2")
(setq epg-pinentry-mode nil)  ; override init.el; the card PIN goes through pinentry
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;;; Load config modules
;; Load appearance first and sessions early.
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
                  "ai"
                  "containers"
                  "org"
                  "readers"))
  (minimal-emacs-load-user-init (format "config/%s.el" module)))

(dolist (module '("python"
                  "go"
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
                  "tex"
                  "zig"
                  "markdown"))
  (minimal-emacs-load-user-init (format "languages/%s.el" module)))
