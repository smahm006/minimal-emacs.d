;;; remote.el --- Remote access, vagrant and terminal -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Tramp — remote file editing over ssh/scp
(use-package tramp
  :ensure nil
  :config
  ;; Use scp for file transfers and direct async processes for speed.
  ;; See https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
  (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t)
  (setq tramp-copy-size-limit (* 1024 1024)  ; 1MB threshold for scp
        tramp-verbose 2)
  ;; Enable direct async processes for scp connections
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)
  ;; Fix magit over tramp
  (setq magit-tramp-pipe-stty-settings 'pty)
  ;; Fix remote compile — tramp disables ssh ControlMaster which breaks
  ;; compilation-mode; remove the hook that causes this.
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook
                 #'tramp-compile-disable-ssh-controlmaster-options)))

;;; Eat — terminal emulator
;; Eat (Emulate A Terminal) is a fast, feature-complete terminal emulator.
(use-package eat
  :bind
  ("C-x t" . eat)
  :config
  ;; Free up keys used for window switching and buffer fullscreen
  (define-key eat-semi-char-mode-map (kbd "M-o") nil)
  (define-key eat-semi-char-mode-map (kbd "M-u") nil))
