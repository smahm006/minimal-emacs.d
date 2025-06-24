;;; container.el --- Virtualization and shell customization  -*- no-byte-compile: t; lexical-binding: t; -*-

;; Remote file editing through ssh/scp.
(use-package tramp
  :ensure nil
  :custom
  (tramp-verbose 0)
  (tramp-chunksize 2000)
  (tramp-histfile-override nil)
  (tramp-default-method "ssh")
  :config
  ;; Allows more dirvish features over tamp
  ;; Remove if casuing emacs to hang
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:")
                     "direct-async-process" t)))

;; Eat is the best terminal emulator which three modes to switch from
(use-package eat
  :bind
  ("C-x t" . eat)
  :config
  (define-key eat-semi-char-mode-map (kbd "M-o") nil)  ;; M-o is used to switch windows
  (define-key eat-semi-char-mode-map (kbd "M-u") nil)) ;; M-u is used to fullscreen buffer

;; Docker
(use-package docker
  :bind
  (:map me/container-map
        ("d c" . docker-containers)
        ("d i" . docker-images)
        ("d u" . docker-compose-up)
        ("d d" . docker-compose-down)
        ("d b" . docker-compose-build)
        ("d l" . docker-compose-logs)))

(use-package dockerfile-mode
  :mode ("/Dockerfile\\'" . dockerfile-ts-mode)
  :mode ("/Containerfile\\'" . dockerfile-ts-mode)
  :mode ("\\.dockerfile\\'" . dockerfile-ts-mode)
  :mode ("\\.containerfile\\'" . dockerfile-ts-mode))

;; Vagrant
(use-package vagrant
  :bind
  (:map me/container-map
        ("v s" . vagrant-status)
        ("v u" . vagrant-up)
        ("v d" . vagrant-halt)
        ("v k" . vagrant-destroy)
        ("v p" . vagrant-provision)))
(use-package vagrant-tramp)

;; Kubernetes
(use-package kubernetes
  :commands (kubernetes-overview)
  :bind
  (:map me/container-map
        ("k k" . kubernetes-overview))
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))
