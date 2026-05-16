;;; containers.el --- Docker and Kubernetes -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Docker — manage containers, images and compose from Emacs
(use-package docker
  :bind
  (:map me/container-map
        ("d c" . docker-containers)
        ("d i" . docker-images)
        ("d u" . docker-compose-up)
        ("d d" . docker-compose-down)
        ("d b" . docker-compose-build)
        ("d l" . docker-compose-logs)))

;;; dockerfile-mode — syntax support for Dockerfiles and Containerfiles
(use-package dockerfile-mode
  :mode
  ("/Dockerfile\\'"     . dockerfile-ts-mode)
  ("/Containerfile\\'"  . dockerfile-ts-mode)
  ("\\.dockerfile\\'"   . dockerfile-ts-mode)
  ("\\.containerfile\\'" . dockerfile-ts-mode))

;;; Kubernetes — manage k8s resources from Emacs
(use-package kubernetes
  :commands (kubernetes-overview)
  :bind
  (:map me/container-map
        ("k k" . kubernetes-overview))
  :custom
  ;; Poll infrequently to avoid hammering the API server
  (kubernetes-poll-frequency 3600)
  (kubernetes-redraw-frequency 3600))

;;; Vagrant — manage Vagrant VMs from Emacs
(use-package vagrant
  :bind
  (:map me/container-map
        ("v s" . vagrant-status)
        ("v u" . vagrant-up)
        ("v d" . vagrant-halt)
        ("v k" . vagrant-destroy)
        ("v p" . vagrant-provision)))

;;; vagrant-tramp — open files on Vagrant VMs via tramp
(use-package vagrant-tramp
  :after (tramp vagrant))
