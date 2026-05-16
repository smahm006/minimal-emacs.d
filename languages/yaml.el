;;; yaml.el --- YAML language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package yaml
  :ensure nil
  :mode
  ("\\.yml\\'"  . yaml-ts-mode)
  ("\\.yaml\\'" . yaml-ts-mode)
  :hook
  (yaml-ts-mode . eglot-ensure)
  (yaml-ts-mode . (lambda ()
                    (define-key me/run-map (kbd "c") #'me/yaml-check)
                    (define-key me/run-map (kbd "f") #'me/yaml-format)))
  :preface
  (defun me/yaml-format ()
    "Format the current buffer using apheleia (yamlfmt)."
    (interactive)
    (apheleia-format-buffer '(yamlfmt)))
  (defun me/yaml-check ()
    "Check the current buffer with yamllint."
    (interactive)
    (compile (format "yamllint -f standard %s"
                     (shell-quote-argument buffer-file-name))))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(yaml-ts-mode . ("yaml-language-server" "--stdio")))
    ;; Enable SchemaStore for automatic schema validation of common
    ;; config files (docker-compose, GitHub Actions, etc.)
    (setq eglot-workspace-configuration
          '(:yaml (:schemaStore (:enable t)
                   :schemas (:default t))))))

;;; flymake-yamllint — inline yamllint errors via flymake
(use-package flymake-yamllint
  :hook (yaml-ts-mode . flymake-yamllint-setup))

;;; yaml-imenu — imenu support for navigating YAML keys
;; Works with consult-imenu (C-c g i) for structured outline navigation.
(use-package yaml-imenu
  :hook (yaml-ts-mode . yaml-imenu-enable))
