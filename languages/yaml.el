;;; yaml.el --- YAML language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package yaml
  :ensure nil
  :mode
  ("\\.yml\\'"  . yaml-ts-mode)
  ("\\.yaml\\'" . yaml-ts-mode)
  :bind (:map me/yaml-run-map ("c" . me/yaml-check))
  :preface
  (defun me/yaml-check ()
    "Check the current buffer with yamllint."
    (interactive)
    (let ((file (me/buffer-file-or-error)))
      (compile (format "yamllint -f standard %s"
                       (shell-quote-argument file)))))
  :config
  (me/enable-run-map yaml-ts-mode-map me/yaml-run-map)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(yaml-ts-mode . ("yaml-language-server" "--stdio")))
    ;; Enable SchemaStore for automatic schema validation of common
    ;; config files (docker-compose, GitHub Actions, etc.)
    (setq eglot-workspace-configuration
          '(:yaml (:schemaStore (:enable t)
                   :schemas (:default t))))))
