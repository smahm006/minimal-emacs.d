;;; yaml.el --- YAML customization  -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package yaml
  :ensure nil
  :mode (("\\.yml\\'"  . yaml-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode))
  :hook
  (yaml-ts-mode . (lambda ()
                    (define-key me/run-map (kbd "c") #'me/yaml-check)
                    (define-key me/run-map (kbd "f") #'me/yaml-format)))
  :preface
  (defun me/yaml-format ()
    "Format buffer using yamlfmt"
    (interactive)
    (let ((output (shell-command-to-string (format "yamlfmt -quiet %s" (shell-quote-argument buffer-file-name)))))
      (message "%s" (string-trim output)))
    (me/revert-buffer-no-confirm))
  (defun me/yaml-check ()
    "Compile current buffer file with yaml."
    (interactive)
    (compile (format "yamllint -f standard %s" (shell-quote-argument buffer-file-name))))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(yaml-ts-mode . ("yaml-language-server" "--stdio")))))

(use-package yaml-pro
  :hook
  (yaml-ts-mode . yaml-pro-ts-mode)
  :custom
  (yaml-pro-ts-yank-subtrees nil))
