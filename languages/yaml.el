;;; yaml.el --- YAML customization  -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package yaml
  :ensure nil
  :mode (("\\.yml\\'"  . yaml-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode))
  :hook
  (yaml-ts-mode . (lambda ()
                    (define-key minimal-emacs/run-map (kbd "c") #'minimal-emacs/yaml-check)
                    (define-key minimal-emacs/run-map (kbd "f") #'minimal-emacs/yaml-format)))
  :preface
  (defun minimal-emacs/yaml-format ()
    "Format buffer using yamlfmt"
    (interactive)
    (let ((output (shell-command-to-string (format "yamlfmt -quiet %s" (shell-quote-argument buffer-file-name)))))
      (message "%s" (string-trim output)))
    (minimal-emacs/revert-buffer-no-confirm))
  (defun minimal-emacs/yaml-check ()
    "Compile current buffer file with yaml."
    (interactive)
    (compile (format "yamllint -f standard %s" (shell-quote-argument buffer-file-name))))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(yaml-ts-mode . ("yaml-language-server" "--stdio")))))

(use-package yaml-pro
  :hook
  (yaml-ts-mode . yaml-pro-ts-mode))
