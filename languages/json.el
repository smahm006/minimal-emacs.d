;;; json.el --- JSON language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package json
  :ensure nil
  :mode
  ("\\.json\\'"  . json-ts-mode)
  ("\\.jsonc\\'" . json-ts-mode)
  :hook
  (json-ts-mode . eglot-ensure)
  (json-ts-mode . (lambda ()
                    (define-key me/run-map (kbd "f") #'me/json-format)))
  :preface
  (defun me/json-format ()
    "Format the current buffer using apheleia (jq)."
    (interactive)
    (apheleia-format-buffer '(jq)))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(json-ts-mode . ("vscode-json-languageserver" "--stdio")))))
