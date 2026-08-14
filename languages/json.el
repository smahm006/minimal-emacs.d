;;; json.el --- JSON language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package json
  :ensure nil
  :mode
  ("\\.json\\'"  . json-ts-mode)
  ("\\.jsonc\\'" . json-ts-mode))
