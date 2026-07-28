;;; markdown.el --- Markdown configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package markdown-ts-mode
  :ensure nil
  :mode
  ("\\.md\\'"       . markdown-ts-mode)
  ("\\.markdown\\'" . markdown-ts-mode)
  :hook
  (markdown-ts-mode . visual-line-mode))
