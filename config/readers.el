;;; readers.el --- Document and ebook reading -*- no-byte-compile: t; lexical-binding: t; -*-

;;; pdf-tools — superior PDF viewing and interaction
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :hook
  (pdf-view-mode . pdf-view-themed-minor-mode)
  (pdf-view-mode . pdf-isearch-minor-mode)
  :custom
  (pdf-view-use-scaling t)
  :config
  (pdf-tools-install :no-query))
