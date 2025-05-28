;;; apps.el --- Applications customization  -*- no-byte-compile: t; lexical-binding: t; -*-

;; PDF support
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :functions (pdf-view-refresh-themed-buffer)
  :custom
  (pdf-view-use-scaling t)
  :config
  (pdf-tools-install :no-query)
  :hook
  ((pdf-view-mode . pdf-view-themed-minor-mode)
   (pdf-view-mode . pdf-isearch-minor-mode)))
