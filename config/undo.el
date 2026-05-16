;;; undo.el --- Undo/redo configuration -*- no-byte-compile: t; lexical-binding: t; -*-

;;; undo-fu — sensible linear undo/redo
(use-package undo-fu
  :bind
  (([remap undo]      . undo-fu-only-undo)
   ([remap undo-redo] . undo-fu-only-redo))
  :custom
  (undo-fu-allow-undo-in-region t)
  (undo-limit        67108864)   ;  64MB
  (undo-strong-limit 100663296)  ;  96MB
  (undo-outer-limit  1006632960) ; 960MB
  )

;;; undo-fu-session — persist undo history across restarts
(use-package undo-fu-session
  :hook (elpaca-after-init . undo-fu-session-global-mode)
  :custom
  (undo-fu-session-directory
   (let ((dir (format "%s/emacs/undo-fu-session/" xdg-data)))
     (me/mkdir dir)
     dir))
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

;;; vundo — visual undo tree
(use-package vundo
  :bind
  (:map ctl-x-map
        ("u" . vundo))
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))
