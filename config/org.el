;;; org.el --- Org mode configuration -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Org — the almighty organizer
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . visual-line-mode)
  :init
  (setq org-directory (format "%s/org/" xdg-home))
  (let ((org-archive-directory (format "%s/archives/" org-directory)))
    (me/mkdir org-archive-directory)
    (setq org-archive-location
          (format "%s/%%s::" org-archive-directory)))
  :bind
  (:map me/org-map
        ("c" . org-capture)
        ("a" . org-agenda))
  (:map org-mode-map
        ("M-<return>" . org-insert-heading-respect-content)
        ("C-<return>" . org-insert-item))
  :custom
  ;; Display
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-startup-truncated t)
  (org-fontify-done-headline t)
  (org-fontify-todo-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-ellipsis "…")
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  ;; Behaviour
  (org-return-follows-link t)
  (org-link-descriptive t)
  (org-enforce-todo-dependencies t)
  (org-log-done 'time)
  (org-confirm-babel-evaluate nil)
  ;; Source blocks
  (org-edit-src-content-indentation 0)
  (org-src-fontify-natively t)
  (org-src-window-setup 'current-window)
  (org-src-strip-leading-and-trailing-blank-lines t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t))

;;; org-modern — modernized org appearance
;; Replaces org-superstar with a more comprehensive visual overhaul
;; covering bullets, tables, keywords and source blocks.
(use-package org-modern
  :hook
  (org-mode        . org-modern-mode)
  (org-agenda-mode . org-modern-agenda)
  :custom
  (org-modern-star '("◉" "○" "✸" "✿"))
  (org-modern-list '((?* . "•") (?+ . "•") (?- . "•")))
  (org-modern-table t)
  (org-modern-keyword t)
  (org-modern-block-fringe t)
  (org-superstar-item-bullet-alist
   '((?* . ?•)
     (?+ . ?•)
     (?- . ?•))))

;;; org-appear — toggle visibility of hidden elements on cursor entry
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t))

;;; toc-org — auto-generate table of contents in org files
(use-package toc-org
  :hook (org-mode . toc-org-mode))

;;; Org agenda
(use-package org-agenda
  :ensure nil
  :bind
  (:map org-agenda-mode-map
        ("C-n" . org-agenda-next-item)
        ("C-p" . org-agenda-previous-item)
        ("g"   . org-agenda-goto))
  :config
  (let ((org-agenda-directory (format "%s/agenda/" org-directory)))
    (me/mkdir org-agenda-directory)
    (setq org-agenda-files (list org-agenda-directory))))
