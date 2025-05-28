;;; tex.el --- LaTex customization  -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package auctex
  ;; Requires the following packages
  ;; texlive-latex-base
  ;; texlive-latex-recommended
  ;; texlive-fonts-recommended
  ;; texlive-latex-extra
  ;; texlive-bibtex-extra (if using citations)
  ;; texlive-lang-(yourlang) (for any language you're writing documents in, except English)
  :hook
  ((LaTeX-mode . LaTeX-preview-setup)
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . TeX-source-correlate-mode)
   (LaTeX-mode . turn-on-reftex))
  :custom
  ;; Use PDF Tools for pdf output
  (TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
     (output-html "xdg-open")))
  (TeX-auto-save t)                          ; Enable auto-saving of TeX files
  (TeX-parse-self t)                         ; Enable parsing of the current TeX file
  (TeX-clean-confirm nil)                    ; Disable confirmation when cleaning
  (TeX-clean-intermediate t)                 ; Clean intermediate files
  (TeX-save-query nil)                       ; Disable query prompts when saving TeX files
  (TeX-master nil)                           ; Ask for master document
  (TeX-PDF-mode t)                           ; Enable PDF mode for TeX files
  (TeX-source-correlate-start-server nil))   ; Don't start server for inverse search (is already running)
