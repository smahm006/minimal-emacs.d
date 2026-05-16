;;; tex.el --- LaTeX configuration -*- no-byte-compile: t; lexical-binding: t; -*-

;; Required system packages:
;; texlive-latex-base
;; texlive-latex-recommended
;; texlive-fonts-recommended
;; texlive-latex-extra
;; texlive-bibtex-extra (if using citations)
;; texlive-lang-(yourlang) (for non-English documents)

(use-package auctex
  :hook
  ((LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . TeX-source-correlate-mode)
   (LaTeX-mode . turn-on-reftex)
   (LaTeX-mode . (lambda ()
                   (define-key me/run-map (kbd "r") #'me/tex-compile)
                   (define-key me/run-map (kbd "v") #'me/tex-view))))
  :preface
  (defun me/tex-compile ()
    "Compile the current LaTeX document."
    (interactive)
    (TeX-command "LaTeX" #'TeX-master-file))

  (defun me/tex-view ()
    "View the compiled PDF output."
    (interactive)
    (TeX-command "View" #'TeX-master-file))

  :custom
  (TeX-view-program-selection
   '(((output-dvi has-no-display-manager) "dvi2tty")
     ((output-dvi style-pstricks)         "dvips and gv")
     (output-dvi                          "xdvi")
     (output-pdf                          "PDF Tools")
     (output-html                         "xdg-open")))
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-clean-confirm nil)
  (TeX-save-query nil)
  (TeX-master nil)
  (TeX-PDF-mode t)
  (TeX-source-correlate-start-server t))  ; enable inverse search with pdf-tools
