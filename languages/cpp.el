;;; cpp.el --- C++ customization  -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package c++
  :ensure nil
  :mode (("\\.cc\\'" . c++-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode))
  :hook
  (c++-ts-mode . eglot-ensure)
  (c++-ts-mode . (lambda ()
                   (define-key minimal-emacs/run-map (kbd "r") #'minimal-emacs/c++-run)
                   (define-key minimal-emacs/run-map (kbd "c") #'minimal-emacs/c++-check)
                   (define-key minimal-emacs/run-map (kbd "f") #'minimal-emacs/c++-format)))
  :preface
  (defun minimal-emacs/c++-run ()
    "Compile current buffer file with c."
    (interactive)
    (compile (format "clang++ -Wall %s -o %s.out && %s.out"
                     (shell-quote-argument buffer-file-name)
                     (shell-quote-argument buffer-file-name)
                     (shell-quote-argument buffer-file-name))))
  (defun minimal-emacs/c++-format ()
    "Format current buffer file with clang-format."
    (interactive)
    (let ((output (shell-command-to-string
                   (format "clang-format -i %s" (shell-quote-argument buffer-file-name)))))
      (message "%s" (string-trim output)))
    (minimal-emacs/revert-buffer-no-confirm))
  (defun minimal-emacs/c++-check ()
    "Check current buffer file with clang-check"
    (interactive)
    (compile (format "clang-check %s" buffer-file-name)))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(c++-ts-mode . ("clangd"
                                  "-j=8"
                                  "--log=error"
                                  "--malloc++-trim"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--all-scopes-completion"
                                  "--completion-style=detailed"
                                  "--pch-storage=memory"
                                  "--header-insertion=never"
                                  "--header-insertion-decorators=0")))))
