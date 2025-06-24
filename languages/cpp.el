;;; cpp.el --- C++ customization  -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package c++
  :ensure nil
  :mode (("\\.cc\\'" . c++-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode))
  :hook
  (c++-ts-mode . eglot-ensure)
  (c++-ts-mode . (lambda ()
                   (define-key me/run-map (kbd "r") #'me/c++-run)
                   (define-key me/run-map (kbd "c") #'me/c++-check)
                   (define-key me/run-map (kbd "f") #'me/c++-format)))
  :preface
  (defun me/c++-run ()
    "Compile current buffer file with c."
    (interactive)
    (compile (format "clang++ -Wall %s -o %s.out && %s.out"
                     (shell-quote-argument buffer-file-name)
                     (shell-quote-argument buffer-file-name)
                     (shell-quote-argument buffer-file-name))))
  (defun me/c++-format ()
    "Format current buffer file with clang-format."
    (interactive)
    (let ((output (shell-command-to-string
                   (format "clang-format -i %s" (shell-quote-argument buffer-file-name)))))
      (message "%s" (string-trim output)))
    (me/revert-buffer-no-confirm))
  (defun me/c++-check ()
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
