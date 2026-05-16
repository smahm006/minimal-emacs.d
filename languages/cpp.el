;;; cpp.el --- C++ language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package c++
  :ensure nil
  :mode
  ("\\.cc\\'"  . c++-ts-mode)
  ("\\.cpp\\'" . c++-ts-mode)
  ("\\.cxx\\'" . c++-ts-mode)
  ("\\.hpp\\'" . c++-ts-mode)
  ("\\.hh\\'"  . c++-ts-mode)
  :hook
  (c++-ts-mode . eglot-ensure)
  (c++-ts-mode . (lambda ()
                   (define-key me/run-map (kbd "r") #'me/c++-run)
                   (define-key me/run-map (kbd "b") #'me/c++-build)
                   (define-key me/run-map (kbd "f") #'me/c++-format)))
  :preface
  (defun me/c++-project-root ()
    "Return the project root for the current C++ buffer."
    (or (locate-dominating-file buffer-file-name "Makefile")
        (locate-dominating-file buffer-file-name "CMakeLists.txt")
        default-directory))

  (defun me/c++-run ()
    "Run the current C++ file or project.
Uses make if a Makefile is found, otherwise compiles and runs the file directly."
    (interactive)
    (let ((root (me/c++-project-root)))
      (if (file-exists-p (expand-file-name "Makefile" root))
          (let ((default-directory root))
            (compile "make run"))
        (compile (format "clang++ -Wall %s -o %s.out && ./%s.out"
                         (shell-quote-argument buffer-file-name)
                         (shell-quote-argument buffer-file-name)
                         (shell-quote-argument buffer-file-name))))))

  (defun me/c++-build ()
    "Build the current C++ project using make."
    (interactive)
    (let ((default-directory (me/c++-project-root)))
      (compile "make")))

  (defun me/c++-format ()
    "Format the current buffer using apheleia (clang-format)."
    (interactive)
    (apheleia-format-buffer '(clang-format)))

  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(c++-ts-mode . ,me/clangd-args))))
