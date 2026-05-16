;;; c.el --- C language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Shared clangd flags used by both c.el and cpp.el
(defvar me/clangd-args
  '("clangd"
    "-j=8"
    "--log=error"
    "--malloc-trim"
    "--background-index"
    "--clang-tidy"
    "--all-scopes-completion"
    "--completion-style=detailed"
    "--pch-storage=memory"
    "--header-insertion=never"
    "--header-insertion-decorators=0")
  "Arguments passed to clangd for both C and C++ buffers.")

(use-package c
  :ensure nil
  :hook
  (c-ts-mode . eglot-ensure)
  (c-ts-mode . (lambda ()
                 (define-key me/run-map (kbd "r") #'me/c-run)
                 (define-key me/run-map (kbd "b") #'me/c-build)
                 (define-key me/run-map (kbd "f") #'me/c-format)))
  :preface
  (defun me/c-project-root ()
    "Return the project root for the current C buffer."
    (or (locate-dominating-file buffer-file-name "Makefile")
        (locate-dominating-file buffer-file-name "CMakeLists.txt")
        default-directory))

  (defun me/c-run ()
    "Run the current C file or project.
Uses make if a Makefile is found, otherwise compiles and runs the file directly."
    (interactive)
    (let ((root (me/c-project-root)))
      (if (file-exists-p (expand-file-name "Makefile" root))
          (let ((default-directory root))
            (compile "make run"))
        (compile (format "clang -Wall %s -o %s.out && ./%s.out"
                         (shell-quote-argument buffer-file-name)
                         (shell-quote-argument buffer-file-name)
                         (shell-quote-argument buffer-file-name))))))

  (defun me/c-build ()
    "Build the current C project using make."
    (interactive)
    (let ((default-directory (me/c-project-root)))
      (compile "make")))

  (defun me/c-format ()
    "Format the current buffer using apheleia (clang-format)."
    (interactive)
    (apheleia-format-buffer '(clang-format)))

  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(c-ts-mode . ,me/clangd-args))))

;;; cmake-mode — syntax support for CMakeLists.txt and .cmake files
(use-package cmake-mode
  :mode
  ("CMakeLists\\.txt\\'" . cmake-mode)
  ("\\.cmake\\'"         . cmake-mode))
