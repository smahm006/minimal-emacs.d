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
  :mode
  ("\\.c\\'" . c-ts-mode)
  ("\\.h\\'" . c-ts-mode)
  :bind (:map me/c-run-map ("r" . me/c-run) ("b" . me/c-build))
  :preface
  (defun me/c-project-root ()
    "Return the project root for the current C buffer."
    (or (locate-dominating-file buffer-file-name "Makefile")
        (locate-dominating-file buffer-file-name "CMakeLists.txt")
        default-directory))

  (defun me/c-run ()
    "Run the C project or current file."
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

  :config
  (me/enable-run-map c-ts-mode-map me/c-run-map)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(c-ts-mode . ,me/clangd-args))))

;;; cmake-mode — syntax support for CMakeLists.txt and .cmake files
(use-package cmake-mode
  :mode
  ("CMakeLists\\.txt\\'" . cmake-mode)
  ("\\.cmake\\'"         . cmake-mode))
