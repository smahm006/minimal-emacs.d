;;; zig.el --- Zig language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package zig-ts-mode
  :mode ("\\.zig\\'" . zig-ts-mode)
  :bind (:map me/zig-run-map
              ("r" . me/zig-run) ("b" . me/zig-build) ("t" . me/zig-test))
  :preface
  (defun me/zig-project-root ()
    "Return the Zig project root by locating build.zig."
    (or (locate-dominating-file buffer-file-name "build.zig")
        default-directory))

  (defun me/zig-run ()
    "Run the current Zig project with zig build run.
Falls back to zig run on the current file if no build.zig is found."
    (interactive)
    (let ((root (me/zig-project-root)))
      (if (file-exists-p (expand-file-name "build.zig" root))
          (let ((default-directory root))
            (compile "zig build run"))
        (compile (format "zig run %s"
                         (shell-quote-argument buffer-file-name))))))

  (defun me/zig-build ()
    "Build the current Zig project with zig build."
    (interactive)
    (let ((default-directory (me/zig-project-root)))
      (compile "zig build")))

  (defun me/zig-test ()
    "Run tests for the current Zig file or project."
    (interactive)
    (let ((root (me/zig-project-root)))
      (if (file-exists-p (expand-file-name "build.zig" root))
          (let ((default-directory root))
            (compile "zig build test"))
        (compile (format "zig test %s"
                         (shell-quote-argument buffer-file-name))))))

  :config
  (me/enable-run-map zig-ts-mode-map me/zig-run-map)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(zig-ts-mode . ("zls")))))
