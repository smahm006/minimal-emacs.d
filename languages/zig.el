;;; zig.el --- Zig language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package zig-ts-mode
  :mode ("\\.zig\\'" . zig-ts-mode)
  :hook
  (zig-ts-mode . eglot-ensure)
  (zig-ts-mode . (lambda ()
                   (define-key me/run-map (kbd "r") #'me/zig-run)
                   (define-key me/run-map (kbd "b") #'me/zig-build)
                   (define-key me/run-map (kbd "t") #'me/zig-test)
                   (define-key me/run-map (kbd "f") #'me/zig-format)))
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

  (defun me/zig-format ()
    "Format the current buffer using apheleia (zig fmt)."
    (interactive)
    (apheleia-format-buffer '(zig-fmt)))

  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(zig-ts-mode . ("zls")))))
