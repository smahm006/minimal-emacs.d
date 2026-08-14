2;;; rust.el --- Rust language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package rust
  :ensure nil
  :mode ("\\.rs\\'" . rust-ts-mode)
  :bind
  (:map me/rust-run-map
        ("r" . me/rust-run) ("c" . me/rust-check))
  :preface
  (defun me/rust-project-root ()
    "Return the Cargo project root for the current buffer."
    (me/project-root-or-error '("Cargo.toml")))

  (defun me/rust-run ()
    "Run the current Rust project with cargo run."
    (interactive)
    (let ((default-directory (me/rust-project-root)))
      (compile "cargo run")))

  (defun me/rust-check ()
    "Check the current Rust project with cargo clippy."
    (interactive)
    (let ((default-directory (me/rust-project-root)))
      (compile "cargo clippy")))

  :config
  (me/enable-run-map rust-ts-mode-map me/rust-run-map)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(rust-ts-mode . ("rust-analyzer"
                                   :initializationOptions
                                   (:check (:command "clippy")))))))
