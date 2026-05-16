2;;; rust.el --- Rust language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package rust
  :ensure nil
  :mode ("\\.rs\\'" . rust-ts-mode)
  :hook
  (rust-ts-mode . eglot-ensure)
  (rust-ts-mode . (lambda ()
                    (define-key me/run-map (kbd "r") #'me/rust-run)
                    (define-key me/run-map (kbd "c") #'me/rust-check)
                    (define-key me/run-map (kbd "f") #'me/rust-format)))
  :preface
  (defun me/rust-project-root ()
    "Return the Cargo project root for the current buffer."
    (or (locate-dominating-file buffer-file-name "Cargo.toml")
        default-directory))

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

  (defun me/rust-format ()
    "Format the current buffer using apheleia (rustfmt)."
    (interactive)
    (apheleia-format-buffer '(rustfmt)))

  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(rust-ts-mode . ("rust-analyzer"
                                   :initializationOptions
                                   (:check (:command "clippy")))))))

;;; cargo-mode — run Cargo commands from Emacs
(use-package cargo-mode
  :hook (rust-ts-mode . cargo-minor-mode))
