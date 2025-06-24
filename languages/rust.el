;;; rust.el --- Rust customization  -*- no-byte-compile: t; lexical-binding: t; -*-

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
  (defun me/rust-run ()
    "Compile current buffer file with rust."
    (interactive)
    (compile (format "rustc %s" buffer-file-name)))
  (defun me/rust-format ()
    "Format currenpt buffer file with rustimports."
    (interactive)
    (let ((output (shell-command-to-string
                   (format "rustfmt -q %s" (shell-quote-argument buffer-file-name)))))
      (message "%s" (string-trim output)))
    (me/revert-buffer-no-confirm))
  (defun me/rust-check ()
    "Check current buffer file with rustimports."
    (interactive)
    (compile (format "rustfmt --color never --check %s" buffer-file-name)))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(rust-ts-mode . ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))))


(use-package cargo-mode
  :hook
  (rust-ts-mode . cargo-minor-mode))
