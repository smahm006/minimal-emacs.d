;;; rust.el --- Rust customization  -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package rust
  :ensure nil
  :mode ("\\.rs\\'" . rust-ts-mode)
  :hook
  (rust-ts-mode . eglot-ensure)
  (rust-ts-mode . (lambda ()
                    (define-key minimal-emacs/run-map (kbd "r") #'minimal-emacs/rust-run)
                    (define-key minimal-emacs/run-map (kbd "c") #'minimal-emacs/rust-check)
                    (define-key minimal-emacs/run-map (kbd "f") #'minimal-emacs/rust-format)))
  :preface
  (defun minimal-emacs/rust-run ()
    "Compile current buffer file with rust."
    (interactive)
    (compile (format "rustc %s" buffer-file-name)))
  (defun minimal-emacs/rust-format ()
    "Format currenpt buffer file with rustimports."
    (interactive)
    (let ((output (shell-command-to-string
                   (format "rustfmt -q %s" (shell-quote-argument buffer-file-name)))))
      (message "%s" (string-trim output)))
    (minimal-emacs/revert-buffer-no-confirm))
  (defun minimal-emacs/rust-check ()
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
