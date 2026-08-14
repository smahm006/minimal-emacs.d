;;; bash.el --- Bash/shell language configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package bash
  :ensure nil
  :mode
  ("\\.sh\\'" . bash-ts-mode)
  ("\\.bash\\'" . bash-ts-mode)
  ("\\.bashrc\\'" . bash-ts-mode)
  ("\\.bash_profile\\'" . bash-ts-mode)
  :interpreter ("bash" . bash-ts-mode)
  :hook
  (bash-ts-mode . executable-make-buffer-file-executable-if-script-p)
  ;; Fallback hooks for sh-mode when bash-ts-mode is unavailable
  (sh-mode . executable-make-buffer-file-executable-if-script-p)

  :bind
  (:map me/bash-run-map
        ("r" . me/bash-run)
        ("c" . me/bash-check))
  :preface
  (defun me/bash-run ()
    "Run the current buffer with bash."
    (interactive)
    (let ((file (me/buffer-file-or-error)))
      (compile (format "bash %s" (shell-quote-argument file)))))
  (defun me/bash-check ()
    "Check the current buffer with shellcheck."
    (interactive)
    (let ((file (me/buffer-file-or-error)))
      (compile (format "shellcheck %s" (shell-quote-argument file)))))
  :config
  (me/enable-run-map bash-ts-mode-map me/bash-run-map)
  (me/enable-run-map sh-mode-map me/bash-run-map)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((bash-ts-mode sh-mode) . ("bash-language-server" "start")))))
