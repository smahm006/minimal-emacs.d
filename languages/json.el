;;; json.el --- JSON customization  -*- no-byte-compile: t; lexical-binding: t; -*-
(use-package json
  :ensure nil
  :mode (("\\.json\\'"  . json-ts-mode))
  :hook
  (json-ts-mode . (lambda ()
                    (define-key minimal-emacs/run-map (kbd "f") #'minimal-emacs/json-format)))
  :preface
  (defun minimal-emacs/json-format ()
    "Format the current buffer as JSON using jq if available, else python -m json.tool."
    (interactive)
    (let* ((jq-exists (executable-find "jq"))
           (cmd (if jq-exists
                    (format "out=$(jq . < %s) && echo \"$out\" > %s"
                            (shell-quote-argument buffer-file-name)
                            (shell-quote-argument buffer-file-name))
                  (format "out=$(python -m json.tool %s) && echo \"$out\" > %s"
                          (shell-quote-argument buffer-file-name)
                          (shell-quote-argument buffer-file-name))))
           (output (shell-command cmd)))
      (message "%s" output)
      (minimal-emacs/revert-buffer-no-confirm))))
