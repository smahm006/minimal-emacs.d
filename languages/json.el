;;; json.el --- JSON customization  -*- no-byte-compile: t; lexical-binding: t; -*-
(use-package json
  :ensure nil
  :mode (("\\.json\\'"  . json-ts-mode))
  :hook
  (json-ts-mode . (lambda ()
                    (define-key me/run-map (kbd "f") #'me/json-format)))
  :preface
  (defun me/json-format ()
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
      (me/revert-buffer-no-confirm))))
