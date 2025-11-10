;;; python.el --- Python customization  -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package python
  :ensure nil
  :mode (("\\.py\\'" . python-ts-mode))
  :hook
  (python-ts-mode . eglot-ensure)
  :hook
  (python-ts-mode . (lambda ()
                      (define-key me/run-map (kbd "v") #'me/pyrightconfig-write)
                      (define-key me/run-map (kbd "V") #'me/python-venv-setup)
                      (define-key me/run-map (kbd "r") #'me/python-run)
                      (define-key me/run-map (kbd "c") #'me/python-check)
                      (define-key me/run-map (kbd "f") #'me/python-format)
                      (define-key me/run-map (kbd "w") #'me/python-fix)
                      (local-set-key [remap backward-sexp] #'python-nav-backward-sexp-safe)
                      (local-set-key [remap forward-sexp] #'python-nav-forward-sexp-safe)))
  :preface
  (defun me/pyrightconfig-write (virtualenv)
    "Write a `pyrightconfig.json' file at the Git root of a project
         with `venvPath' and `venv' set to the absolute path of
         `virtualenv'. When run interactively, prompts for a directory."
    (interactive "DEnv: ")
    (let* ((venv-dir (if (featurep 'tramp)
                         (tramp-file-local-name (file-truename virtualenv))
                       (file-truename virtualenv)))
           (venv-file-name (directory-file-name venv-dir))
           (venvPath (file-name-directory venv-file-name))
           (venv (file-name-base venv-file-name))
           (base-dir (vc-git-root default-directory))
           (out-file (expand-file-name "pyrightconfig.json" base-dir))
           (out-contents (json-encode (list :venvPath venvPath :venv venv))))
      (with-temp-file out-file (insert out-contents))
      (message "Configured `%s` to use environment `%s`" out-file venv-dir)))
  (defun me/python-venv-setup ()
    "Install .pyvenv virtual environment at the root of the project.
         Additionally installed libraries from requirements.txt if it exists."
    (interactive)
    (let* ((base-dir (vc-git-root default-directory)) (venv-dir (concat base-dir ".venv")))
      (progn
        (save-window-excursion
          (shell-command (s-concat "python3 -m venv " venv-dir)pp)
          (when (file-exists-p (concat base-dir "requirements.txt"))
            (shell-command (s-concat "source " venv-dir "/bin/activate && pip3 install -r " base-dir "requirements.txt")))
          (me/pyrightconfig-write venv-dir)))
      (message (concat "Created " venv-dir))))
  (defun me/python-run ()
    "Compile current buffer file with python."
    (interactive)
    (compile (format "python3 %s" (shell-quote-argument buffer-file-name))))
  (defun me/python-format ()
    "Format buffer using ruff"
    (interactive)
    (let ((output (shell-command-to-string (format "ruff format %s" (shell-quote-argument buffer-file-name)))))
      (message "%s" (string-trim output)))
    (me/revert-buffer-no-confirm))
  (defun me/python-fix ()
    "Check for and fix any errors"
    (interactive)
    (let ((output (shell-command-to-string (format "ruff check --fix %s" (shell-quote-argument buffer-file-name)))))
      (message "%s" (string-trim output)))
    (me/revert-buffer-no-confirm))
  (defun me/python-check ()
    "Compile current buffer file with python."
    (interactive)
    (compile (format "ruff check %s" (shell-quote-argument buffer-file-name))))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(python-ts-mode . ("ty" "server")))))


(use-package sphinx-doc
  :hook
  (python-ts-mode . sphinx-doc-mode)
  (python-ts-mode . (lambda ()
                      (define-key me/run-map (kbd "d") #'sphinx-doc))))
