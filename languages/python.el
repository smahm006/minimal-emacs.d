;;; python.el --- Python configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package python
  :ensure nil
  :mode (("\\.py\\'" . python-ts-mode))
  :bind (:map me/python-run-map
              ("r" . me/python-run) ("c" . me/python-check)
              ("w" . me/python-fix) ("t" . me/python-test))
  :hook
  (python-ts-mode . (lambda ()
                      (local-set-key [remap backward-sexp] #'python-nav-backward-sexp-safe)
                      (local-set-key [remap forward-sexp] #'python-nav-forward-sexp-safe)))
  :preface
  (defun me/python-run ()
    "Run the file with uv or python3."
    (interactive)
    (let* ((file (me/buffer-file-or-error))
           (root (locate-dominating-file file "pyproject.toml"))
           (cmd (if root
                    "uv run python"
                  "python3")))
      (let ((default-directory (or root default-directory)))
        (compile (format "%s %s" cmd (shell-quote-argument file))))))
  (defun me/python-fix ()
    "Fix auto-fixable lint errors in-place using Ruff."
    (interactive)
    (let ((file (me/buffer-file-or-error)))
      (compile (format "ruff check --fix --color never %s"
                       (shell-quote-argument file)))))
  (defun me/python-check ()
    "Check the current buffer for lint errors using ruff."
    (interactive)
    (let ((file (me/buffer-file-or-error)))
      (compile (format "ruff check --color never %s"
                       (shell-quote-argument file)))))
  (defun me/python-test ()
    "Run the project's Python tests."
    (interactive)
    (let ((root (me/project-root-or-error '("pyproject.toml" "pytest.ini" "tox.ini"))))
      (let ((default-directory root)) (compile "python -m pytest"))))
  :config
  (me/enable-run-map python-ts-mode-map me/python-run-map)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(python-ts-mode . ("ty" "server")))))
