;;; python.el --- Python configuration -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package python
  :ensure nil
  :mode (("\\.py\\'" . python-ts-mode))
  :hook
  (python-ts-mode . eglot-ensure)
  (python-ts-mode . (lambda ()
                      (define-key me/run-map (kbd "r") #'me/python-run)
                      (define-key me/run-map (kbd "c") #'me/python-check)
                      (define-key me/run-map (kbd "f") #'me/python-format)
                      (define-key me/run-map (kbd "w") #'me/python-fix)))
  :preface
  (defun me/python-run ()
    "Run the current file using uv run.
If a uv-managed project is detected (pyproject.toml), uses `uv run python'.
Otherwise falls back to plain `python3'."
    (interactive)
    (let* ((root (locate-dominating-file buffer-file-name "pyproject.toml"))
           (cmd (if root
                    (let ((default-directory root))
                      (format "uv run python %s"
                              (shell-quote-argument buffer-file-name)))
                  (format "python3 %s"
                          (shell-quote-argument buffer-file-name)))))
      (compile cmd)))
  (defun me/python-format ()
    "Format the current buffer using apheleia (ruff-isort + ruff format)."
    (interactive)
    (apheleia-format-buffer '(ruff-isort ruff)))
  (defun me/python-fix ()
    "Fix auto-fixable lint errors in-place using apheleia (ruff check --fix)."
    (interactive)
    (apheleia-format-buffer '(ruff-fix)))
  (defun me/python-check ()
    "Check the current buffer for lint errors using ruff."
    (interactive)
    (compile (format "ruff check --color never %s"
                     (shell-quote-argument buffer-file-name))))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(python-ts-mode . ("ty" "server")))))

;;; pet — detect the correct Python executable for the current project
;; Supports uv, venv, conda, poetry and pipenv automatically.
;; Tells eglot and flymake which Python to use without manual config.
(use-package pet
  :hook
  (python-ts-mode . pet-mode)
  (python-ts-mode . me/python-setup-pet)
  :preface
  (defun me/python-setup-pet ()
    "Configure eglot and flyspell to use the pet-detected Python."
    (when (featurep 'eglot)
      (setq-local eglot-workspace-configuration
                  `(:python (:pythonPath ,(pet-executable-find "python")))))))

;;; pytest — run pytest from Emacs and navigate to failures
(use-package python-pytest
  :after python
  :hook
  (python-ts-mode . (lambda ()
                      (define-key me/run-map (kbd "t t") #'python-pytest-file)
                      (define-key me/run-map (kbd "t f") #'python-pytest-run-def-at-point-treesit)
                      (define-key me/run-map (kbd "t c") #'python-pytest-run-class-at-point-treesit)
                      (define-key me/run-map (kbd "t a") #'python-pytest)
                      (define-key me/run-map (kbd "t r") #'python-pytest-last-failed)))
  :custom
  (python-pytest-executable "uv run pytest"))

;;; pip-requirements — syntax support for requirements.txt files
(use-package pip-requirements
  :mode
  ("requirements\\(?:\\-.+\\)?\\.txt\\'" . pip-requirements-mode)
  ("requirements\\.in\\'"                . pip-requirements-mode))
