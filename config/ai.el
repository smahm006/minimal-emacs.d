;;; ai.el --- AI coding agents via ACP -*- no-byte-compile: t; lexical-binding: t; -*-

;;; agent-shell — LLM coding agents in a native Emacs buffer
;; Agents are driven over ACP (Agent Client Protocol) making it agent agnostic.
(use-package agent-shell
  :hook (agent-shell-viewport-edit-mode . visual-line-mode)
  :bind
  (:map me/ai-map
        ("a" . agent-shell)
        ("n" . agent-shell-new-shell)
        ("s" . agent-shell-switch-buffer)
        ("k" . agent-shell-interrupt)
        ("M" . agent-shell-set-session-model)
        ("m" . agent-shell-set-session-mode)
        ("r" . agent-shell-restart)
        ("A c" . agent-shell-anthropic-start-claude-code)
        ("A p" . agent-shell-pi-start-agent)
        ("A x" . agent-shell-openai-start-codex)
        ("A o" . agent-shell-opencode-start-agent)
        ("D c p" . me/claude-delete-project-sessions)
        ("D c a" . me/claude-delete-all-sessions))
  :preface
  (defconst me/claude-projects-directory (expand-file-name "~/.claude/projects/")
    "Directory for Claude project sessions.")

  (defun me/claude-project-sessions-directory ()
    "Return the Claude session directory for this project."
    (let ((root (directory-file-name
                 (expand-file-name
                  (or (when-let* ((proj (project-current)))
                        (project-root proj))
                      default-directory)))))
      (expand-file-name (replace-regexp-in-string "[^a-zA-Z0-9]" "-" root)
                        me/claude-projects-directory)))

  (defun me/claude--close-shells (buffers)
    "Kill agent shell BUFFERS and their ACP clients."
    (let ((kill-buffer-query-functions nil))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))))

  (defun me/claude-delete-project-sessions ()
    "Close this project's shells and delete its Claude sessions."
    (interactive)
    (let ((dir (me/claude-project-sessions-directory))
          (shells (when (fboundp 'agent-shell-project-buffers)
                    (agent-shell-project-buffers))))
      (unless (or shells (file-directory-p dir))
        (user-error "No Claude sessions for this project: %s" dir))
      (when (yes-or-no-p (format "Close %d shell(s) and delete %s? "
                                 (length shells) (abbreviate-file-name dir)))
        (me/claude--close-shells shells)
        (when (file-directory-p dir)
          (delete-directory dir t t))
        (message "Closed %d shell(s), deleted %s"
                 (length shells) (abbreviate-file-name dir)))))

  (defun me/claude-delete-all-sessions ()
    "Close all agent shells and delete all Claude sessions."
    (interactive)
    (let ((shells (when (fboundp 'agent-shell-buffers)
                    (agent-shell-buffers)))
          (projects (when (file-directory-p me/claude-projects-directory)
                      (directory-files me/claude-projects-directory
                                       nil directory-files-no-dot-files-regexp))))
      (unless (or shells projects)
        (user-error "No Claude sessions to delete"))
      (when (yes-or-no-p (format "Close %d shell(s) and delete sessions for all %d project(s)? "
                                 (length shells) (length projects)))
        (me/claude--close-shells shells)
        (when (file-directory-p me/claude-projects-directory)
          (delete-directory me/claude-projects-directory t t))
        (message "Closed %d shell(s), deleted sessions for %d project(s)"
                 (length shells) (length projects)))))
  :config
  ;; Only carry over an explicitly selected region. The default also picks up
  ;; the current line, buffer files and the error at point, which means `C-c a a'
  ;; silently pastes whatever point happened to be on into a fresh shell.
  (setopt agent-shell-context-sources '(region)))

;;; agent-shell-dashboard — one page listing every agent session
(use-package agent-shell-dashboard
  :ensure (:host github :repo "wandersoncferreira/agent-shell-dashboard")
  :bind
  (:map me/ai-map
        ("d" . agent-shell-dashboard))
  :custom
  ;; The default sub-line summariser shells out to `claude -p' per session on
  ;; every render. Use the raw tail of the last message instead.
  (agent-shell-dashboard-excerpt-function #'agent-shell-dashboard-excerpt-tail))
