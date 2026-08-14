;;; vcs.el --- Version control -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Magit — git interface
(use-package magit
  :bind
  (:map ctl-x-map
        ("g" . magit-status))
  (:map me/vc-map
        ("f"  . magit-fetch)
        ("F"  . magit-fetch-all)
        ("p"  . magit-pull-branch)
        ("P"  . magit-push-current)
        ("b"  . magit-branch-or-checkout)
        ("c"  . magit-commit)
        ("a"  . me/magit-amend-file-and-push)
        ("A"  . me/magit-amend-all-and-push)
        ("d"  . magit-diff-unstaged)
        ("la" . magit-log-all)
        ("lc" . magit-log-current)
        ("lf" . magit-log-buffer-file)
        ("r"  . magit-rebase)
        ("o"  . me/open-on-github))
  :preface
  (defun me/magit-amend-file-and-push ()
    "Stage the current file, amend the last commit, and force-push with lease."
    (interactive)
    (let ((file (buffer-file-name)))
      (if file
          (progn
            (magit-run-git "add" file)
            (magit-run-git "commit" "--amend" "--no-edit")
            (magit-run-git "push" "--force-with-lease"))
        (message "No file associated with this buffer!"))))
  (defun me/magit-amend-all-and-push ()
    "Stage all modified files, amend the last commit, and force-push with lease."
    (interactive)
    (magit-stage-modified)
    (magit-run-git "commit" "--amend" "--no-edit")
    (magit-run-git "push" "--force-with-lease"))
  (defun me/open-on-github ()
    "Open the current file (and region if active) on GitHub."
    (interactive)
    (let* ((base-dir (vc-root-dir))
           (repo-url (magit-git-string "remote" "get-url" "--push" "origin"))
           (branch-name (magit-git-string "rev-parse" "--abbrev-ref" "HEAD"))
           (start-line (if (use-region-p)
                           (line-number-at-pos (region-beginning))
                         (line-number-at-pos)))
           (end-line (if (use-region-p) (line-number-at-pos (region-end))))
           (relative-path (if base-dir
                              (file-relative-name buffer-file-name base-dir)
                            (error "Could not determine project root")))
           (https-repo-url
            (if (string-prefix-p "git@" repo-url)
                (concat "https://"
                        (replace-regexp-in-string
                         ":" "/" (substring repo-url 4)))
              repo-url))
           (github-url
            (concat
             (substring https-repo-url 0 -4)
             "/blob/"
             branch-name
             "/"
             relative-path
             "#L" (number-to-string start-line)
             (when (and (use-region-p)
                        (< 0 (- end-line start-line)))
               (concat "..L" (number-to-string end-line))))))
      (unless repo-url
        (error "No remote repository found"))
      (browse-url github-url))))

;;; git-timemachine — step through a file's git history
(use-package git-timemachine
  :commands git-timemachine
  :bind
  (:map me/vc-map
        ("t" . git-timemachine)))

;;; git-modes — major modes for git config files
(use-package git-modes)

;;; Ediff — side-by-side file diffing
(use-package ediff
  :ensure nil
  :hook
  ((ediff-before-setup . me/store-pre-ediff-winconfig)
   (ediff-quit         . me/restore-pre-ediff-winconfig))
  :preface
  (defvar me/ediff-original-windows nil
    "Window configuration before ediff was opened.")
  (defun me/store-pre-ediff-winconfig ()
    "Store the window configuration before opening ediff."
    (setq me/ediff-original-windows (current-window-configuration)))
  (defun me/restore-pre-ediff-winconfig ()
    "Restore the window configuration after quitting ediff."
    (set-window-configuration me/ediff-original-windows))
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))
