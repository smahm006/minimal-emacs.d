;;; vc.el --- Version Control customization  -*- no-byte-compile: t; lexical-binding: t; -*-

;; The magical git client
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
    "Stage only the current file, amend the last commit without editing, and force push with lease."
    (interactive)
    (let ((file (buffer-file-name)))
      (if file
          (progn
            (magit-run-git "add" file)
            (magit-run-git "commit" "--amend" "--no-edit")
            (magit-run-git "push" "--force-with-lease"))
        (message "No file associated with this buffer!"))))
  (defun me/magit-amend-all-and-push ()
    "Stage all unstaged files, amend the last commit without editing, and force push with lease."
    (interactive)
    (magit-stage-modified)
    (magit-run-git "commit" "--amend" "--no-edit")
    (magit-run-git "push" "--force-with-lease"))
  (defun me/open-on-github ()
    "Open the current file in GitHub."
    (interactive)
    (let* ((base-dir (vc-root-dir)) ; Get the Git root directory
           (repo-url (magit-git-string "remote" "get-url" "--push" "origin")) ; Get remote origin URL
           (branch-name (magit-git-string "rev-parse" "--abbrev-ref" "HEAD")) ; Get branch name
           ;; Get start and end line numbers
           (start-line (if (use-region-p)
                           (line-number-at-pos (region-beginning))
                         (line-number-at-pos)))
           (end-line (if (use-region-p) (line-number-at-pos (region-end))))
           ;; Calculate relative path
           (relative-path (if base-dir
                              (file-relative-name buffer-file-name base-dir)
                            (error "Could not determine project root")))
           ;; Convert SSH repo URL to HTTPS
           (https-repo-url (if (string-prefix-p "git@" repo-url)
                               (concat "https://"
                                       (replace-regexp-in-string
                                        ":" "/" (substring repo-url 4)))
                             repo-url))
           ;; Construct the final URL
           (github-url (concat
                        (substring https-repo-url 0 -4) ; Remove `.git` suffix
                        "/blob/"
                        branch-name
                        "/"
                        relative-path
                        "#L" (number-to-string start-line)
                        (if (and (use-region-p) (< 0 (- end-line start-line)))
                            (concat "..L" (number-to-string end-line)))))) ; Final URL
      (unless repo-url
        (error "No remote repository found"))
      (browse-url github-url))))

;; Emacs package for highlighting uncommitted changes.
(use-package diff-hl
  :hook
  ((find-file    . diff-hl-mode)
   (vc-dir-mode  . diff-hl-dir-mode)
   (dired-mode   . diff-hl-dired-mode)
   (diff-hl-mode . diff-hl-flydiff-mode)
   (magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :bind
  (:map me/vc-map
        ("g" . diff-hl-show-hunk)
        :repeat-map diff-hl-show-hunk-map
        ("n" . diff-hl-show-hunk-next)
        ("p" . diff-hl-show-hunk-previous)
        ("r" . diff-hl-revert-hunk)
        ("S" . diff-hl-stage-current-hunk)
        :exit
        ("C" . magit-commit-create))
  :custom
  ;; A slightly faster algorithm for diffing.
  (vc-git-diff-switches '("--histogram"))
  ;; Slightly more conservative delay before updating the diff
  (diff-hl-flydiff-delay 0.5)  ; default: 0.3
  ;; UX: get realtime feedback in diffs after staging/unstaging hunks.
  (diff-hl-show-staged-changes nil)
  :preface
  (defun me/diff-hl-inline-popup-show-adv (orig-func &rest args)
    (setcar (nthcdr 2 args) "")
    (apply orig-func args))
  (defun me/diff-hl-fix-face-colors (&rest _)
    "Set foreground to background color for diff-hl faces"
    (seq-do (lambda (face)
              (if-let ((color (face-background face)))
                  (progn (set-face-foreground face color)
                         (set-face-background face nil))))
            '(diff-hl-insert
              diff-hl-delete
              diff-hl-change)))
  :config
  (advice-add #'diff-hl-inline-popup-show :around #'me/diff-hl-inline-popup-show-adv)
  ;; UI: minimal fringe indicators
  ;; https://github.com/dgutov/diff-hl/issues/116#issuecomment-1573253134
  (let* ((width 2)
         (bitmap (vector (1- (expt 2 width)))))
    (define-fringe-bitmap 'me/diff-hl-bitmap bitmap 1 width '(top t)))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) 'me/diff-hl-bitmap))
  (me/diff-hl-fix-face-colors)
  (advice-add #'enable-theme :after #'me/diff-hl-fix-face-colors)
  (when (not (display-graphic-p))
    (diff-hl-margin-mode)))

;; Step through historic versions of git controlled file
(use-package git-timemachine
  :bind
  (:map me/vc-map
        ("t" . git-timemachine)))


;; Handle file differences
(use-package ediff
  :ensure nil
  :hook
  ((ediff-before-setup . me/store-pre-ediff-winconfig)
   (ediff-quit . me/restore-pre-ediff-winconfig))
  :preface
  (defvar minimal-emacs-ediff-original-windows nil)
  (defun me/store-pre-ediff-winconfig ()
    "This function stores the current window configuration before opening ediff."
    (setq me/ediff-original-windows (current-window-configuration)))
  (defun me/restore-pre-ediff-winconfig ()
    "This function resets the original window arrangement."
    (set-window-configuration me/ediff-original-windows))
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))
