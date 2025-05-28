;;; ux.el --- User Experience customization  -*- no-byte-compile: t; lexical-binding: t; -*-

;; Enable Smooth Scrolling
(setq pixel-scroll-precision-interpolate-mice nil)        ; Disable interpolation (causes wired jumps)
(setq pixel-scroll-precision-mode (display-graphic-p))    ; Enable pixel-wise scrolling
(setq pixel-scroll-precision-use-momentum t)              ; Enable momentum for scrolling lagre buffers

;; Backup settings
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'(lambda()
                               (let ((inhibit-message t))
                                 (recentf-mode 1))))
(add-hook 'kill-emacs-hook #'recentf-cleanup)
(setq backup-by-copying t)                              ; Backs up by copying instead of renaming
(setq vc-make-backup-files t)                           ; Back up version-controlled files
(setq version-control t)                                ; Enable versioned backups
(setq make-backup-files t)                              ; Make backups of saved files
(setq kept-new-versions 100)                            ; Keep 100 newest versions
(setq kept-old-versions 2)                              ; Keep 2 oldest versions
(setq delete-old-versions t)                            ; Delete excess backups
(setq delete-by-moving-to-trash t)                      ; Move backups to trash on delete
(setq auto-save-default t)                              ; Enable auto-saving
(setq auto-save-timeout 30)                             ; Seconds before auto-save triggers
(setq auto-save-interval 300)                           ; Keystrokes before auto-save triggers
(setq auto-save-list-file-prefix nil)                   ; Disable auto-save list files
(setq history-delete-duplicates t)                      ; Remove duplicates from history
(setq history-length 500)                               ; History length
(setq savehist-file (format "%s/emacs/history" xdg-cache))
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))

;; Define directories for backups and autosaves
(defvar backup-session-dir (format "%s/emacs/backups/session/" xdg-data))
(minimal-emacs/mkdir backup-session-dir)
(defvar backup-save-dir (format "%s/emacs/backups/save/" xdg-data))
(minimal-emacs/mkdir backup-save-dir)
(let ((auto-save-dir (format "%s/emacs/auto-save/" xdg-cache)))
  (minimal-emacs/mkdir auto-save-dir)
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t))))

;; Set where Emacs saves regular backups
(setq backup-directory-alist `(("." . ,backup-save-dir)))

;; Define the custom backup function
(defun minimal-emacs/backup-buffer ()
  "Make a session backup at the first save of each emacs session and a save backup on each subsequent save."
  (when (not buffer-backed-up)
    (let ((backup-directory-alist `(("." . ,backup-session-dir)))
          (kept-new-versions 3))
      (backup-buffer)))
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook #'minimal-emacs/backup-buffer)

;; Window Management
(use-package ace-window
  :ensure t
  :defer t
  :autoload ace-display-buffer
  :init
  (winner-mode)
  :bind
  (("M-o" . ace-window)
   ("M-O" . minimal-emacs/ace-window-prefix)
   ("M-u" . minimal-emacs/toggle-fullscreen-window)
   ([remap split-window-right] . minimal-emacs/hsplit-last-window)
   ([remap split-window-below] . minimal-emacs/vsplit-last-window)
   (:map minimal-emacs/window-map
         ("b" . balance-windows)
         ("c" . recenter-top-bottom)
         ("i" . enlarge-window)
         ("j" . shrink-window-horizontally)
         ("k" . shrink-window)
         ("u" . winner-undo)
         ("r" . winner-redo)
         ("l" . enlarge-window-horizontally)
         ("s" . switch-window-then-swap-buffer)
         ("-" . text-scale-decrease)
         ("+" . text-scale-increase)
         ("=" . (lambda () (interactive) (text-scale-increase 0)))))
  :preface
  (defun minimal-emacs/hsplit-last-window ()
    "Focus to the last created horizontal window."
    (interactive)
    (split-window-horizontally)
    (other-window 1))
  (defun minimal-emacs/vsplit-last-window ()
    "Focus to the last created vertical window."
    (interactive)
    (split-window-vertically)
    (other-window 1))
  (defun minimal-emacs/toggle-fullscreen-window ()
    "Toggle a buffer as fullscreen"
    (interactive)
    (if (= 1 (length (window-list)))
        (jump-to-register '_)
      (progn
        (window-configuration-to-register '_)
        (delete-other-windows))))
  (defun minimal-emacs/ace-window-prefix ()
    "https://karthinks.com/software/emacs-window-management-almanac/#a-window-prefix-command-for-ace-window"
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
         (setq
          window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
          type 'reuse)
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))
  :custom
  ;; Switch to minibuffer as well
  (aw-minibuffer-flag t)
  ;; Make Emacs ask where to place a new buffer
  (display-buffer-base-action
   '((display-buffer-reuse-window
      display-buffer-in-previous-window
      ace-display-buffer)))
  :custom-face
  (aw-leading-char-face ((t (:foreground "red" :weight bold :height 2.0)))))

;; Buffer Management
(use-package ibuffer
  :ensure nil
  :bind
  (:map ctl-x-map
   ("B" . minimal-emacs/switch-to-previous-buffer)
   :map minimal-emacs/buffer-map
   ("r" . minimal-emacs/rename-file-and-buffer)
   ("d" . minimal-emacs/delete-file-and-buffer)
   ("o" . minimal-emacs/kill-other-buffers))
  :init (minimal-emacs/protected-buffers)
  :preface
  (defvar protected-buffers '("*scratch*" "*Messages*"))
  (defun minimal-emacs/protected-buffers ()
    "Protect some buffers from being killed."
    (dolist (buffer protected-buffers)
      (with-current-buffer buffer
        (emacs-lock-mode 'kill))))
  (defun minimal-emacs/switch-to-previous-buffer ()
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))
  (defun minimal-emacs/rename-file-and-buffer ()
    "Rename the current buffer and file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (message "Buffer is not visiting a file!")
        (let ((new-name (read-file-name "New name: " filename)))
          (cond
           ((vc-backend filename) (vc-rename-file filename new-name))
           (t
            (rename-file filename new-name t)
            (set-visited-file-name new-name t t)))))))
  (defun minimal-emacs/delete-file-and-buffer ()
    "Kill the current buffer and deletes the file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (when filename
        (if (vc-backend filename)
            (vc-delete-file filename)
          (progn
            (delete-file filename)
            (message "Deleted file %s" filename)
            (kill-buffer))))))
  (defun minimal-emacs/kill-other-buffers ()
    "Kill other buffers except current one and protected buffers."
    (interactive)
    (eglot-shutdown-all)
    (mapc 'kill-buffer
          (cl-remove-if
           (lambda (x)
             (or
              (eq x (current-buffer))
              (member (buffer-name x) protected-buffers)))
           (buffer-list)))
    (delete-other-windows)))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;; Project Management
(use-package project
  :ensure nil
  :bind
  (:map ctl-x-map
   ("C-p" . project-find-file)
   :map project-prefix-map
   ("l" . project-list-buffers)
   ("F" . project-forget-project)
   ("S" . minimal-emacs/project-save-all-buffers)
   ("s" . project-search))
  :preface
  (defun minimal-emacs/project-save-all-buffers (&optional proj arg)
    "Save all file-visiting buffers in project without asking."
    (interactive)
    (let* ((proj (or proj (project-current)))
           (buffers (project-buffers (proj))))
      (dolist (buf buffers)
        ;; Act on base buffer of indirect buffers, if needed.
        (with-current-buffer (or (buffer-base-buffer buf) buf)
          (when (and (buffer-file-name buf)   ; Ignore all non-file-visiting buffers.
                     (buffer-modified-p buf)) ; Ignore all unchanged buffers.
            (let ((buffer-save-without-query t))  ; Save silently.
              (save-buffer arg)))))))
  :config
  (setq project-buffers-viewer 'project-list-buffers-ibuffer)
  (setq project-kill-buffers-display-buffer-list t)
  (setq project-switch-commands
        '((project-find-file "Find file" "f")
          (project-find-dir "Find dir" "d")
          (project-dired "Dired" "D")
          (consult-ripgrep "ripgrep" "r")
          (magit-project-status "Magit" "m")))
  (setq project-vc-extra-root-markers '(".project")))

;; File manager
(use-package dirvish
  :init
  (load-file (expand-file-name "elpa/dirvish-2.3.0/extensions/dirvish-yank.el" minimal-emacs-user-directory))
  (load-file (expand-file-name "elpa/dirvish-2.3.0/extensions/dirvish-quick-access.el" minimal-emacs-user-directory))
  (load-file (expand-file-name "elpa/dirvish-2.3.0/extensions/dirvish-collapse.el" minimal-emacs-user-directory))
  (load-file (expand-file-name "elpa/dirvish-2.3.0/extensions/dirvish-icons.el" minimal-emacs-user-directory))
  (load-file (expand-file-name "elpa/dirvish-2.3.0/extensions/dirvish-vc.el" minimal-emacs-user-directory))
  (load-file (expand-file-name "elpa/dirvish-2.3.0/extensions/dirvish-subtree.el" minimal-emacs-user-directory))
  (load-file (expand-file-name "elpa/dirvish-2.3.0/extensions/dirvish-side.el" minimal-emacs-user-directory))
  (dirvish-override-dired-mode)
  :bind
  ("<f1>" . dirvish-side)
  (:map minimal-emacs/file-map
        ("d" . dirvish)
        ("f" . dirvish-fd))
  (:map dirvish-mode-map
        ("F"   . dirvish-toggle-fullscreen)
        ("N"   . dirvish-narrow)
        ("M-f" . dirvish-history-go-forward)
        ("M-b" . dirvish-history-go-backward)
        ("M-p" . dired-up-directory)
        ("M-n" . dired-find-file)
        ("M-d" . empty-trash)
        ("M-l" . dirvish-ls-switches-menu)
        ("M-m" . dirvish-mark-menu)
        ("M-s" . dirvish-setup-menu)
        ("M-t" . dirvish-layout-toggle)
        ("a"   . dirvish-quick-access)
        ("s"   . dirvish-quicksort)
        ("f"   . dirvish-file-info-menu)
        ("y"   . dirvish-yank-menu)
        ("v"   . dirvish-vc-menu)
        ("^"   . dirvish-history-last)
        ("h"   . dirvish-history-jump)
        ("z"   . dirvish-show-history)
        ("TAB" . dirvish-subtree-toggle))
  :custom
  (dirvish-reuse-session nil)
  (dirvish-subtree-state-style 'nerd)
  (dirvish-use-header-line 'global)
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-attributes
   '(nerd-icons file-time file-size collapse subtree-state vc-state))
  (dirvish-path-separators (list
                            (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                            (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                            (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (dirvish-header-line-format
   '(:left (path) :right (free-space))
   dirvish-mode-line-format
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (vc-info yank index)))
  (dirvish-quick-access-entries
   '(("h" "~/"                                          "Home")
     ("d" "~/dump/"                                     "Downloads")
     ("w" "~/workstation"                               "Workstation")
     ("i" "~/media/image"                           "Images")
     ("m" "/mnt/"                                       "Drives")
     ("t" "~/.local/share/Trash/files/"                 "TrashCan")
     ("r" "/"                                           "Root")))
  :config
  (dirvish-side-follow-mode))
