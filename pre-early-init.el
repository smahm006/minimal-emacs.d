;;; pre-early-init.el --- Early init customization -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Set debug mode on/off
(setq debug-on-error t)

;;; Package manager
;; Disable the built-in package manager since Elpaca will replace it.
(setq minimal-emacs-package-initialize-and-refresh nil)

;;; XDG Base Directory paths
(defvar xdg-home (or (getenv "HOME") "~"))
(defvar xdg-data (or (getenv "XDG_DATA_HOME") (expand-file-name "~/.local/share")))
(defvar xdg-config (or (getenv "XDG_CONFIG_HOME") (expand-file-name "~/.config")))
(defvar xdg-cache (or (getenv "XDG_CACHE_HOME") (expand-file-name "~/.cache")))

;;; Native compilation cache
;; Redirect the eln-cache to XDG_CACHE_HOME to keep config folder clean.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name "emacs/eln-cache/" xdg-cache)))

;;; Change the default emacs configuration dircetory to avoid cluttering main
(setq minimal-emacs-user-directory user-emacs-directory)
(setq minimal-emacs-var-dir (expand-file-name "var/" minimal-emacs-user-directory))
(setq user-emacs-directory minimal-emacs-var-dir)
