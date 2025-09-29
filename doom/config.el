;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; --- Appearance & Basics ---
(setq doom-theme 'doom-one
      user-full-name "jesus"
      user-mail-address "theaminishere@gmail.com"
      org-directory "~/notes"
      doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
      display-line-numbers-type 'relative)

(use-package! org-modern
  :hook (org-mode . org-modern-mode))

(use-package! doom-modeline
  :config
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project))

;; --- Ensure Emacs sees LaTeX binaries ---
(setenv "PATH" (concat "/usr/sbin:" (getenv "PATH")))
(setq exec-path (append '("/usr/sbin") exec-path))

;; --- Org Fragtog: automatic fragment toggle ---
(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))
