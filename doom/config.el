;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(add-load-path! "themes")
(setq doom-theme 'doom-rose-pine)
;; Set your name and email. Or don't. I don't care.
(setq user-full-name "jesus"
      user-mail-address "theaminishere@gmail.com")

;; This is the only directory that should matter to you.
(setq org-directory "~/notes")

;; that is actually installed on your system. Do not mess this up.
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))

(after! org
  ;; This variable controls the bullets for headlines.
  (setq org-superstar-headline-bullets-list '("•" "◆" "▲" "▶"))
  ;; This makes list bullets prettier too.
  (setq org-superstar-item-bullet-alist '((?+ . ?⋆) (?- . ?•))))
;; Line numbers. A comfort for a Vim user.

(use-package! org-modern :hook (org-mode . org-modern-mode))

(use-package! doom-modeline
  :config
  (setq doom-modeline-icon t              ; Display icons
        doom-modeline-major-mode-icon t   ; Display major mode icon
        doom-modeline-buffer-file-name-style 'truncate-upto-project)) ; Clean up file path

(setq display-line-numbers-type t)
