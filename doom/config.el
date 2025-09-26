;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Correctly add the local themes directory to the load-path
(add-load-path! "themes")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "jesus"
      user-mail-address "theaminishere@gmail.com")

;; It ensures all parts of Doom's UI use the same font consistently.
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 12))

;; Set your theme
(setq doom-theme 'doom-rose-pine)

;; Configure line numbers
(setq display-line-numbers-type t)

;; Set org-mode directories BEFORE the org module loads. This is correct.
(setq org-directory "~/notes/")
(setq org-roam-directory (file-truename "~/notes/roam/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CORRECTED AND SIMPLIFIED PACKAGE CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! org-roam
  :after org
  :custom
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode)
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook #'org-roam-mode)))


(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


(use-package! vterm
  :commands vterm)
