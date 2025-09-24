;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; -----------------------------
;; Fonts
;; -----------------------------
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))
(setq doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))
(setq doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 18))

;; -----------------------------
;; Theme
;; -----------------------------
(setq doom-theme 'doom-gruvbox)

;; -----------------------------
;; Line Numbers
;; -----------------------------
(setq display-line-numbers-type 'relative)

;; -----------------------------
;; Org & Org-roam
;; -----------------------------
(setq org-directory "~/notes/org/"
      org-roam-directory (file-truename "~/notes/roam/")
      org-roam-file-extensions '("org")
      org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

(setq org-roam-capture-templates
      '(("m" "main" plain "%?"
         :if-new (file+head "main/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new (file+head "reference/${title}.org" "#+title: ${title}\n")
         :immediate-finish t :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new (file+head "articles/${title}.org"
                            "#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t :unnarrowed t)
        ("d" "default" plain "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/notes/roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

;; -----------------------------
;; Org-roam UI (browser graph)
;; -----------------------------
(use-package! org-roam-ui
  :after org-roam
  :hook (org-roam-mode . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
