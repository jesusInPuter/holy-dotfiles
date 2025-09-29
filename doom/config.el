;; ------------------------
;; 0. Frame Transparency
;; ------------------------
(add-to-list 'default-frame-alist '(alpha . (0.9 . 0.9))) ; all new frames
(set-frame-parameter (selected-frame) 'alpha '(0.9 . 0.9)) ; current frame

;; ------------------------
;; 1. Theme & Fonts
;; ------------------------
(setq doom-theme 'doom-gruvbox
      doom-font (font-spec :family "FiraCode Nerd Font" :size 16)
      doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 16))

;; ------------------------
;; 2. Org Directory Setup
;; ------------------------
(setq org-directory "~/notes")                           ; root org files
(setq org-roam-directory (file-truename "~/notes/roam")) ; Org-roam nodes
(setq my-notes-directory "~/notes")                      ; optional default notes dir

;; ------------------------
;; 3. Org Headings Resize & Org-modern
;; ------------------------
(after! org
  (dolist (face '((org-level-1 . 1.35)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Source Sans Pro" :weight 'bold :height (cdr face)))
  (set-face-attribute 'org-document-title nil :font "Source Sans Pro" :weight 'bold :height 1.8))

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "◆" "▷" "•" "‣"))
  (setq org-modern-hide-stars nil)
  (setq org-modern-todo nil))

;; ------------------------
;; 4. Prettified Symbols / Icons
;; ------------------------
(defun my/prettify-symbols-setup ()
  (push '("[ ]" . "") prettify-symbols-alist)
  (push '("[X]" . "") prettify-symbols-alist)
  (push '("[-]" . "") prettify-symbols-alist)
  (push '("#+BEGIN_SRC" . ?≫) prettify-symbols-alist)
  (push '("#+END_SRC" . ?≫) prettify-symbols-alist)
  (push '(":PROPERTIES:" . "") prettify-symbols-alist)
  (push '(":projects:" . "") prettify-symbols-alist)
  (push '(":work:"     . "") prettify-symbols-alist)
  (push '(":inbox:"    . "") prettify-symbols-alist)
  (push '(":task:"     . "") prettify-symbols-alist)
  (push '(":emacs:"    . "") prettify-symbols-alist)
  (prettify-symbols-mode))

(add-hook 'org-mode-hook #'my/prettify-symbols-setup)
(add-hook 'org-agenda-mode-hook #'my/prettify-symbols-setup)

;; ------------------------
;; 5. Display Line Numbers
;; ------------------------
(setq display-line-numbers-type 'relative
      display-line-numbers-width 3
      display-line-numbers-width-start t)
(global-display-line-numbers-mode t)

;; ------------------------
;; 6. Org-roam v2
;; ------------------------
(use-package! org-roam
  :after org
  :init
  (setq org-roam-directory (file-truename "~/notes/roam"))
  :custom
  (org-roam-v2-ack t)
  :config
  (org-roam-db-autosync-mode))

(setq org-roam-capture-templates
      '(("d" "default" plain
         "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t
         :hook (lambda ()
                 ;; Open capture in vertical split on right
                 (let ((new-win (split-window-right)))
                   (set-window-buffer new-win (current-buffer))
                   (select-window new-win))))))

;; ------------------------
;; 7. LaTeX Previews with org-fragtog
;; ------------------------
(setq org-preview-latex-default-process 'dvisvgm)
(setq org-preview-latex-process-alist
      '((dvipng :programs ("latex" "dvipng")
                :description "dvi -> png"
                :message "you need to install latex and dvipng"
                :use-xcolor t
                :image-input-type "dvi"
                :image-output-type "png"
                :image-size-adjust (1.0 . 1.0)
                :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f"))
        (dvisvgm :programs ("latex" "dvisvgm")
                 :description "dvi -> svg"
                 :message "you need to install dvisvgm"
                 :use-xcolor t
                 :image-input-type "dvi"
                 :image-output-type "svg"
                 :image-size-adjust (1.7 . 1.5)
                 :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f"))
        (imagemagick :programs ("latex" "convert")
                     :description "pdf -> png"
                     :message "you need to install imagemagick"
                     :use-xcolor t
                     :image-input-type "pdf"
                     :image-output-type "png"
                     :image-size-adjust (1.0 . 1.0)
                     :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f"))))

(use-package! org-fragtog
  :hook (org-mode-hook . org-fragtog-mode))

;; ------------------------
;; 8. Exec Path
;; ------------------------
(use-package! exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; ------------------------
;; 9. Visual Enhancements
;; ------------------------
(add-hook 'org-mode-hook 'visual-line-mode)
