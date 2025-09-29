;; ------------------------
;; 1. Theme & Fonts
;; ------------------------
(setq doom-theme 'doom-nord
      doom-font (font-spec :family "Hack Nerd Font" :size 14)
      doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 16))

;; ------------------------
;; 2. Org Headings Resize
;; ------------------------
(after! org
  ;; Resize Org headings
  (dolist (face '((org-level-1 . 1.35)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Source Sans Pro" :weight 'bold :height (cdr face)))

  ;; Document title
  (set-face-attribute 'org-document-title nil :font "Source Sans Pro" :weight 'bold :height 1.8))

;; ------------------------
;; 3. Prettified Symbols / Icons
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

(setq display-line-numbers-type 'relative
      display-line-numbers-width 3      ;; reserve enough space for double/triple digits
      display-line-numbers-width-start t)
(global-display-line-numbers-mode t)

(add-hook 'org-mode-hook #'my/prettify-symbols-setup)
(add-hook 'org-agenda-mode-hook #'my/prettify-symbols-setup)

;; ------------------------
;; 5. LaTeX Previews with org-fragtog
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
                 :message "you need to install latex and dvisvgm"
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

(use-package! exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; ------------------------
;; 6. Visual Enhancements
;; ------------------------
(add-hook 'org-mode-hook 'visual-line-mode)
