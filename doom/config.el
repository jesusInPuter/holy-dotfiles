(add-to-list 'exec-path "/usr/sbin")
;; ------------------------
;; 0. Frame Transparency
;; ------------------------
(add-to-list 'default-frame-alist '(alpha . (0.8 . 0.8))) ; all new frames
(set-frame-parameter (selected-frame) 'alpha '(0.8 . 0.8)) ; current frame

;; ------------------------
;; 1. Theme & Fonts
;; ------------------------
(setq doom-theme 'doom-gruvbox
      doom-font (font-spec :family "FiraCode Nerd Font" :size 19)
      doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 17))

;; ------------------------
;; Org Babel
;; ------------------------
(org-babel-do-load-languages
 'org-babel-load-languages
 '((latex . t)
   (gnuplot . t)
   (python . t)))

(setq org-confirm-babel-evaluate nil)

;; ------------------------
;; 2. Org Directory Setup
;; ------------------------

(setq org-directory "~/notes")                           ; root org files
(setq org-roam-directory (file-truename "~/notes/roam")) ; Org-roam nodes
(setq my-notes-directory "~/notes")                      ; optional default notes dir

(setq org-attach-use-id-dir t)
(setq org-attach-id-dir "~/notes/assets/")

(use-package! org-download
  :after org
  :config
  (setq org-download-method 'attach)
  (setq org-download-heading-lvl nil)
  (setq org-download-timestamp "%Y%m%d-%H%M%S_")

  (map! :map org-mode-map
        :leader
        :desc "Paste image from clipboard" "i p" #'org-download-clipboard
        :leader
        :desc "Yank image from URL"      "i y" #'org-download-yank))

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


(after! org
  (add-to-list 'org-latex-default-packages-alist '("" "tikz")))
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
         :unnarrowed t)))

; ;; ------------------------
; ;; 7. LaTeX Previews with org-fragtog (direct load, bypass .elc curse)
; ;; ------------------------
;; ------------------------
;; Org LaTeX Preview Setup
;; ------------------------
(after! org
  ;; Load required packages for LaTeX export
  (add-to-list 'org-latex-default-packages-alist '("" "tikz" t))
  (add-to-list 'org-latex-default-packages-alist '("" "pgfplots" t))
  (add-to-list 'org-latex-default-packages-alist '("" "amsmath" t))

  ;; Use imagemagick to preview LaTeX fragments (works with xelatex/pdf)
  (setq org-preview-latex-default-process 'imagemagick)
  (setq org-latex-compiler "xelatex")

  ;; LaTeX PDF process for export
  (setq org-latex-pdf-process
        '("xelatex -interaction=nonstopmode -output-directory=%o %f"
          "xelatex -interaction=nonstopmode -output-directory=%o %f"))

  ;; Org-fragtog for auto preview on insert
  (add-hook 'org-mode-hook 'org-fragtog-mode)

  ;; Scale LaTeX previews relative to headings
  (setq org-format-latex-options
        '(:foreground default
          :background default
          :scale 1.3
          :html-foreground "Black"
          :html-background "Transparent"
          :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))
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
(dirvish-override-dired-mode)
(setq lsp-auto-guess-root t)
(after! lsp-mode
  (add-hook! 'c-mode-common-hook 'lsp-format-on-save-mode))
(set-popup-rule! "^\\*Capture\\*" :side 'right :size 0.5 :select t)

;;==================================================
;; CUSTOM 
;;==================================================
(map! :leader
      :desc "Org-download: Insert image from clipboard"
      "i p" #'org-download-clipboard)
(defun wrap-region-in-src-block (lang)
  "Wrap the active region in an Org-mode #+BEGIN_SRC ... #+END_SRC block."
  (interactive "sLanguage: ")
  (let ((beg (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert (concat "\n#+END_SRC\n"))
    (goto-char beg)
    (insert (concat "#+BEGIN_SRC " lang "\n"))))
(global-set-key (kbd "C-c w") 'wrap-region-in-src-block)

(defun my/org-set-image-width (width)
  "Prompt for a width and apply it to the org-mode image at point.
This version correctly handles attribute insertion without extra newlines."
  (interactive "nWidth: ")
  (save-excursion
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      (unless (eq type 'link)
        (user-error "Not on an link"))
      (unless (string-match-p "\\.\\(png\\|jpe?g\\|gif\\|svg\\|webp\\)$" (org-element-property :path context))
        (user-error "Link at point is not an image"))

      (beginning-of-line)
      (if (and (re-search-backward "^#\\+ATTR_ORG:" (line-beginning-position) t)
               (= (1+ (line-number-at-pos)) (line-number-at-pos (point-max))))
          (if (string-match ":width [0-9]+" (match-string 0))
              (replace-match (format ":width %d" width) t t nil 1)
            (goto-end-of-line)
            (insert (format " :width %d" width)))
        (progn
          (goto-char (org-element-property :begin context))
          (beginning-of-line)
          (insert (format "#+ATTR_ORG: :width %d\n" width)))))))

(map! :leader
      :after org
      :desc "Set Org image width" "i w" #'my/org-set-image-width)
