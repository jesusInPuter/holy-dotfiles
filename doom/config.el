;;;=============================================================================
;;; Core UI & Frame Configuration
;;;=============================================================================
(setq doom-theme 'doom-gruvbox
      doom-font (font-spec :family "FiraCode Nerd Font" :size 19)
      doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 17))

;; Frame transparency
(add-to-list 'default-frame-alist '(alpha . (0.8 . 0.8)))
(set-frame-parameter (selected-frame) 'alpha '(0.8 . 0.8))

;; Relative line numbers
(setq display-line-numbers-type 'relative
      display-line-numbers-width 3
      display-line-numbers-width-start t)
(global-display-line-numbers-mode t)

;; Inherit shell PATH
(use-package! exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;;=============================================================================
;;; Org Mode Setup
;;;=============================================================================
(setq org-directory "~/notes")

(after! org
  ;; Org Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((latex . t)
     (gnuplot . t)
     (python . t)))
  (setq org-confirm-babel-evaluate nil)

  ;; LaTeX support
  (add-to-list 'org-latex-default-packages-alist '("" "tikz" t))
  (add-to-list 'org-latex-default-packages-alist '("" "pgfplots" t))
  (add-to-list 'org-latex-default-packages-alist '("" "amsmath" t))
  (setq org-latex-compiler "xelatex"
        org-latex-pdf-process
        '("xelatex -interaction=nonstopmode -output-directory=%o %f"
          "xelatex -interaction=nonstopmode -output-directory=%o %f")
        org-preview-latex-default-process 'imagemagick
        org-format-latex-options
        '(:foreground default :background default :scale 1.3
          :html-foreground "Black" :html-background "Transparent"
          :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

  ;; Auto LaTeX preview
  (add-hook 'org-mode-hook 'org-fragtog-mode)

  ;; Fonts & headings
  (dolist (face '((org-level-1 . 1.35)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Source Sans Pro" :weight 'bold :height (cdr face)))
  (set-face-attribute 'org-document-title nil :font "Source Sans Pro" :weight 'bold :height 1.8)

  ;; Prettify symbols
  (defun my/prettify-symbols-setup ()
    (setq prettify-symbols-alist
          '(("[ ]" . "")
            ("[X]" . "")
            ("[-]" . "")
            ("#+BEGIN_SRC" . ?≫)
            ("#+END_SRC" . ?≫)
            (":PROPERTIES:" . "")
            (":projects:" . "")
            (":work:" . "")
            (":inbox:" . "")
            (":task:" . "")
            (":emacs:" . "")))
    (prettify-symbols-mode 1))
  (add-hook 'org-mode-hook #'my/prettify-symbols-setup)
  (add-hook 'org-agenda-mode-hook #'my/prettify-symbols-setup))
  (setq org-agenda-files '("~/notes/topics.org"))
  (map! :leader
      :desc "Active Topics Agenda"
      "t a" #'org-agenda)
;;;=============================================================================
;;; Org-Roam (Slipbox only)
;;;=============================================================================
(use-package! org-roam
  :after org
  :config
  (setq org-roam-directory (file-truename "~/notes/0x03_Resources/slipbox"))
  (setq org-roam-capture-templates
        '(("d" "default slipbox note" plain
           "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)))
  (org-roam-db-autosync-mode 1))

(map! :leader
      :prefix "n r"
      :desc "Quick capture new note" "c"
      (cmd!
        (let* ((title (read-string "Note title: "))
               (node (org-roam-node-create :title title))
               (templates '(("d" "default slipbox note" plain
                             "%?"
                             :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                "#+title: ${title}\n")
                             :unnarrowed t)))
               (buf (org-roam-capture- :node node :templates templates :goto t)))
          (pop-to-buffer buf))))
;;;=============================================================================
;;; Org-Download / Attach
;;;=============================================================================
(use-package! org-download
  :after org
  :config
  (setq org-attach-use-id-dir t
        org-attach-id-dir "~/notes/0x03_Resources/assets/"
        org-download-method 'attach
        org-download-heading-lvl nil
        org-download-timestamp "%Y%m%d-%H%M%S_")
  (map! :map org-mode-map
        :leader
        :desc "Download image from clipboard" "i p" #'org-download-clipboard
        :desc "Download image from URL" "i y" #'org-download-yank))

;;;=============================================================================
;;; Org-Modern
;;;=============================================================================
(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "◆" "▷" "•" "‣")
        org-modern-hide-stars nil
        org-modern-todo nil))

;;;=============================================================================
;;; Org-SRS (Slipbox headings)
;;;=============================================================================
(use-package! org-srs
  :after org
  :config
  (setq org-srs-spaced-repetition-algorithm 'sm2
        org-srs-intervals '(1 4 7 12 16 21 30 45 60))

  (defun my/org-srs-add-to-heading ()
    "Add SRS properties to the current Org heading (only in slipbox)."
    (interactive)
    (unless (org-at-heading-p)
      (error "Not at a heading"))
    (when (and buffer-file-name
               (string-prefix-p (file-truename org-roam-directory)
                                (file-truename buffer-file-name)))
      (unless (org-entry-get nil "SRS" t)
        (org-srs-insert-heading))))

  ;; Bind SRS keys safely
  (map! :leader
        :prefix "o s"
        :desc "Start Org-SRS Review" "s" #'org-srs-review
        :desc "Add heading to Org-SRS" "a" #'my/org-srs-add-to-heading))

;;;=============================================================================
;;; Custom Functions
;;;=============================================================================
(defun my/wrap-region-in-src-block (lang)
  "Wrap the active region in an Org #+BEGIN_SRC block of language LANG."
  (interactive "sLanguage: ")
  (when (use-region-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (goto-char end)
      (insert "\n#+END_SRC")
      (goto-char beg)
      (insert (format "#+BEGIN_SRC %s\n" lang)))))
(global-set-key (kbd "C-c w") #'my/wrap-region-in-src-block)

(defun my/org-set-image-width (width)
  "Set the #+ATTR_ORG :width for an image link at point to WIDTH."
  (interactive "nWidth: ")
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (ignore-errors
        (let ((context (org-element-context)))
          (when (and (eq (org-element-type context) 'link)
                     (string-match-p "\\.\\(png\\|jpe?g\\|gif\\|svg\\|webp\\)$"
                                     (org-element-property :path context)))
            (beginning-of-line)
            (if (looking-at-p "^#\\+ATTR_ORG:")
                (if (string-match ":width [0-9]+" (thing-at-point 'line))
                    (replace-match (format ":width %d" width) t t nil 1)
                  (progn
                    (end-of-line)
                    (insert (format " :width %d" width))))
              (progn
                (goto-char (org-element-property :begin context))
                (beginning-of-line)
                (insert (format "#+ATTR_ORG: :width %d\n" width))))))))))

(defun my/toggle-active-topics-popup-final ()
  "Toggles the display of the Active Topics TODO list popup on the left side.
  Enforces display policy via 'display-buffer-alist' for robust window management."
  (interactive)
  (let ((agenda-buffer (get-buffer "*Org Agenda*")))
    (if (and agenda-buffer (get-buffer-window agenda-buffer (selected-frame)))
        (delete-window (get-buffer-window agenda-buffer (selected-frame)))

      (let ((display-buffer-alist ; Temporarily override display rules
             (cons '("*Org Agenda*"
                     (display-buffer-in-side-window)
                     (side . left)
                     (slot . 0)
                     (window-width . 40)
                     (preserve-size . (t . nil))
                     (window . dedicated))
                   display-buffer-alist)))
        
        ;; Ensure no other agenda view tries to steal focus or split
        (let ((org-agenda-window-setup 'only-window))
          (org-agenda nil "t")))))) ; Org Agenda respects the local display-buffer-alist

(map! :leader
      :desc "Toggle Active Topics Popup"
      "t p" #'my/toggle-active-topics-popup-final)
;;;=============================================================================
;;; Development & LSP
;;;=============================================================================

(setq lsp-auto-guess-root t
      lsp-clients-clangd-executable "/usr/sbin/clangd")

(add-hook 'org-mode-hook #'visual-line-mode)

;; Universal auto-start for all programming languages
(after! lsp-mode
  (add-hook 'prog-mode-hook #'lsp-deferred)
  (add-hook 'c-mode-common-hook #'lsp-format-on-save-mode))

;; Optional: Auto-completion
(after! corfu
  (setq corfu-auto t
        corfu-auto-delay 0.05
        corfu-auto-prefix 1))

;; Optional: UI extras
(use-package! lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics t))

;;;=============================================================================
;;; Popups & Window Management
;;;=============================================================================
(set-popup-rule! "^\\*Capture\\*" :side 'right :size 0.5 :select t)

;;;=============================================================================
;;; Corfu + LSP Setup
;;;=============================================================================

;; Enable Corfu globally
(use-package! corfu
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t                ; enable auto-completion
        corfu-auto-delay 0.05       ; popup delay
        corfu-auto-prefix 1         ; start completing after 1 char
        corfu-preview-current t     ; preview current candidate
        corfu-quit-no-match t
        corfu-scroll-margin 5))

;; Optional: fancy icons in completion
(use-package! kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Make LSP and Corfu talk properly
(use-package! cape
  :after corfu
  :init
  ;; Combine LSP completions and text completions
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; LSP setup (universal)
(after! lsp-mode
  (setq lsp-auto-guess-root t)
  (add-hook 'prog-mode-hook #'lsp-deferred)
  (add-hook 'c-mode-common-hook #'lsp-format-on-save-mode))

(use-package! lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics t))


;; ==============================
;; 🧩 Super Save Configuration
;; ==============================

(use-package! super-save
  :after saveplace
  :config
  ;; Enable globally
  (super-save-mode +1)

  ;; Save buffers when Emacs is idle for a few seconds
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration 120) ;; seconds of idle before autosave

  ;; Add extra triggers that cause saving
  (add-to-list 'super-save-triggers 'other-window)
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-triggers 'projectile-switch-project)

  ;; Keep things quiet (set to nil if you want to see save messages)
  (setq super-save-silent t)

  ;; Exclude scratch and temporary buffers
  (setq super-save-exclude '("#scratch" "*Messages*" "COMMIT_EDITMSG"))

  ;; Ensure Org files always get saved
  (defun my/save-all-org-buffers ()
    "Save all Org buffers, even indirect ones."
    (interactive)
    (save-some-buffers t
                       (lambda ()
                         (and (buffer-file-name)
                              (string-match "\\.org$" (buffer-file-name))))))
  ;; Save all org buffers when Emacs loses focus or autosave runs
  (add-hook 'focus-out-hook #'my/save-all-org-buffers)
  (add-hook 'auto-save-hook #'my/save-all-org-buffers)

  ;; Also auto-save when finishing org-capture
  (add-hook 'org-capture-after-finalize-hook #'super-save-command)

  ;; Extra safety: save everything every 5 minutes
  (run-with-timer 0 300 #'super-save-command))
