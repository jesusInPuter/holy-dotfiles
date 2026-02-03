;;;=============================================================================
;;; Core UI & Frame Configuration
;;;=============================================================================
(setq doom-theme 'doom-gruvbox
      doom-font (font-spec :family "FiraCode Nerd Font" :size 19)
      doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 17))

(add-to-list 'default-frame-alist '(alpha . (0.8 . 0.8)))
(set-frame-parameter (selected-frame) 'alpha '(0.8 . 0.8))

(setq display-line-numbers-type 'relative
      display-line-numbers-width 3
      display-line-numbers-width-start t)
(global-display-line-numbers-mode t)

(use-package! exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

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
  (add-hook 'org-agenda-mode-hook #'my/prettify-symbols-setup)

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
;;; Org Mode Setup
;;;=============================================================================
(setq org-directory "~/notes")

(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((latex . t)
     (gnuplot . t)
     (python . t)))
  (setq org-confirm-babel-evaluate nil)

  (add-to-list 'org-latex-default-packages-alist '("" "tikz" t))
  (add-to-list 'org-latex-default-packages-alist '("" "pgfplots" t))
  (add-to-list 'org-latex-default-packages-alist '("" "amsmath" t))

  (setq org-latex-compiler "xelatex"
        org-latex-pdf-process
        '("xelatex -interaction=nonstopmode -output-directory=%o %f"
          "xelatex -interaction=nonstopmode -output-directory=%o %f")
        org-preview-latex-default-process 'imagemagick)

  (add-hook 'org-mode-hook #'org-fragtog-mode)

  (dolist (face '((org-level-1 . 1.35)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)))
    (set-face-attribute (car face) nil :font "Source Sans Pro"
                        :weight 'bold :height (cdr face)))

  (set-face-attribute 'org-document-title nil
                      :font "Source Sans Pro"
                      :weight 'bold
                      :height 1.8))

(setq org-agenda-files '("~/notes/topics.org"))

(map! :leader
      :desc "Active Topics Agenda"
      "t a" #'org-agenda)

;;;=============================================================================
;;; Org-Roam
;;;=============================================================================
(use-package! org-roam
  :after org
  :config
  (setq org-roam-directory (file-truename "~/notes"))

(setq org-roam-capture-templates
      '(
        ;; --------------------------------------------------
        ;; Atomic idea (core thinking unit)
        ;; --------------------------------------------------
        ("a" "Atomic note" plain
         "%?"
         :target (file+head
                  "0x03_Resources/slipbox/%<%Y%m%d%H%M%S>-${slug}.org"
                  "#+title: ${title}
#+filetags: 
")
         :unnarrowed t)

        ;; --------------------------------------------------
        ;; Book pointer (source node, NOT for ideas)
        ;; --------------------------------------------------
        ("b" "Book pointer" plain
         ""
         :target (file+head
                  "0x03_Resources/slipbox/books/${slug}.org"
                  "#+title: ${title}
#+filetags: :book:
:AUTHOR:
:THEMES:
:STATUS: unread
")
         :unnarrowed t)

        ;; --------------------------------------------------
        ;; Article / Blog / Paper
        ;; --------------------------------------------------
        ("p" "Article" plain
         ""
         :target (file+head
                  "0x03_Resources/slipbox/articles/${slug}.org"
                  "#+title: ${title}
#+filetags: :article:
:AUTHOR:
:URL:
:THEMES:
")
         :unnarrowed t)

        ;; --------------------------------------------------
        ;; Idea derived from a Book (filtered selector)
        ;; --------------------------------------------------
        ("i" "Idea from Book" plain
         "%?
* Source
- Book :: %(let ((node
                  (org-roam-node-read
                   nil
                   (lambda (node)
                     (member \"book\" (org-roam-node-tags node))))))
            (when node
              (org-link-make-string
               (concat \"id:\" (org-roam-node-id node))
               (org-roam-node-title node))))
"
         :target (file+head
                  "0x03_Resources/slipbox/%<%Y%m%d%H%M%S>-${slug}.org"
                  "#+title: ${title}
#+filetags: 
")
         :unnarrowed t)

        ;; --------------------------------------------------
        ;; Monolithic Area note (curation layer)
        ;; --------------------------------------------------
        ("m" "Monolithic note (choose Area)" plain
         "%?"
         :target
         (file+head
          (lambda ()
            (let* ((base (expand-file-name "0x02_Areas/" org-directory))
                   (dir (read-directory-name "Area directory: " base nil nil)))
              (make-directory dir t)
              (expand-file-name
               (concat (format-time-string "%Y%m%d%H%M%S-") "${slug}.org")
               dir)))
          "#+title: ${title}
#+filetags: 
")
         :unnarrowed t)
        ))

  (org-roam-db-autosync-mode 1))

(map! :leader
      "n r f" #'org-roam-node-find
      "n r i" #'org-roam-node-insert)

;;;=============================================================================
;;; Org-Download
;;;=============================================================================
(use-package! org-download
  :after org
  :config
  (setq org-attach-use-id-dir t
        org-attach-id-dir "~/notes/0x03_Resources/assets/"
        org-download-method 'attach))

;;;=============================================================================
;;; Development & LSP
;;;=============================================================================
(setq lsp-auto-guess-root t)

(after! lsp-mode
  (add-hook 'prog-mode-hook #'lsp-deferred))

;;;=============================================================================
;;; Corfu
;;;=============================================================================
(use-package! corfu
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.05))

;;;=============================================================================
;;; Super Save
;;;=============================================================================
(use-package! super-save
  :config
  (super-save-mode +1)
  (setq super-save-silent t))

;;; custom apps
(defun my/org-roam-insert-book ()
  "Insert a link to a book node."
  (interactive)
  (let ((node (org-roam-node-read
               nil
               (lambda (node)
                 (member "book" (org-roam-node-tags node))))))
    (when node
      (insert
       (org-link-make-string
        (concat "id:" (org-roam-node-id node))
        (org-roam-node-title node))))))

(after! org
  (map! :map org-mode-map
        "C-c b" #'my/org-roam-insert-book))
