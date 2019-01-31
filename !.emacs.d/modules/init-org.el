(defvar org-directory "~/io/")

(defun +org|setup-basic ()
  (setq-default
    org-log-into-drawer 1
    org-adapt-indentation t
    org-log-done 'time
    org-ellipsis "  "
    org-pretty-entities t
    org-hide-emphasis-markers t
    org-archive-mark-done nil
    org-image-actual-width nil
    org-hide-leading-stars t
    org-hide-leading-stars-before-indent-mode t
    org-tags-column 0
    org-todo-keywords
    '((sequence "[ ](t)" "[-](p)" "[?](m)" "|" "[X](d)")
       (sequence "TODO(T)" "|" "DONE(D)")
       (sequence "NEXT(n)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)"))
    org-todo-keyword-faces
    '(("[-]" :inherit font-lock-constant-face :weight bold)
       ("[?]" :inherit warning :weight bold)
       ("WAITING" :inherit default :weight bold)
       ("LATER" :inherit warning :weight bold))
    org-refile-targets
    '((nil :maxlevel . 3)
      (org-agenda-files :maxlevel . 3))))

(defun +org|setup-ui ()
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-]\\) "
			     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (let* ((variable-tuple
	  (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
		((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
		((x-list-fonts "Verdana")         '(:font "Verdana"))
		((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
		(nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
	 (base-font-color     (face-foreground 'default nil 'default))
	 (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Source Sans Pro" :height 180 :weight light))))
   '(fixed-pitch ((t ( :family "Inconsolata" :slant normal :weight normal :height 1.0 :width normal)))))

  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)

  (custom-theme-set-faces
   'user
   '(org-block                 ((t (:inherit fixed-pitch))))
   '(org-document-info         ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-link                  ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value        ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim              ((t (:inherit (shadow fixed-pitch))))))
  )

(defun +org|setup-agenda ()
  (setq org-agenda-window-setup 'other-window
    org-agenda-restore-windows-after-quit nil)
  (unless org-agenda-files
    (setq org-agenda-files (concat org-directory "/.agenda-files")))
  (setq org-agenda-custom-commands
    '((" " "My Agenda"
        ((agenda "This Week" ((org-agenda-span 7) ;; days for the calander
                               ))
          (tags-todo "-pause+TODO=\"NEXT\""
            ((org-agenda-overriding-header "NEXT")))
          (tags-todo "-pause+@work"
            ((org-agenda-overriding-header "WORK")))
          ))
       ("r" "Review"
         (
           (tags-todo "-pause+TODO=\"TODO\"-CATEGORY=\"routine\""
             ((org-agenda-overriding-header "TODOs")))
           (tags-todo "pause"
             ((org-agenda-overriding-header "PAUSED")))
           ))
       ("Q" . "Custom Queries")
       ("Qn" "Note Search" search ""
         ((org-agenda-files (file-expand-wildcards (concat org-directory "/notes/*.org")))))
       ))
  )

(defun +org|setup-capture ()
  (setq org-capture-templates
    `(("t" "todo" entry
        (file+headline ,(concat org-directory "/inbox.org") "Tasks")
        "* TODO %?\n:LOGBOOK:\n- Added: %U\n:END:"
        ::empty-lines-before 1
        ::empty-lines-after 1)
       ("n" "note" entry
         (file+headline ,(concat org-directory "/inbox.org") "Notes")
         "* %^{description}\n:LOGBOOK:\n- Added: %U\n:END:\n\n%?"
         ::empty-lines-before 1
         ::empty-lines-after 1)
       ("l" "link" entry
         (file+headline ,(concat org-directory "/inbox.org") "Notes")
         "* %?\n:LOGBOOK:\n- Added: %U\n:END:\n%^L"
         ::empty-lines-before 1
         ::empty-lines-after 1))))

(defun +org|setup-babel ()
  (setq
    org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.12/libexec/plantuml.jar"
    org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
       (gnuplot . t)
       (plantuml . t))))

(use-package org
  :ensure org-plus-contrib
  :init
  ;; (add-hook 'org-mode-hook 'flyspell-mode)
  :config
  (+org|setup-basic)
  (+org|setup-ui)
  (+org|setup-agenda)
  (+org|setup-capture)
  (+org|setup-babel)
  (defhydra hydra-org-subtree ()
    "subtree"
    ("q" nil "quit" :color: blue)
    ("j" org-move-subtree-down "down")
    ("k" org-move-subtree-up "promote")
    ("h" org-promote-subtree "promote")
    ("l" org-demote-subtree "demote"))
  :general
  (map|open
    "c" '(org-capture :which-key "Capture")
    "a" '(org-agenda :which-key "Agenda"))
  (map|local 'org-mode-map
    "l" '(org-insert-link :which-key "Inert Link")
    "s" '(hydra-org-subtree/body :which-key "Subtree")
    "t" '(org-todo :which-key "TODO")
    "T" '(org-show-todo-tree :which-key "Show TODOs")))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'evil-org-mode-hook
    (lambda ()
      (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-download
  :config
  (setq-default org-download-image-dir (expand-file-name ".attach" org-directory))
  (defun +org-attach*download-fullname (path)
    "Write PATH relative to current file."
    (let ((dir (or (if buffer-file-name (file-name-directory buffer-file-name))
                 default-directory)))
      (if (file-in-directory-p dir org-directory)
        (file-relative-name path dir)
        path)))
  (advice-add #'org-download--dir-2 :override #'ignore)
  (advice-add #'org-download--fullname
    :filter-return #'+org-attach*download-fullname))

(use-package org-bullets
  :quelpa (org-bullets :fetcher github :repo "Kaligule/org-bullets")
  :hook (org-mode . org-bullets-mode))

(use-package org-fancy-priorities
  :diminish
  :defines org-fancy-priorities-list
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(use-package writeroom-mode
  :commands (writeroom-mode)
  :config
  (add-to-list 'writeroom-global-effects 'visual-line-mode)
  (setq writeroom-restore-window-config t
    writeroom-width 80
    writeroom-global-effects '(writeroom-set-bottom-divider-width
                                writeroom-set-internal-border-width)))

(use-package ox-reveal
  :after org
  :defer t
  :ensure nil
  :quelpa (ox-reveal :fetcher github :repo "yjwen/org-reveal")
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3/"
    org-reveal-mathjax t))

;; export
(add-hook 'org-load-hook #'+org|init-export)


(defun +org|init-export ()
  (setq org-export-backends '(ascii html latex md)
    org-publish-timestamp-directory (concat x/cache-dir "org-timestamps/"))

  (when (and (executable-find "pandoc")
          (require 'ox-pandoc nil t))
    (add-to-list 'org-export-backends 'pandoc nil #'eq)
    (setq org-pandoc-options
      '((standalone . t)
         (mathjax . t)
         (variable . "revealjs-url=https://cdn.jsdelivr.net/npm/reveal.js@3/")))))

(use-package htmlize
  :commands (htmlize-buffer
              htmlize-file
              htmlize-many-files
              htmlize-many-files-dired
              htmlize-region))

(use-package plantuml-mode
  :defer t
  :mode ("\\.pum\\'" . plantuml-mode)
  :config
  (setq plantuml-jar-path org-plantuml-jar-path))

(use-package gnuplot
  :defer t)

(use-package ox-hugo
  :after ox)

(provide 'init-org)
