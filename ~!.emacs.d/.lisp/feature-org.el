(defun +org|org-archive-done-tasks ()
  "Archive finished or cancelled tasks."
  (interactive)
  (org-map-entries
    (lambda ()
      (org-archive-subtree)
      (setq org-map-continue-from (outline-previous-heading)))
    "TODO=\"DONE\"|TODO=\"CANCELLED\"" (if (org-before-first-heading-p) 'file 'tree)))

(defun +org|yank-more ()
  (interactive)
  (insert "[[")
  (yank)
  (insert "][more]]"))

(defvar org-directory "~/Documents/io/")

(defun +org|setup-basic ()
  (setq-default
   org-log-into-drawer 1
   org-adapt-indentation nil
   org-edit-src-content-indentation 0
   org-log-done 'time
   org-ellipsis "  "
   org-pretty-entities t
   org-hide-emphasis-markers nil
   org-archive-mark-done nil
   org-image-actual-width nil
   org-hide-leading-stars t
   org-hide-leading-stars-before-indent-mode t
   org-tags-column 0
   org-startup-with-inline-images t
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
     (org-agenda-files :maxlevel . 3)))
  (defvar org-modules
    '(;; ol-w3m
      ;; ol-bbdb
      ol-bibtex
      org-protocol
      ;; ol-docview
      ;; ol-gnus
      ;; ol-info
      ;; ol-irc
      ;; ol-mhe
      ;; ol-rmail
      ;; ol-eww
      ))
  )

(define-minor-mode +org-pretty-mode
  "Hides emphasis markers and toggles pretty entities."
  :init-value nil
  :lighter " *"
  :group 'evil-org
  (setq org-hide-emphasis-markers +org-pretty-mode)
  (org-toggle-pretty-entities)
  (with-silent-modifications
   ;; In case the above un-align tables
   (org-table-map-tables 'org-table-align t)))

(defun +org|setup-ui ()
  "Configures the UI for `org-mode'."
  (setq org-indirect-buffer-display 'current-window
        org-eldoc-breadcrumb-separator " → "
        org-enforce-todo-dependencies t
        org-entities-user
        '(("flat"  "\\flat" nil "" "" "266D" "♭")
          ("sharp" "\\sharp" nil "" "" "266F" "♯"))
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-footnote-auto-label 'plain
        org-hide-leading-stars t
        org-hide-leading-stars-before-indent-mode t
        org-image-actual-width nil
        org-list-description-max-indent 4
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success))
        org-startup-indented t
        org-tags-column -80
        org-use-sub-superscripts '{})

  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3))
        ;; Without this, completers like ivy/helm are only given the first level of
        ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
        ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
        ;; target! e.g. FILE/Tasks/heading/subheading
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  ;; Scale up LaTeX previews a bit (default is too small)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  ;; ...and fix their background w/ themes
;;   (add-hook! 'doom-load-theme-hook
;;     (defun +org-refresh-latex-background ()
;;       "Previews are usually rendered with light backgrounds, so ensure their
;; background (and foreground) match the current theme."
;;       (plist-put! org-format-latex-options
;;                   :background
;;                   (face-attribute (or (cadr (assq 'default face-remapping-alist))
;;                                       'default)
;;                                   :background nil t))))

  ;; HACK Face specs fed directly to `org-todo-keyword-faces' don't respect
  ;;      underlying faces like the `org-todo' face does, so we define our own
  ;;      intermediary faces that extend from org-todo.
  (custom-declare-face '+org-todo-active '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
  (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
  (custom-declare-face '+org-todo-onhold '((t (:inherit (bold warning org-todo)))) "")
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; An ongoing project that cannot be completed in one step
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something is holding up this task; or it is paused
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")) ; Task was completed
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)))

  ;; (defadvice! +org-display-link-in-eldoc-a (orig-fn &rest args)
  ;;   "Display full link in minibuffer when cursor/mouse is over it."
  ;;   :around #'org-eldoc-documentation-function
  ;;   (or (when-let (link (org-element-property :raw-link (org-element-context)))
  ;;         (format "Link: %s" link))
  ;;       (apply orig-fn args)))

  ;; Automatic indent detection in org files is meaningless
  ;; (cl-pushnew 'org-mode doom-detect-indentation-excluded-modes :test #'eq)

  ;; (set-pretty-symbols! 'org-mode
  ;;   :name "#+NAME:"
  ;;   :src_block "#+BEGIN_SRC"
  ;;   :src_block_end "#+END_SRC")
  )

(defun +org|setup-keys ()
  (general-create-definer map|org
    :states '(normal visual insert emacs)
    :prefix "SPC o"
    :non-normal-prefix "C-SPC o")
  (defhydra hydra-org-subtree ()
    "subtree"
    ("q" nil "quit" :color: blue)
    ("j" org-move-subtree-down "down")
    ("k" org-move-subtree-up "promote")
    ("h" org-promote-subtree "promote")
    ("l" org-demote-subtree "demote"))
  (map|org
    "c" '(org-capture :which-key "Capture")
    "a" '(org-agenda :which-key "Agenda"))
  (map|local 'org-mode-map
    "A" '(+org|org-archive-done-tasks :which-key "Archive All")
    "a" '(org-archive-subtree-default :which-key "Archive Subtree")
    "b" '(org-insert-structure-template :which-key "Insert Block")
    "l" '(org-insert-link :which-key "Inert Link")
    "h" '(org-insert-heading-after-current :which-key "Inert Heading")
    "y" '(+org|yank-more :which-key "Yank More")
    "s" '(hydra-org-subtree/body :which-key "Subtree")
    "f" '(org-toggle-narrow-to-subtree :which-key "Toggle Focus")
    "t" '(org-todo :which-key "TODO")
    "T" '(org-show-todo-tree :which-key "Show TODOs")
    "p" '(org-tree-slide-mode :which-key "Present")))

(defun +org|setup-agenda ()
  (setq org-agenda-window-setup 'other-window
        org-agenda-restore-windows-after-quit nil)
  (unless org-agenda-files
    (setq org-agenda-files (concat org-directory ".agenda-files")))
  (setq org-agenda-custom-commands
        '((" " "My Agenda"
           ((agenda "This Week" ((org-agenda-span 7) ;; days for the calander
                                 ))
            (tags-todo "-pause+TODO=\"NEXT\""
                       ((org-agenda-overriding-header "NEXT")))
            (tags-todo "-pause+@work"
                       ((org-agenda-overriding-header "WORK")))
            (tags-todo "-pause+TODO=\"DRAFT\""
                       ((org-agenda-overriding-header "WRITING")))
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
           ((org-agenda-files (file-expand-wildcards (concat org-directory "notes/*.org")))))
          ))
  )

(use-package org-super-agenda
  :after (org org-agenda)
  :quelpa (org-super-agenda :fetcher github :repo "alphapapa/org-super-agenda")
  :config
  (org-super-agenda-mode t)
  (setq org-super-agenda-groups
        '((:name "Important tasks ":priority "A")
          (:name "SynSIG" :tag "SynSIG")
          (:auto-category t)
          )))

(defun +org|setup-capture ()
  (setq org-capture-templates
        `(("t" "todo" entry
           (file+headline ,(concat org-directory "inbox.org") "Tasks")
           "* TODO %?\n:LOGBOOK:\n- Added: %U\n:END:"
           ::empty-lines-before 1
           ::empty-lines-after 1)
          ("n" "note" entry
           (file+headline ,(concat org-directory "inbox.org") "Notes")
           "* %^{description}\n:LOGBOOK:\n- Added: %U\n:END:\n\n%?"
           ::empty-lines-before 1
           ::empty-lines-after 1)
          ("l" "link" entry
           (file+headline ,(concat org-directory "inbox.org") "Notes")
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
  (require 'org-tempo)
  (+org|setup-keys)
  (+org|setup-basic)
  (+org|setup-ui)
  (+org|setup-agenda)
  (+org|setup-capture)
  (+org|setup-babel)
  )

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

(use-package org-re-reveal
  :after org
  :config
  (setq
   org-reveal-mathjax t))

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

(use-package org-tree-slide
  :commands (org-tree-slide-mode)
  :config
  (org-tree-slide-simple-profile)
  ;; (setq
  ;;   org-tree-slide-activate-message " "
  ;;   org-tree-slide-deactivate-message " "
  ;;   org-tree-slide-modeline-display nil)

  (add-hook 'org-tree-slide-mode-hook #'evil-normalize-keymaps)

  (general-define-key
   :states '(normal visual)
   :keymaps 'org-tree-slide-mode-map
   "q" 'org-tree-slide-mode
   "<up>" 'org-tree-slide-content
   "<down>" 'org-tree-slide-display-header-toggle
   "<left>" 'org-tree-slide-move-previous-tree
   "<right>" 'org-tree-slide-move-next-tree)
  )

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

(use-package org-web-tools)

(use-package deft
  :commands deft
  :init
  (setq deft-extensions '("org")
        deft-default-extension "org"
        deft-directory org-directory
        deft-recursive t
        ;; de-couples filename and note title:
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-recursive-ignore-dir-regexp "\\(?:\\.\\|\\.\\.\\|\\.archives\\|www\\)$"
        ;; deft-ignore-file-regexp "\\(?:www/*\\)"
        ;; deft-recursive-ignore-dir-regexp "\\(?:www\\)"
        ;; deft-org-mode-title-prefix t
        ;; converts the filter string into a readable file-name using kebab-case:
        deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))
  :config
  :general
  (map|org
    "n" '(deft :which-key "Deft")))
;; start filtering immediately
;; (set-evil-initial-state! 'deft-mode 'insert)
;; (map! :map deft-mode-map
;;       :localleader
;;       :n "RET" #'deft-new-file-named
;;       :n "a" #'deft-archive-file
;;       :n "c" #'deft-filter-clear
;;       :n "d" #'deft-delete-file
;;       :n "f" #'deft-find-file
;;       :n "g" #'deft-refresh
;;       :n "l" #'deft-filter
;;       :n "n" #'deft-new-file
;;       :n "r" #'deft-rename-file
;;       :n "s" #'deft-toggle-sort-method
;;       :n "t" #'deft-toggle-incremental-search))

(use-package org-cliplink
  :general
  (map|local 'org-mode-map
    "L" '(org-cliplink :which-key "insert clipboard")))

(use-package org-journal
  :defer t
  :custom
  (org-journal-dir (concat org-directory "journal/"))
  (org-journal-cache-file (concat x/cache-dir "org-journal.cache"))
  (org-journal-file-type `weekly)
  (org-journal-file-format "%Y-%m-%d")
  (org-journal-date-format "%A, %d %B %Y")
  :general
  (map|org
    "j" '(org-journal-new-entry :which-key "Journal"))
  )

(provide 'feature-org)
