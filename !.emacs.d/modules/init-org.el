(defvar org-directory "~/io/")

(defun +org|setup-basic ()
  (setq-default
    org-log-into-drawer 1
    org-log-done 'time
    org-ellipsis "  "
    org-pretty-entities t
    org-hide-emphasis-markers t
    org-archive-mark-done nil
    org-image-actual-width nil
    org-refile-targets
    '((nil :maxlevel . 3)
       (org-agenda-files :maxlevel . 3))))

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
  (+org|setup-agenda)
  (+org|setup-capture)
  (+org|setup-babel)
  :general
  (map|open
    "c" '(org-capture :which-key "Capture")
    "a" '(org-agenda :which-key "Agenda"))
  (map|local 'org-mode-map
    "l" '(org-insert-link :which-key "Inert Link")))

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