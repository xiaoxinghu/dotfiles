(defvar org-directory "~/io/")

(defun +org|setup-basic ()
  (setq-default
   org-log-into-drawer 1
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

(use-package org
  :ensure org-plus-contrib
  :init
  (add-hook 'org-mode-hook 'flyspell-mode)
  :config
  (+org|setup-basic)
  (+org|setup-agenda)
  (+org|setup-capture)
  :general
  (map|open
    "c" '(org-capture :which-key "Capture")
    "a" '(org-agenda :which-key "Agenda"))
  (map|local 'org-mode-map
    "l" '(org-insert-link :which-key "Inert Link")))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-download
  :ensure t
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
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package org-fancy-priorities
  :ensure t
  :diminish
  :defines org-fancy-priorities-list
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(use-package writeroom-mode
  :ensure t
  :commands (writeroom-mode)
  :config
  (add-to-list 'writeroom-global-effects 'visual-line-mode)
  (setq writeroom-restore-window-config t
	writeroom-width 80
	writeroom-global-effects '(writeroom-set-bottom-divider-width
				   writeroom-set-internal-border-width)))

(use-package ox-reveal
  :ensure t
  :quelpa (ox-reveal :fetcher github :repo "yjwen/org-reveal")
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3/"
        org-reveal-mathjax t))

(provide 'init-org)
