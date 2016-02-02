;;; packages.el --- x-org Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Xiaoxing Hu
;; Copyright (c) 2014-2015 Xiaoxing Hu & Contributors
;;
;; Author: Xiaoxing Hu <dawnstar.hu@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq x-org-packages
    '(
      (org :location built-in)
      ))

;; List of packages to exclude.
(setq x-org-excluded-packages '())

(defun x-org/post-init-org ()
  (setq org-modules (quote (org-protocol)))
  (require 'org-protocol)
  (setq org-directory "~/io")
  (setq org-agenda-files (list org-directory
                               (concat org-directory "/notes")
                               (concat org-directory "/posts")
                               (concat org-directory "/projects")))
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  (setq org-log-into-drawer 1)
  ;; Capture Templates
  (setq org-capture-templates
        `(("t" "todo" entry
           (file (concat org-directory "/inbox.org"))
           (file ,(concat configuration-layer-private-directory "x-org/templates/todo.txt"))
           ::empty-lines-before 1
          ::empty-lines-after 1)
          ("n" "note" entry
           (file (concat org-directory "/inbox.org"))
           (file ,(concat configuration-layer-private-directory "x-org/templates/note.txt"))
           ::empty-lines-before 1
           ::empty-lines-after 1)
          ("l" "link" entry
           (file (concat org-directory "/inbox.org"))
           (file ,(concat configuration-layer-private-directory "x-org/templates/link.txt"))
           ::empty-lines-before 1
           ::empty-lines-after 1)
          ("j" "journal" plain
           (file+datetree (concat org-directory "/journal.org"))
           (file ,(concat configuration-layer-private-directory "x-org/templates/journal.txt"))
           ::empty-lines-before 1
           ::empty-lines-after 1)
          ))

  ;; Capture Window popup
  (defadvice org-capture
      (after make-full-window-frame activate)
    "Advise capture to be the only window when used as a popup"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-other-windows)))

  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-frame)))

  ;; TODO keywords
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold))))

  ;; Agenda
  (setq org-agenda-custom-commands
        (quote ((" " "Home"
                 ((agenda "" nil)
                  (todo "NEXT"
                        ((org-agenda-overriding-header "NEXT")))
                  (tags "REFILE"
                        ((org-agenda-overriding-header "TO REFILE")))
                  (tags-todo "PROJECT+TODO=\"TODO\""
                             ((org-agenda-overriding-header "PROJECTS")
                              (org-agenda-sorting-strategy '(todo-state-up))
                              ))
                  (tags-todo "POST+TODO=\"TODO\""
                             ((org-agenda-overriding-header "WRITING")
                              (org-agenda-sorting-strategy '(todo-state-up))
                              ))
                  (tags-todo "NOTE+TODO=\"TODO\""
                             ((org-agenda-overriding-header "NOTES")
                              (org-agenda-sorting-strategy '(todo-state-up))
                              ))
                  (todo "WAITING|HOLD"
                        ((org-agenda-overriding-header "PENDING")
                         (org-agenda-sorting-strategy '(todo-state-up))
                         ))
                  )))))

  ;; Archiving
  (setq org-archive-mark-done nil)
  (setq org-archive-location "%s_archive::* Archived Tasks")
  )


;; For each package, define a function x-org/init-<package-name>
;;
;; (defun x-org/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
