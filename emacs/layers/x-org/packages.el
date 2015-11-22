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
  (setq org-directory "~/org")
  (setq org-agenda-files (list org-directory
                               (concat org-directory "/notes")
                               (concat org-directory "/projects")))
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  (setq org-log-into-drawer 1)
  ;; Capture Templates
  (setq org-capture-templates
        `(("t" "todo" entry
           (file+headline (concat org-directory "/inbox.org") "Tasks")
           (file ,(concat configuration-layer-private-directory "x-org/templates/todo.txt"))
           ::empty-lines-before 1
          ::empty-lines-after 1)
          ("n" "note" entry (file+headline (concat org-directory "/inbox.org") "Notes")
           (file ,(concat configuration-layer-private-directory "x-org/templates/note.txt"))
           ::empty-lines-before 1
           ::empty-lines-after 1)
          ("l" "link" entry (file+headline (concat org-directory "/inbox.org") "Notes")
           (file ,(concat configuration-layer-private-directory "x-org/templates/link.txt"))
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
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold))))

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
