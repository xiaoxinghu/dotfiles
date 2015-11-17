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
  (setq org-directory "~/org")
  (setq org-agenda-files (list org-directory))
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  (setq org-log-into-drawer 1)
  ;; Capture Templates
  (setq org-capture-templates
        `(("t" "todo" entry
           (file+headline (concat org-directory "/inbox.org") "Tasks")
           (file ,(concat org-directory "/templates/todo.txt"))
           ;; (file "~/org/templates/todo.txt")
           ::empty-lines-before 1
          ::empty-lines-after 1)
          ("n" "note" entry (file+headline (concat org-directory "/inbox.org") "Notes")
           "* %?\n %U\n %i\n")
          ))
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
