;;; packages.el --- x-publish Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq x-publish-packages
    '(
      ;; package names go here
      (org :location built-in)
      (ox-publish :location built-in)
      ))

(defun x-publish/init-ox-publish ()
  (setq org-publish-project-alist
        '(
          ("org-notes"
           :base-directory "~/org/"
           :base-extension "org"
           :publishing-directory "~/public_html/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4             ; Just the default for this project.
           :auto-preamble t
           )
          ("org-static"
           :base-directory "~/org/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/public_html/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("org" :components ("org-notes" "org-static"))
          ))
  (message "x-publish cool")
  )

;; List of packages to exclude.
(setq x-publish-excluded-packages '())

;; For each package, define a function x-publish/init-<package-name>
;;
;; (defun x-publish/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
