#!/usr/bin/env emacs --script

(require 'org)
(require 'ox-publish)
(setq org-confirm-babel-evaluate nil)
(defun org-babel-execute:yaml (body params) body)
(setq org-export-allow-bind-keywords t)
(setq src "~/io/notes/")
(setq dest "~/Projects/home/source/_posts/")
(message org-version)

(setq org-publish-project-alist
      `(
        ("org-site" :components ("org-content"))
        ("org-content"
         :base-directory ,src
         :base-extension "org"
         :publishing-directory ,dest
         :recursive t
         :exclude "desktop.org\\|^_"
         :publishing-function org-html-publish-to-html
         :body-only t
         :with-toc nil
         )
        ("org-static"
         :base-directory ,src
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :exclude "src/*\\|node_modules/*\\|webpack.config.js\\|public/*"
         :publishing-directory ,dest
         :recursive t
         :publishing-function org-publish-attachment
         )
        ))

(org-publish-all)
