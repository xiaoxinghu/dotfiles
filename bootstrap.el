#!/usr/bin/env emacs --script

(require 'org)
(require 'ob)
(require 'ob-tangle)

(defvar pwd (file-name-directory load-file-name))

(find-file (expand-file-name "home.org" pwd))
(org-babel-tangle)
(kill-buffer)
