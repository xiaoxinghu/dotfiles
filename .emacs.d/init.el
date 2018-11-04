;;; init.el init -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'core)
(require 'packages)
(require 'key-bindings)
(require 'ui)
(require 'editor)

;; modules
(require 'init-magit)
(require 'init-evil)
(require 'init-lisp)
(require 'init-company)
(require 'init-projectile)
(require 'init-treemacs)
(require 'init-deft)
(require 'init-spell)
(require 'init-dired)
(require 'init-flycheck)
;; lang
(require 'init-org)
(require 'init-javascript)
(require 'init-web)
(require 'init-markdown)
(require 'init-swift)
(require 'init-sh)
(when IS-MAC
  (require 'init-macos))
