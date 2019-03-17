;;; init.el init -*- lexical-binding: t; -*-

(defvar doom-gc-cons-threshold 16777216 ; 16mb
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar doom-gc-cons-upper-limit 268435456 ; 256mb
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar doom--file-name-handler-alist file-name-handler-alist)

(defun doom|restore-startup-optimizations ()
  "Resets garbage collection settings to reasonable defaults (a large
`gc-cons-threshold' can cause random freezes otherwise) and resets
`file-name-handler-alist'."
  (setq file-name-handler-alist doom--file-name-handler-alist)
  ;; Do this on idle timer to defer a possible GC pause that could result; also
  ;; allows deferred packages to take advantage of these optimizations.
  (run-with-idle-timer
    3 nil (lambda () (setq-default gc-cons-threshold doom-gc-cons-threshold))))


(if (or after-init-time noninteractive)
  (setq gc-cons-threshold doom-gc-cons-threshold)
  ;; A big contributor to startup times is garbage collection. We up the gc
  ;; threshold to temporarily prevent it from running, then reset it later in
  ;; `doom|restore-startup-optimizations'.
  (setq gc-cons-threshold doom-gc-cons-upper-limit)
  ;; This is consulted on every `require', `load' and various path/io functions.
  ;; You get a minor speed up by nooping this.
  (setq file-name-handler-alist nil)
  ;; Not restoring these to their defaults will cause stuttering/freezes.
  (add-hook 'emacs-startup-hook #'doom|restore-startup-optimizations))

(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'autoloads)
(require 'core)
(require 'packages)
(require 'keybinds)
(require 'ui)
(require 'editor)

;; modules
(require 'init-evil)
(require 'init-lisp)
(require 'init-git)
(require 'init-company)
(require 'init-projectile)
(require 'init-dashboard)
(require 'init-treemacs)
(require 'init-deft)
(require 'init-spell)
(require 'init-dired)
(require 'init-flycheck)
(require 'init-data)
(require 'init-translate)
;; lang
(require 'init-lsp)
(require 'init-org)
(require 'init-javascript)
(require 'init-web)
(require 'init-markdown)
(require 'init-swift)
(require 'init-sh)
(require 'init-lua)
(require 'init-ledger)
(require 'init-yaml)
(when IS-MAC
  (require 'init-macos))
