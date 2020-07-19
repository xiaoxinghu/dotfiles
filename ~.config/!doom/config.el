;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Xiaoxing Hu"
      user-mail-address "xiaoxing@huxx.org")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 14))
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))
;; (setq doom-font "JetBrainsMono Nerd Font")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/brain/")
(setq org-roam-directory org-directory)


(use-package deft
  :custom
  (deft-directory org-directory)
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org"))

(after! org-roam
  (setq
    org-roam-capture-ref-templates
    '(("r" "ref" plain (function org-roam-capture--get-point)
        ""
        :file-name "internet/${slug}"
        :head "#+TITLE: ${title}
#+ROAM_KEY: ${ref}
"
        :unnarrowed t))))

(after! org
  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min)))

  (setq
    org-capture-templates
    '(
       ("t" "Task" entry
         (file+headline "todo.org" "Inbox")
         "* [ ] %?\n%i\n%a" :prepend t)
       ("n" "Note" entry
         (function org-journal-find-location)
         "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
       ("j" "Journal" entry
         (function org-journal-find-location)
         "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))
  )

(use-package org-journal
  :custom
    (org-journal-dir (expand-file-name "journal/" org-directory))
    (org-journal-date-prefix "#+TITLE: ")
    (org-journal-file-format "%Y-%m-%d.org")
    (org-journal-time-prefix "* ")
    (org-journal-date-format "%A, %d %B %Y"))

(require 'org-roam-protocol)

(use-package org-super-agenda
  :init
  (setq org-super-agenda-groups
    '(
       (:name "Important" :priority "A")
       (:name "Work" :tag "@work")
       ))
  :config
  (org-super-agenda-mode))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(after! projectile
  (dolist (file '("package.json" "package-lock.json"))
    (add-to-list 'projectile-globally-ignored-files file)))


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
