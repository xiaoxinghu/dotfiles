;;; .init.el --- My emacs setup                      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xiaoxing Hu

;; Author: Xiaoxing Hu <xiaoxing@huxx.org>
;; Keywords: docs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(load (concat user-emacs-directory ".lisp/bootstrap")
      nil 'nomessage)
(x/initialize-core)

(use-package avy
  :commands (avy-goto-word-1)
  :general
  (map!
    "SPC" '(avy-goto-word-or-subword-1  :which-key "go to char")))

(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (bind-key "C-c C-r" 'ivy-resume))

(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode)
  (setq prescient-save-file (concat x/var-dir "pres
-save.el")))

(use-package counsel
  :bind
  ("M-F" . counsel-projectile-ag)
  :general
  (map!
    "f" '(:ignore t :which-key "Files")
    "f f" '(counsel-find-file :which-key "find file")
    "f y" '(x|yank-buffer-filename :which-key "yank filename")
    "f r" '(counsel-recentf :which-key "recent file")
    "f R" '(crux-rename-buffer-and-file :which-key "rename file")
    "f d" '(dired :which-key "dired")
    "f D" '(crux-delete-file-and-buffer :which-key "delete file")
    ;; "b" '(:ignore t :which-key "Buffers")
    ;; "b b" '(counsel-ibuffer :which-key "ibuffer")
    ":" 'counsel-M-x)
  :bind
  ("M-x" . counsel-M-x)
  ("C-c k" . counsel-ag))

(use-package swiper
  :bind
  ("M-f" . swiper))

(use-package dired
  :ensure nil
  :config
  (defhydra hydra-dired (:hint nil :color pink)
    "
     _+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
     _C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
     _D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
     _R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
     _Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
     _S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
     _r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
     _z_ compress-file  _A_ find regexp
     _Z_ compress       _Q_ repl regexp

     T - tag prefix
     "
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff)         ;; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy)        ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer)        ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay)   ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))

  (define-key dired-mode-map "." 'hydra-dired/body))

(use-package ranger
  :after dired
  :init
  :config
  (setq ranger-override-dired t
        ranger-cleanup-on-disable t
        ranger-omit-regexp "^\.DS_Store$"
        ranger-excluded-extensions '("mkv" "iso" "mp4")
        ranger-deer-show-details nil
        ranger-max-preview-size 10
        dired-omit-verbose nil))

(use-package evil
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config ;; tweak evil after loading it
  (evil-mode)
  ;; example how to map a command in normal mode (called 'normal state' in evil)
  (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit)

  (defun +evil|disable-highlights ()
    "Disable ex search buffer highlights."
    (when (evil-ex-hl-active-p 'evil-ex-search)
      (evil-ex-nohighlight)
      t))
  (add-hook 'doom-escape-hook #'+evil|disable-highlights)

  (defun +evil*escape (&rest _)
    "Call `doom/escape' if `evil-force-normal-state' is called interactively."
    (when (called-interactively-p 'any)
      (call-interactively #'doom/escape)))
  ;; Make ESC (from normal mode) the universal escaper. See `doom-escape-hook'.
  (advice-add #'evil-force-normal-state :after #'+evil*escape)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-magit
  :after (evil magit)
  :config
  (message "evil-magit started"))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :config (evil-commentary-mode 1))

(use-package company
  :init
  (setq
    ;; company-idle-delay nil
    company-tooltip-limit 14
    company-dabbrev-downcase nil
    company-dabbrev-ignore-case nil
    company-dabbrev-code-other-buffers t
    company-tooltip-align-annotations t
    company-require-match 'never
    ;; company-global-modes
    ;; '(not erc-mode message-mode help-mode gud-mode eshell-mode)
    ;; company-backends '(company-capf)
    company-frontends
    '(company-pseudo-tooltip-frontend
       company-echo-metadata-frontend)
    )
  :config
  (global-company-mode +1)
  (company-tng-configure-default))

(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :config
  (setq prescient-save-file (concat x/cache-dir "prescient-save.el"))
  (prescient-persist-mode +1))

(use-package company-box
  :when EMACS26+
  :hook (company-mode . company-box-mode)
  :config
  (setq
    company-box-show-single-candidate t
    company-box-backends-colors nil
    company-box-max-candidates 50
    company-box-icons-alist 'company-box-icons-all-the-icons
    company-box-icons-functions
    '(+company-box-icons--yasnippet company-box-icons--lsp +company-box-icons--elisp company-box-icons--acphp)
    company-box-icons-all-the-icons
    `((Unknown       . ,(all-the-icons-material "find_in_page"             :height 0.8 :face 'all-the-icons-purple))
       (Text          . ,(all-the-icons-material "text_fields"              :height 0.8 :face 'all-the-icons-green))
       (Method        . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
       (Function      . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
       (Constructor   . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
       (Field         . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
       (Variable      . ,(all-the-icons-material "adjust"                   :height 0.8 :face 'all-the-icons-blue))
       (Class         . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))
       (Interface     . ,(all-the-icons-material "settings_input_component" :height 0.8 :face 'all-the-icons-red))
       (Module        . ,(all-the-icons-material "view_module"              :height 0.8 :face 'all-the-icons-red))
       (Property      . ,(all-the-icons-material "settings"                 :height 0.8 :face 'all-the-icons-red))
       (Unit          . ,(all-the-icons-material "straighten"               :height 0.8 :face 'all-the-icons-red))
       (Value         . ,(all-the-icons-material "filter_1"                 :height 0.8 :face 'all-the-icons-red))
       (Enum          . ,(all-the-icons-material "plus_one"                 :height 0.8 :face 'all-the-icons-red))
       (Keyword       . ,(all-the-icons-material "filter_center_focus"      :height 0.8 :face 'all-the-icons-red))
       (Snippet       . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-red))
       (Color         . ,(all-the-icons-material "color_lens"               :height 0.8 :face 'all-the-icons-red))
       (File          . ,(all-the-icons-material "insert_drive_file"        :height 0.8 :face 'all-the-icons-red))
       (Reference     . ,(all-the-icons-material "collections_bookmark"     :height 0.8 :face 'all-the-icons-red))
       (Folder        . ,(all-the-icons-material "folder"                   :height 0.8 :face 'all-the-icons-red))
       (EnumMember    . ,(all-the-icons-material "people"                   :height 0.8 :face 'all-the-icons-red))
       (Constant      . ,(all-the-icons-material "pause_circle_filled"      :height 0.8 :face 'all-the-icons-red))
       (Struct        . ,(all-the-icons-material "streetview"               :height 0.8 :face 'all-the-icons-red))
       (Event         . ,(all-the-icons-material "event"                    :height 0.8 :face 'all-the-icons-red))
       (Operator      . ,(all-the-icons-material "control_point"            :height 0.8 :face 'all-the-icons-red))
       (TypeParameter . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))
       ;; (Template   . ,(company-box-icons-image "Template.png"))))
       (Yasnippet     . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-green))
       (ElispFunction . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
       (ElispVariable . ,(all-the-icons-material "check_circle"             :height 0.8 :face 'all-the-icons-blue))
       (ElispFeature  . ,(all-the-icons-material "stars"                    :height 0.8 :face 'all-the-icons-orange))
       (ElispFace     . ,(all-the-icons-material "format_paint"             :height 0.8 :face 'all-the-icons-pink))))

  (defun +company-box-icons--yasnippet (candidate)
    (when (get-text-property 0 'yas-annotation candidate)
      'Yasnippet))

  (defun +company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym)  'ElispFunction)
          ((boundp sym)   'ElispVariable)
          ((featurep sym) 'ElispFeature)
          ((facep sym)    'ElispFace))))))

(defun +org|org-archive-done-tasks ()
  "Archive finished or cancelled tasks."
  (interactive)
  (org-map-entries
    (lambda ()
      (org-archive-subtree)
      (setq org-map-continue-from (outline-previous-heading)))
    "TODO=\"DONE\"|TODO=\"CANCELLED\"" (if (org-before-first-heading-p) 'file 'tree)))

(defun +org|yank-more ()
  (interactive)
  (insert "[[")
  (yank)
  (insert "][more]]"))

(defvar org-directory "~/io/")

(defun +org|setup-basic ()
  (setq-default
   org-log-into-drawer 1
   org-adapt-indentation nil
   org-edit-src-content-indentation 0
   org-log-done 'time
   org-ellipsis "  "
   org-pretty-entities t
   org-hide-emphasis-markers nil
   org-archive-mark-done nil
   org-image-actual-width nil
   org-hide-leading-stars t
   org-hide-leading-stars-before-indent-mode t
   org-tags-column 0
   org-todo-keywords
   '((sequence "[ ](t)" "[-](p)" "[?](m)" "|" "[X](d)")
     (sequence "TODO(T)" "|" "DONE(D)")
     (sequence "NEXT(n)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)"))
   org-todo-keyword-faces
   '(("[-]" :inherit font-lock-constant-face :weight bold)
     ("[?]" :inherit warning :weight bold)
     ("WAITING" :inherit default :weight bold)
     ("LATER" :inherit warning :weight bold))
   org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3))))

(defun +org|setup-ui ()
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (let* ((variable-tuple
          (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Source Sans Pro" :height 180 :weight light))))
   '(fixed-pitch ((t ( :family "Inconsolata" :slant normal :weight normal :height 1.0 :width normal)))))

  ;; (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'writing-mode)

  (custom-theme-set-faces
   'user
   '(org-block                 ((t (:inherit fixed-pitch))))
   '(org-document-info         ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-link                  ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value        ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim              ((t (:inherit (shadow fixed-pitch))))))
  )

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
            (tags-todo "-pause+TODO=\"DRAFT\""
                       ((org-agenda-overriding-header "WRITING")))
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

(use-package org-super-agenda
  :ensure t
  :after (org org-agenda)
  :quelpa (org-super-agenda :fetcher github :repo "alphapapa/org-super-agenda")
  :config
  (org-super-agenda-mode t)
  (setq org-super-agenda-groups
        '((:name "Important tasks ":priority "A")
          (:name "SynSIG" :tag "SynSIG")
          (:auto-category t)
          )))

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

(defun +org|setup-babel ()
  (setq
   org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.12/libexec/plantuml.jar"
   org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (gnuplot . t)
     (plantuml . t))))

(use-package org
  :ensure org-plus-contrib
  :init
  ;; (add-hook 'org-mode-hook 'flyspell-mode)
  :config
  (require 'org-tempo)
  (+org|setup-basic)
  (+org|setup-ui)
  (+org|setup-agenda)
  (+org|setup-capture)
  (+org|setup-babel)
  (defhydra hydra-org-subtree ()
    "subtree"
    ("q" nil "quit" :color: blue)
    ("j" org-move-subtree-down "down")
    ("k" org-move-subtree-up "promote")
    ("h" org-promote-subtree "promote")
    ("l" org-demote-subtree "demote"))
  :general
  (map|open
    "c" '(org-capture :which-key "Capture")
    "a" '(org-agenda :which-key "Agenda"))
  (map|local 'org-mode-map
    "A" '(+org|org-archive-done-tasks :which-key "Archive All")
    "a" '(org-archive-subtree-default :which-key "Archive Subtree")
    "b" '(org-insert-structure-template :which-key "Insert Block")
    "l" '(org-insert-link :which-key "Inert Link")
    "h" '(org-insert-heading-after-current :which-key "Inert Heading")
    "y" '(+org|yank-more :which-key "Yank More")
    "s" '(hydra-org-subtree/body :which-key "Subtree")
    "f" '(org-toggle-narrow-to-subtree :which-key "Toggle Focus")
    "t" '(org-todo :which-key "TODO")
    "T" '(org-show-todo-tree :which-key "Show TODOs")
    "p" '(org-tree-slide-mode :which-key "Present")))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-re-reveal
  :after org
  :config
  (setq
   org-reveal-mathjax t))

(add-hook 'org-load-hook #'+org|init-export)
(defun +org|init-export ()
  (setq org-export-backends '(ascii html latex md)
        org-publish-timestamp-directory (concat x/cache-dir "org-timestamps/"))

  (when (and (executable-find "pandoc")
             (require 'ox-pandoc nil t))
    (add-to-list 'org-export-backends 'pandoc nil #'eq)
    (setq org-pandoc-options
          '((standalone . t)
            (mathjax . t)
            (variable . "revealjs-url=https://cdn.jsdelivr.net/npm/reveal.js@3/")))))

(use-package org-download
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
  :quelpa (org-bullets :fetcher github :repo "Kaligule/org-bullets")
  :hook (org-mode . org-bullets-mode))

(use-package org-fancy-priorities
  :diminish
  :defines org-fancy-priorities-list
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(use-package org-tree-slide
  :commands (org-tree-slide-mode)
  :config
  (org-tree-slide-simple-profile)
  ;; (setq
  ;;   org-tree-slide-activate-message " "
  ;;   org-tree-slide-deactivate-message " "
  ;;   org-tree-slide-modeline-display nil)

  (add-hook 'org-tree-slide-mode-hook #'evil-normalize-keymaps)

  (general-define-key
    :states '(normal visual)
    :keymaps 'org-tree-slide-mode-map
    "q" 'org-tree-slide-mode
    "<up>" 'org-tree-slide-content
    "<down>" 'org-tree-slide-display-header-toggle
    "<left>" 'org-tree-slide-move-previous-tree
    "<right>" 'org-tree-slide-move-next-tree)
  )

(use-package htmlize
  :commands (htmlize-buffer
              htmlize-file
              htmlize-many-files
              htmlize-many-files-dired
              htmlize-region))

(use-package plantuml-mode
  :defer t
  :mode ("\\.pum\\'" . plantuml-mode)
  :config
  (setq plantuml-jar-path org-plantuml-jar-path))

(use-package gnuplot
  :defer t)

(use-package ox-hugo
  :after ox)

(use-package org-web-tools)

(use-package deft
  :commands deft
  :init
  (setq deft-extensions '("org")
    deft-default-extension "org"
    deft-directory "~/io"
    deft-recursive t
    ;; de-couples filename and note title:
    deft-use-filename-as-title t
    deft-use-filter-string-for-filename t
    deft-recursive-ignore-dir-regexp "\\(?:\\.\\|\\.\\.\\|www\\)$"
    ;; deft-ignore-file-regexp "\\(?:www/*\\)"
    ;; deft-recursive-ignore-dir-regexp "\\(?:www\\)"
    ;; deft-org-mode-title-prefix t
    ;; converts the filter string into a readable file-name using kebab-case:
    deft-file-naming-rules
    '((noslash . "-")
      (nospace . "-")
      (case-fn . downcase)))
  :config
  :general
  (map|open
    "n" '(deft :which-key "Deft")))
;; start filtering immediately
;; (set-evil-initial-state! 'deft-mode 'insert)
;; (map! :map deft-mode-map
;;       :localleader
;;       :n "RET" #'deft-new-file-named
;;       :n "a" #'deft-archive-file
;;       :n "c" #'deft-filter-clear
;;       :n "d" #'deft-delete-file
;;       :n "f" #'deft-find-file
;;       :n "g" #'deft-refresh
;;       :n "l" #'deft-filter
;;       :n "n" #'deft-new-file
;;       :n "r" #'deft-rename-file
;;       :n "s" #'deft-toggle-sort-method
;;       :n "t" #'deft-toggle-incremental-search))

(defvar-local +spellcheck-immediately t
  "If non-nil, spellcheck the current buffer upon starting `flyspell-mode'.

Since spellchecking can be slow in some buffers, this can be disabled with:

  (setq-hook! 'TeX-mode-hook +spellcheck-immediately nil)")

(use-package flyspell ; built-in
  :defer t
  :init (add-hook 'flyspell-mode-hook #'+spellcheck|immediately)
  :config
  (defun +spellcheck|immediately ()
    "Spellcheck the buffer when `flyspell-mode' is enabled."
    (when (and flyspell-mode +spellcheck-immediately)
      (flyspell-buffer))))

(use-package flyspell-correct
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic)
  :defer t
  :ensure nil
  :quelpa (flyspell-correct :fetcher github :repo "d12frosted/flyspell-correct")
  :init
  ;; (add-hook 'flyspell-mode-hook 'flyspell-popup-auto-correct-mode)
  :general
  (map!
    "s" '(hydra-spell/body :which-key "Spell"))
  :config
  ;; (require 'flyspell-correct-ivy)
  (defhydra hydra-spell (:hint t :color red)
    ("q" nil "quit" :color: blue)
    ("t" flyspell-mode "Toggle")
    ("j" flyspell-correct-next "Next")
    ("k" flyspell-correct-previous "Previous")
    ("c" flyspell-correct-wrapper "Correct"))
  ;; (require 'flyspell-correct-popup)
  ;; (setq flyspell-popup-correct-delay 0.8)
  ;; (define-key popup-menu-keymap [escape] #'keyboard-quit))
  )

(use-package flyspell-correct-ivy
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))


;; (use-package flyspell-correct-ivy
;;   :commands (flyspell-correct-ivy)
;;   :init
;;   (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package google-translate
  :init
  (setq google-translate-translation-directions-alist
    '(("en" . "zh-CN") ("zh-CN" . "en")))
  :config
  (require 'google-translate-smooth-ui)
  (map!
    "xt" 'google-translate-smooth-translate))

(use-package ledger-mode
  :mode "\\.journal\\'")

;; (use-package flycheck-ledger
;;   :after ledger-mode)

(use-package writeroom-mode
  :commands (writeroom-mode)
  :config
  (setq
    writeroom-major-modes '(text-mode org-mode)
    writeroom-global-effects '(visual-line-mode)
    writeroom-extra-line-spacing 0.3
    writeroom-restore-window-config t
    writeroom-width 100))

;;;###autoload
(defun writing-mode()
  "Enter writing mode."
  (interactive)
  (writeroom-mode 1)
  (blink-cursor-mode 1)
  )

(use-package magit
  :config
  (setq transient-default-level 5
    transient-levels-file  (concat x/etc-dir "transient/levels")
    transient-values-file  (concat x/etc-dir "transient/values")
    transient-history-file (concat x/etc-dir "transient/history")
    magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
    magit-diff-refine-hunk t) ; show granular diffs in selected hunk
  :general
  (map!
    "g" '(:ignore t :which-key "Git")
    "g s" '(magit-status :which-key "status")))

(use-package magit-gitflow
  :after magit
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package git-timemachine
  :config
  (setq git-timemachine-show-minibuffer-details t)
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)
  )

(use-package evil-magit
  :after magit
  :init
  (setq evil-magit-state 'normal
    evil-magit-use-z-for-folds t)
  :general
  (general-define-key
    :states '(normal visual)
    :keymaps 'magit-mode-map
    "%" #'magit-gitflow-popup))

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-require-colon nil)
  (define-key magit-todos-section-map "j" nil))
;; (advice-add #'magit-todos-mode :around #'doom*shut-up))

;; (use-package magithub
;;   :after magit
;;   :preface
;;   ;; Magithub is not well-behaved, so this needs to be set early
;;   (setq magithub-dir (concat doom-etc-dir "magithub/"))
;;   :init
;;   (setq magithub-clone-default-directory "~/"
;;         magithub-preferred-remote-method 'clone_url)
;;   :config
;;   (unless +magit-hub-enable-by-default
;;     ;; Disable magit by default. Can be enabled through magithub settings popup,
;;     ;; or setting `+magit-hub-enable-by-default'.
;;     (advice-add #'magithub-enabled-p :override #'+magit*hub-enabled-p)
;;     ;; I don't use `magithub-settings--simple' to redefine this because it
;;     ;; changes the order of settings. Obnoxious, but the alternative is even
;;     ;; more so.
;;     (advice-add #'magithub-settings--format-magithub.enabled
;;                 :override #'+magit*hub-settings--format-magithub.enabled))
;;   (when +magit-hub-features
;;     (magithub-feature-autoinject +magit-hub-features)))

(use-package gitignore-mode)

(use-package forge
  :after magit
  :init
  (setq forge-database-file (concat x/etc-dir "forge/forge-database.sqlite")))

;;;###autoload
(defun doom-project-name (&optional dir)
  "Return the name of the current project."
  (let ((project-root (or (projectile-project-root dir)
                          (if dir (expand-file-name dir)))))
    (if project-root
        (funcall projectile-project-name-function project-root)
      "-")))

(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :commands (projectile-project-root projectile-project-name projectile-project-p)
  :init
  (setq
   projectile-cache-file (concat x/cache-dir "projectile.cache")
   projectile-known-projects-file (concat x/cache-dir "projectile.projects")
   projectile-completion-system 'ivy
   projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" x/local-dir))
  :bind-keymap
  ("s-p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode)
  :general
  (map!
    "p" '(:ignore t :which-key "Projects")
    "/" '(counsel-projectile-ag :which-key "find file in project")
    "p p" '(projectile-switch-project :which-key "Open Project")
    "p f" '(projectile-find-file :which-key "Find File")))

;;;###autoload
(defalias 'doom-project-root #'projectile-project-root)

(defun +treemacs--init ()
  (require 'treemacs)
  (let ((origin-buffer (current-buffer)))
    (cl-letf (((symbol-function 'treemacs-workspace->is-empty?)
               (symbol-function 'ignore)))
      (treemacs--init))
    (dolist (project (treemacs-workspace->projects (treemacs-current-workspace)))
      (treemacs-do-remove-project-from-workspace project))
    (with-current-buffer origin-buffer
      (let ((project-root (or (doom-project-root) default-directory)))
        (treemacs-do-add-project-to-workspace
         (treemacs--canonical-path project-root)
         (doom-project-name project-root)))
      (setq treemacs--ready-to-follow t)
      (when (or treemacs-follow-after-init treemacs-follow-mode)
        (treemacs--follow)))))

;;;###autoload
(defun +treemacs/toggle ()
  "Initialize or toggle treemacs.

Ensures that only the current project is present and all other projects have
been removed.

Use `treemacs' command for old functionality."
  (interactive)
  (require 'treemacs)
  (pcase (treemacs-current-visibility)
    (`visible (delete-window (treemacs-get-local-window)))
    (_ (+treemacs--init))))

;;;###autoload
(defun +treemacs/find-file (arg)
  "Open treemacs (if necessary) and find current file."
  (interactive "P")
  (let ((origin-buffer (current-buffer)))
    (+treemacs--init)
    (with-current-buffer origin-buffer
      (treemacs-find-file arg))))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
      treemacs-deferred-git-apply-delay   0.5
      treemacs-display-in-side-window     t
      treemacs-file-event-delay           5000
      treemacs-file-follow-delay          0.2
      treemacs-follow-after-init          t
      treemacs-follow-recenter-distance   0.1
      treemacs-goto-tag-strategy          'refetch-index
      treemacs-indentation                2
      treemacs-indentation-string         " "
      treemacs-is-never-other-window      nil
      treemacs-no-png-images              nil
      treemacs-project-follow-cleanup     nil
      treemacs-persist-file               (expand-file-name "treemacs-persist" x/local-dir)
      treemacs-recenter-after-file-follow nil
      treemacs-recenter-after-tag-follow  nil
      treemacs-show-hidden-files          t
      treemacs-silent-filewatch           nil
      treemacs-silent-refresh             nil
      treemacs-sorting                    'alphabetic-desc
      treemacs-space-between-root-nodes   t
      treemacs-tag-follow-cleanup         t
      treemacs-tag-follow-delay           1.5
      treemacs-width                      35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
         (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :general
  (map!
    "t" '(:ignore t :which-key "Toggle")
    "t t" '(+treemacs/toggle :which-key "Treemacs"))

  ;; :bind
  ;; (:map global-map
  ;;  ("M-0"       . treemacs-select-window)
  ;;  ("C-x t 1"   . treemacs-delete-other-windows)
  ;;  ("C-x t t"   . treemacs)
  ;;  ("C-x t B"   . treemacs-bookmark)
  ;;  ("C-x t C-t" . treemacs-find-file)
  ;;  ("C-x t M-t" . treemacs-find-tag))
  )

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package yasnippet)

;;;###autoload
(defun +flycheck|init-popups ()
  "Activate `flycheck-posframe-mode' if available and in GUI Emacs.
Activate `flycheck-popup-tip-mode' otherwise.
Do nothing if `lsp-ui-mode' is active and `lsp-ui-sideline-enable' is non-nil."
  (unless (and (bound-and-true-p lsp-ui-mode)
               lsp-ui-sideline-enable)
    (if (and (fboundp 'flycheck-posframe-mode)
             (display-graphic-p))
        (flycheck-posframe-mode +1)
      (flycheck-popup-tip-mode +1))))

(use-package flycheck
  :init (global-flycheck-mode)
  :general
  (map!
    "e" '(hydra-flycheck/body :which-key "Errors"))
  :config
  ;; Emacs feels snappier without checks on newline
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (global-flycheck-mode +1)
  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
     :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
     :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("j"  flycheck-next-error                                       "Next")
    ("k"  flycheck-previous-error                                   "Previous")
    ("gg" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil))
  )

;; (use-package flycheck-popup-tip
;;   :commands (flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup)
;;   :init (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)
;;   :config (setq flycheck-popup-tip-error-prefix "✕ "))

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :init (add-hook 'flycheck-mode-hook #'+flycheck|init-popups)
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;; (use-package flycheck-posframe
;;   :commands flycheck-posframe-show-posframe
;;   :config
;;   (setq flycheck-posframe-warning-prefix "⚠ "
;;         flycheck-posframe-info-prefix "··· "
;;         flycheck-posframe-error-prefix "✕ "))

(use-package lsp-mode
  ;; :quelpa (lsp-mode :fetcher github :repo "emacs-lsp/lsp-mode")
  :init
  (setq
   lsp-session-file (concat x/etc-dir "lsp-session")
   lsp-auto-guess-root t
   lsp-keep-workspace-alive nil
   lsp-eldoc-render-all nil
   lsp-inhibit-message t
   lsp-message-project-root-warning t)
  (add-hook 'kill-emacs-hook (setq lsp-restart 'ignore))

  :config
  (require 'lsp-clients)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

  (defhydra hydra-lsp (:exit t :hint nil)
    "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
    ("d" lsp-find-declaration)
    ("D" lsp-ui-peek-find-definitions)
    ("R" lsp-ui-peek-find-references)
    ("i" lsp-ui-peek-find-implementation)
    ("t" lsp-find-type-definition)
    ("s" lsp-signature-help)
    ("o" lsp-describe-thing-at-point)
    ("r" lsp-rename)

    ("f" lsp-format-buffer)
    ("m" lsp-ui-imenu)
    ("x" lsp-execute-code-action)

    ("M-s" lsp-describe-session)
    ("M-r" lsp-restart-workspace)
    ("S" lsp-shutdown-workspace))

  (map!
    "l" '(hydra-lsp/body :which-key "LSP"))
  )

(use-package lsp-ui
  :after (lsp)
  :config
  (setq
   lsp-prefer-flymake nil
   lsp-ui-doc-max-height 8
   lsp-ui-doc-max-width 35
   lsp-ui-flycheck-live-reporting nil
   lsp-ui-sideline-enable nil
   lsp-ui-sideline-show-diagnostics nil
   lsp-ui-sideline-ignore-duplicate t)
  )

(use-package company-lsp
  ;; :quelpa (company-lsp :fetcher github :repo "tigersoldier/company-lsp")
  :after lsp-mode
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-async t))

(use-package highlight-quoted)

;; redefines the silly indent of keyword lists
;; before
;;   (:foo bar
;;         :baz qux)
;; after
;;   (:foo bar
;;    :baz qux)
(with-eval-after-load "lisp-mode"
  (defun lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (let ((normal-indent (current-column))
      (orig-point (point)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond
       ;; car of form doesn't seem to be a symbol, or is a keyword
       ((and (elt state 2)
         (or (not (looking-at "\\sw\\|\\s_"))
         (looking-at ":")))
    (if (not (> (save-excursion (forward-line 1) (point))
            calculate-lisp-indent-last-sexp))
        (progn (goto-char calculate-lisp-indent-last-sexp)
           (beginning-of-line)
           (parse-partial-sexp (point)
                       calculate-lisp-indent-last-sexp 0 t)))
    ;; Indent under the list or under the first sexp on the same
    ;; line as calculate-lisp-indent-last-sexp.  Note that first
    ;; thing on that line has to be complete sexp since we are
    ;; inside the innermost containing sexp.
    (backward-prefix-chars)
    (current-column))
       ((and (save-excursion
           (goto-char indent-point)
           (skip-syntax-forward " ")
           (not (looking-at ":")))
         (save-excursion
           (goto-char orig-point)
           (looking-at ":")))
    (save-excursion
      (goto-char (+ 2 (elt state 1)))
      (current-column)))
       (t
    (let ((function (buffer-substring (point)
                      (progn (forward-sexp 1) (point))))
          method)
      (setq method (or (function-get (intern-soft function)
                     'lisp-indent-function)
               (get (intern-soft function) 'lisp-indent-hook)))
      (cond ((or (eq method 'defun)
             (and (null method)
              (> (length function) 3)
              (string-match "\\`def" function)))
         (lisp-indent-defform state indent-point))
        ((integerp method)
         (lisp-indent-specform method state
                       indent-point normal-indent))
        (method
         (funcall method indent-point state))))))))
  ;; rainbow the lisp
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)
  )

(use-package js2-mode
  :mode "\\.\\(js\\|snap\\)\\'"
  :interpreter "node"
  :init
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  :config
  (setq js2-skip-preprocessor-directives t
    js-chain-indent t
    ;; let flycheck handle this
    js2-mode-show-parse-errors nil
    js2-mode-show-strict-warnings nil
    ;; Flycheck provides these features, so disable them: conflicting with
    ;; the eslint settings.
    js2-strict-trailing-comma-warning nil
    js2-strict-missing-semi-warning nil
    ;; maximum fontification
    js2-highlight-level 3
    js2-highlight-external-variables t)
  (add-hook 'js2-mode-hook #'rainbow-delimiters-mode)

  ;; typescript-language-server does not take prefix in count, filter here
  (defun x|company-transformer (candidates)
    (let ((completion-ignore-case t))
      (all-completions (company-grab-symbol) candidates)))
  (defun x|js-hook nil
    (make-local-variable 'company-transformers)
    (push 'x|company-transformer company-transformers))
  (add-hook 'js-mode-hook 'x|js-hook)

  (with-eval-after-load 'lsp-clients
    (add-hook 'js2-mode-hook #'lsp)))

(use-package rjsx-mode
  :mode "components/.+\\.js$"
  :init
  (defun +javascript-jsx-file-p ()
    "Detect React or preact imports early in the file."
    (and buffer-file-name
      (string= (file-name-extension buffer-file-name) "js")
      (re-search-forward "\\(^\\s-*import +React\\|\\( from \\|require(\\)[\"']p?react\\)"
        magic-mode-regexp-match-limit t)
      (progn (goto-char (match-beginning 1))
        (not (sp-point-in-string-or-comment)))))
  (add-to-list 'magic-mode-alist '(+javascript-jsx-file-p . rjsx-mode))
  :config
  ;; (set-electric! 'rjsx-mode :chars '(?\} ?\) ?. ?>))
  ;; (when (featurep! :feature syntax-checker)
  ;;   (add-hook! 'rjsx-mode-hook
  ;;     ;; jshint doesn't know how to deal with jsx
  ;;     (push 'javascript-jshint flycheck-disabled-checkers)))

  ;; ;; `rjsx-electric-gt' relies on js2's parser to tell it when the cursor is in
  ;; ;; a self-closing tag, so that it can insert a matching ending tag at point.
  ;; ;; However, the parser doesn't run immediately, so a fast typist can outrun
  ;; ;; it, causing tags to stay unclosed, so we force it to parse.
  ;; (defun +javascript|reparse (n)
  ;;   ;; if n != 1, rjsx-electric-gt calls rjsx-maybe-reparse itself
  ;;   (if (= n 1) (rjsx-maybe-reparse)))
  ;; (advice-add #'rjsx-electric-gt :before #'+javascript|reparse)
  (add-hook 'rjsx-mode-hook #'lsp)
  )

(use-package add-node-modules-path
  :config
  (progn
    (eval-after-load 'js2-mode
      '(add-hook 'js2-mode-hook #'add-node-modules-path))))

(use-package emmet-mode
  :preface (defvar emmet-mode-keymap (make-sparse-keymap))
  :hook (css-mode web-mode html-mode haml-mode nxml-mode rjsx-mode reason-mode)
  :config
  (when (require 'yasnippet nil t)
    (add-hook 'emmet-mode-hook #'yas-minor-mode-on))
  (setq emmet-move-cursor-between-quotes t)
  :general
  (:keymaps 'emmet-mode-keymap
   :states '(visual)
   "TAB" #'emmet-wrap-with-markup)
  (:keymaps 'emmet-mode-keymap
   :states '(insert)
   "TAB" #'emmet-expand-line)
  )
;; (setq-hook! 'rjsx-mode-hook emmet-expand-jsx-className? t)
;; (map! :map emmet-mode-keymap
;; 	:v [tab] #'emmet-wrap-with-markup
;; 	:i [tab] #'+web/indent-or-yas-or-emmet-expand
;; 	:i "M-E" #'emmet-expand-line))


(use-package web-mode
  :mode "\\.p?html?$")

(use-package markdown-mode
  :mode ("/README\\(?:\\.\\(?:markdown\\|md\\)\\)?\\'" . gfm-mode)
  :init
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh")
        markdown-fontify-code-blocks-natively t
        markdown-hide-urls nil ; trigger with `markdown-toggle-url-hiding'
        markdown-enable-math t ; syntax highlighting for latex fragments
        markdown-gfm-uppercase-checkbox t) ; for compat with org-mode
  :config)

(use-package fish-mode)
(defvar eshell-directory-name (concat x/etc-dir "eshell"))
(use-package company-shell
  :after sh-script)

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rustic
  :config
  (setq rustic-indent-method-chain t
    rustic-flycheck-setup-mode-line-p nil
    ;; use :editor format instead
    rustic-format-on-save nil))

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package lua-mode)

(use-package swift-mode)

(use-package elm-mode
  :config
  (setq elm-format-on-save t))

(use-package flycheck-elm
  :after elm-mode
  :config (add-to-list 'flycheck-checkers 'elm nil #'eq))

(use-package dockerfile-mode)

(use-package docker)
