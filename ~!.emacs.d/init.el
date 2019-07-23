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

(defvar doom-gc-cons-threshold 16777216 ; 16mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this. If you experience stuttering, increase this.")

(defvar doom-gc-cons-upper-limit 268435456 ; 256mb
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar doom--file-name-handler-alist file-name-handler-alist)

(defun doom|restore-startup-optimizations ()
  "Reset garbage collection settings to reasonable defaults.
A large `gc-cons-threshold' can cause random freezes otherwise and
resets `file-name-handler-alist'."
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

(setq user-full-name "Xiaoxing Hu"
      user-mail-address "xiaoxing@huxx.org")

(defconst EMACS26+ (> emacs-major-version 25))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defvar x/local-dir (expand-file-name ".local/" user-emacs-directory)
  "Root directory for local Emacs files.
     Use this as permanent storage for files
     that are safe to share across systems (if this config is symlinked across
     several computers).")
(defvar x/packages-dir (concat x/local-dir "packages/")
  "Where package.el and quelpa plugins (and their caches) are stored.")

(defvar x/cache-dir (concat x/local-dir "cache/")
  "Directory for volatile storage.

     Use this for files that change often, like cache files.")

(defvar x/etc-dir (concat x/local-dir "etc/")
  "Directory for non-volatile storage.

     Use this for files that don't change much, like servers binaries, external
     dependencies or long-term shared data.")

(defvar x/var-dir (concat x/local-dir "var/")
  "Directory for volatile storage.")

(make-directory x/local-dir :parents)
(setq custom-file (concat x/local-dir "custom.el"))

(setq-default
  abbrev-file-name             (concat x/local-dir "abbrev.el")
  auto-save-list-file-name     (concat x/cache-dir "autosave")
  backup-directory-alist       (list (cons "." (concat x/cache-dir "backup/")))
  pcache-directory             (concat x/cache-dir "pcache/")
  request-storage-directory    (concat x/cache-dir "request")
  server-auth-dir              (concat x/cache-dir "server/")
  shared-game-score-directory  (concat x/etc-dir "shared-game-score/")
  tramp-auto-save-directory    (concat x/cache-dir "tramp-auto-save/")
  tramp-backup-directory-alist backup-directory-alist
  tramp-persistency-file-name  (concat x/cache-dir "tramp-persistency.el")
  url-cache-directory          (concat x/cache-dir "url/")
  url-configuration-directory  (concat x/etc-dir "url/")
  gamegrid-user-score-file-directory (concat x/etc-dir "games/"))

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system        'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; perdy
(setq locale-coding-system   'utf-8)   ; please
(setq-default buffer-file-coding-system 'utf-8) ; with sugar on top

(setq-default
 ;; be quiet at startup; don't load or display anything unnecessary
 inhibit-startup-message t
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil
 find-file-visit-truename t       ; resolve symlinks when opening files
 ;; History & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil  ; don't create backup~ files
 ;; Don't store authinfo in plain text!
 auth-sources (list (expand-file-name "authinfo.gpg" x/etc-dir)
                    "~/.authinfo.gpg"))

(require 'package)

(defvar x/core-packages '(use-package quelpa)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/"))
      ;; quelpa-stable-p t
      quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-melpa-recipe-stores nil
      ;; use-package-ensure-function 'quelpa
      use-package-always-ensure t
      package-user-dir (expand-file-name "elpa" x/packages-dir)
      quelpa-dir (expand-file-name "quelpa" x/packages-dir)
      )
(package-initialize)

;; bootstrap core packages
(defun x/ensure-core-packages ()
  "Make sure `x/core-packages' are installed."
  (let ((core-packages (cl-remove-if #'package-installed-p x/core-packages)))
    (unless (= (length core-packages) 0)
      (message "Installing core packages")
      (package-refresh-contents)
      (dolist (package core-packages)
        (package-install package)
        (if (package-installed-p package)
            (message "✓ Installed %s" package)
          (error "✕ Couldn't install %s" package)))
      (message "Installing core packages...done"))))

(x/ensure-core-packages)

(quelpa
 '(quelpa-use-package
   :stable nil
   :fetcher git
   :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))

(require 'quelpa-use-package)

(defvar doom-escape-hook nil
  "A hook run after C-g is pressed (or ESC in normal mode, for evil users). Both
trigger `doom/escape'.

If any hook returns non-nil, all hooks after it are ignored.")

(defun doom/escape ()
  "Run the `doom-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((cl-find-if #'funcall doom-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'doom/escape)

(use-package general
  :config
  (general-evil-setup)
  (general-create-definer map!
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (general-create-definer map|local
    :states '(normal visual insert emacs)
    :prefix "SPC m"
    :non-normal-prefix "C-SPC m")

  (general-create-definer map|open
    :states '(normal visual insert emacs)
    :prefix "SPC o"
    :non-normal-prefix "C-SPC o")

  (map!
    ;; simple command
    "u"   '(universal-argument :which-key "Universal argument")
    "'"   '(iterm-focus :which-key "iterm")
    "?"   '(iterm-goto-filedir-or-home :which-key "iterm - goto dir")
    "TAB" '(x|switch-to-other-buffer :which-key "prev buffer")

    ;; Applications
    "m" '(:ignore t :which-key "Major")
    "a" '(:ignore t :which-key "Applications")
    "o" '(:ignore t :which-key "Open")
    "ar" 'ranger
    "ad" 'dired
    "q" '(:ignore t :which-key "Quit")
    "qq" 'save-buffers-kill-terminal

    ;; Help
    "h" '(:ignore t :which-key "Help")
    "hf" 'describe-function
    "hv" 'describe-variable
    "hk" 'describe-key
    "hh" 'help-for-help
    )

  ;; conventions
  (general-define-key
   :keymaps 'global ;; use to be override
   "M-a" 'mark-whole-buffer
   "M-s" 'save-buffer
   "M-q" 'save-buffers-kill-terminal
   "M-b" 'counsel-ibuffer
   "M-w" 'delete-frame
   "M-o" 'ranger
   "M-p" 'counsel-projectile-find-file
   "M-P" 'counsel-projectile-switch-project
   "M-n" 'make-frame-command
   "M-v" 'yank
   "M-RET" 'toggle-frame-fullscreen)

  (global-set-key (kbd "M-`") 'x|switch-to-other-buffer)
  )

(use-package hydra
  ;; :bind (("s-b" . hydra-buffer/body))
  :general
  (map!
    "b" 'hydra-buffer/body
    "z" 'hydra-text-zoom/body)
  :config
  (hydra-add-font-lock)

  (defhydra hydra-buffer ()
    "buffer"
    ("b" counsel-ibuffer "buffers" :exit t)
    ("x" kill-this-buffer "kill buffer" :exit t)
    ("m" buffer-menu "buffer-menu" :exit t)
    ("h" switch-to-prev-buffer "prev")
    ("l" switch-to-next-buffer "next"))

  (defhydra hydra-text-zoom (:hint t :color red)
    "
      Text zoom: _j_:zoom in, _k_:zoom out, _0_:reset
"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("0" (text-scale-set 0) "reset"))

  (defhydra hydra-buffer-menu (:color pink
                               :hint nil)
    "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------                        (__)
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch                         (oo)
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch                      /------\\/
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur                 / |    ||
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only^^    *  /\\---/\\
_~_: modified      ^ ^                ^ ^                ^^                                 ~~   ~~
"
    ("m" Buffer-menu-mark)
    ("u" Buffer-menu-unmark)
    ("U" Buffer-menu-backup-unmark)
    ("d" Buffer-menu-delete)
    ("D" Buffer-menu-delete-backwards)
    ("s" Buffer-menu-save)
    ("~" Buffer-menu-not-modified)
    ("x" Buffer-menu-execute)
    ("b" Buffer-menu-bury)
    ("g" revert-buffer)
    ("T" Buffer-menu-toggle-files-only)
    ("O" Buffer-menu-multi-occur :color blue)
    ("I" Buffer-menu-isearch-buffers :color blue)
    ("R" Buffer-menu-isearch-buffers-regexp :color blue)
    ("c" nil "cancel")
    ("v" Buffer-menu-select "select" :color blue)
    ("o" Buffer-menu-other-window "other-window" :color blue)
    ("q" quit-window "quit" :color blue))

  (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)
  )

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(add-to-list 'default-frame-alist '(font . "Fira Code"))
(use-package all-the-icons)

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t))

;; (use-package tao-theme
;;   :ensure t
;;   :init
;;   (load-theme 'tao-yin t))
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;(doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (use-package poet-theme
;;   :config
;;   (add-hook 'text-mode-hook
;; 	    (lambda ()
;; 	      (variable-pitch-mode 1))))

(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package hl-line ; built in
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
  :config
  ;; I don't need hl-line showing in other windows. This also offers a small
  ;; speed boost when buffer is displayed in multiple windows.
  (setq hl-line-sticky-flag nil
    global-hl-line-sticky-flag nil))


(use-package doom-modeline
  :defer t
  :hook (after-init . doom-modeline-mode))

(use-package linum-mode
  :ensure nil
  :general
  (map!
    "t" '(:ignore t :which-key "Toggle")
    "t l" '(linum-mode :which-key "Line Number")))

(use-package doom-modeline
  :defer t
  :hook (after-init . doom-modeline-mode))

(setq-default
  vc-follow-symlinks t
  ;; Bookmarks
  bookmark-default-file (concat x/etc-dir "bookmarks")
  bookmark-save-flag t
  ;; Formatting
  delete-trailing-lines nil
  fill-column 80
  sentence-end-double-space nil
  word-wrap t

  ;; Scrolling
  hscroll-margin 2
  hscroll-step 1
  scroll-conservatively 1001
  scroll-margin 0
  scroll-preserve-screen-position t
  ;; Whitespace (see `editorconfig')
  indent-tabs-mode nil
  require-final-newline t
  tab-always-indent t
  tab-width 4
  tabify-regexp "^\t* [ \t]+" ; for :retab
  ;; Wrapping
  truncate-lines t
  truncate-partial-width-windows 50

  vc-make-backup-files nil
  ring-bell-function 'ignore
  fill-column 80)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package server
  :config
  (unless (server-running-p)
    (server-start)
    (message "start server")))

(use-package crux)

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns))
        (exec-path-from-shell-initialize)))

(use-package recentf
  :config
  (setq recentf-save-file (concat x/local-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-exclude
        (list #'file-remote-p "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
              "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$"
              ;; ignore private DOOM temp files (but not all of them)
              (lambda (file) (file-in-directory-p file x/local-dir))))
  (recentf-mode +1))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil
        sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0.1
        sp-max-pair-length 4
        sp-max-prefix-length 50
        sp-escape-quotes-after-insert nil)  ; not smart enough

  ;; Smartparens' navigation feature is neat, but does not justify how expensive
  ;; it is. It's also less useful for evil users. This may need to be
  ;; reactivated for non-evil users though. Needs more testing!
  (defun doom|disable-smartparens-navigate-skip-match ()
    (setq sp-navigate-skip-match nil
          sp-navigate-consider-sgml-tags nil))
  (add-hook 'after-change-major-mode-hook #'doom|disable-smartparens-navigate-skip-match)

  ;; autopairing in `eval-expression' and `evil-ex'
  (defun doom|init-smartparens-in-eval-expression ()
    "Enable `smartparens-mode' in the minibuffer, during `eval-expression' or
`evil-ex'."
    (when (memq this-command '(eval-expression evil-ex))
      (smartparens-mode)))
  (add-hook 'minibuffer-setup-hook #'doom|init-smartparens-in-eval-expression)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

  ;; smartparens breaks evil-mode's replace state
  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook  #'turn-on-smartparens-mode)

  (smartparens-global-mode +1))

(use-package rainbow-delimiters)

;;;###autoload
(defun x|switch-to-other-buffer ()
  "to the other buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

;;;###autoload
(defun x|yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun x|recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch
Emacs versions."
  (interactive)
  (byte-recompile-directory package-user-dir nil t))

(provide 'autoloads)

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

(defvar org-directory "~/io/")

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

(defun +org|setup-basic ()
  (setq-default
    org-log-into-drawer 1
    org-adapt-indentation t
    org-log-done 'time
    org-ellipsis "  "
    org-pretty-entities t
    org-hide-emphasis-markers t
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

(use-package org-re-reveal
  :after org
  :config
  (setq
    org-reveal-mathjax t))

;; export
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

(use-package yasnippet)

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
