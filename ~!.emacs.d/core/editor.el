;;; Code:
(require 'core)
;; sane default
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

;; recentf
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

;; avy
(use-package avy
  :commands (avy-goto-word-1)
  :general
  (map!
    "SPC" '(avy-goto-word-or-subword-1  :which-key "go to char")))

;; ivy, a generic completion mechanism for Emacs.
(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (bind-key "C-c C-r" 'ivy-resume))

;; Simple but effective sorting and filtering for Emacs.
(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode)
  (setq prescient-save-file (concat x/var-dir "pres
-save.el"))
  )

;; counsel, a collection of Ivy-enhanced versions of common Emacs commands.
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

;; swiper, an Ivy-enhanced alternative to isearch.
(use-package swiper
  :bind
  ("M-f" . swiper))

;; smartparens
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

;; https://editorconfig.org
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; rainbow
(use-package rainbow-delimiters)

(provide 'editor)
;;; editor ends here
