;;; Code:
(require 'core)

;; sane default
(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `(("." . ,temporary-file-directory)) )
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix
      (concat x/local-dir "auto-save-list/.saves-"))
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message "") ; print a default message in the empty scratch buffer opened at startup

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
