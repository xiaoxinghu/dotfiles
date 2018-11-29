(use-package magit
  :general
  (map!
    "g" '(:ignore t :which-key "Git")
    "g s" '(magit-status :which-key "status")))

(use-package magit-gitflow
  :after magit
  :hook (magit-mode . turn-on-magit-gitflow))

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

(provide 'init-magit)
