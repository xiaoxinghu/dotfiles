(use-package magit
  :ensure t
  :general
  (map!
    "g" '(:ignore t :which-key "Git")
    "g s" '(magit-status :which-key "status")))

(use-package magit-gitflow
  :ensure t
  :after magit
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package evil-magit
  :ensure t
  :after magit
  :init
  (setq evil-magit-state 'normal
	evil-magit-use-z-for-folds t)
  :general
  (general-define-key
   :states '(normal visual)
   :keymaps 'magit-mode-map
   "%" #'magit-gitflow-popup))

(provide 'init-magit)
