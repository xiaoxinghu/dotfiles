(use-package projectile
  :ensure t
  :delight '(:eval (concat " " (projectile-project-name)))
  :init
  (setq
   projectile-completion-system 'ivy
   projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" x/local-dir))
  :bind-keymap
  ("s-p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :ensure
  :config
  (counsel-projectile-mode)
  :general
  (map!
    "p" '(:ignore t :which-key "Projects")
    "p p" '(projectile-switch-project :which-key "Open Project")
    "p f" '(projectile-find-file :which-key "Find File")))

(provide 'init-projectile)
