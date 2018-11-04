;;; Code:

(require 'core)

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
    "p p" '(projectile-switch-project :which-key "Open Project")
    "p f" '(projectile-find-file :which-key "Find File")))

(provide 'init-projectile)
;;; init-projectile ends here
