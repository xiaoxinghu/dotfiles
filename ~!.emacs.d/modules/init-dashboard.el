(use-package dashboard
  :init
  (setq dashboard-items '((recents  . 5)
                           (agenda . 5)
                           ;; (bookmarks . 5)
                           (projects . 5)
                           ;; (registers . 5)
                           ))
  (setq dashboard-startup-banner 3)
  :config
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)
