(use-package elm-mode
  :config
  (setq elm-format-on-save t))

(use-package flycheck-elm
  :after elm-mode
  :config (add-to-list 'flycheck-checkers 'elm nil #'eq))

(provide 'feature-elm)
