(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  ;; Emacs feels snappier without checks on newline
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)))

(provide 'init-flycheck)
