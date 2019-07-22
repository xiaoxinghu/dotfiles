(use-package flycheck-rust)

(use-package rustic
  :config
  (setq rustic-indent-method-chain t
    rustic-flycheck-setup-mode-line-p nil
    ;; use :editor format instead
    rustic-format-on-save nil))

(provide 'init-rust)
