(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rustic
  :config
  (setq rustic-indent-method-chain t
        rustic-flycheck-setup-mode-line-p nil
        ;; use :editor format instead
        rustic-format-on-save nil))

(provide 'feature-rust)
