(use-package lsp
  :ensure lsp-mode
  :config
  (require 'lsp-clients)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  :init
  (setq lsp-eldoc-render-all nil
    lsp-inhibit-message t
    lsp-message-project-root-warning t
    lsp-prefer-flymake nil
    lsp-message-project-root-warning t
    lsp-auto-guess-root t)
  (setq lsp-clients-typescript-server "typescript-language-server"
    lsp-clients-typescript-server-args '("--stdio"))
  )

(use-package lsp-ui
  :after (lsp))

(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(provide 'init-lsp)
