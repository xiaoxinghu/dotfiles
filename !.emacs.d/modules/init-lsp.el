(use-package lsp-mode
  ;; :quelpa (lsp-mode :fetcher github :repo "emacs-lsp/lsp-mode")
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
  )

(use-package lsp-ui
  :after (lsp))

(use-package company-lsp
  ;; :quelpa (company-lsp :fetcher github :repo "tigersoldier/company-lsp")
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-async t))

(provide 'init-lsp)
