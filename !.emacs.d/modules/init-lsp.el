(use-package lsp-mode
  ;; :quelpa (lsp-mode :fetcher github :repo "emacs-lsp/lsp-mode")
  :init
  (setq
    lsp-session-file (concat x/etc-dir "lsp-session")
    lsp-auto-guess-root t
    lsp-keep-workspace-alive nil
    lsp-eldoc-render-all nil
    lsp-inhibit-message t
    lsp-message-project-root-warning t)
  (add-hook 'kill-emacs-hook (setq lsp-restart 'ignore))

  :config
  (require 'lsp-clients)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  )

(use-package lsp-ui
  :after (lsp)
  :config
  (setq
    lsp-prefer-flymake nil
    lsp-ui-doc-max-height 8
    lsp-ui-doc-max-width 35
    lsp-ui-sideline-ignore-duplicate t)
  )

(use-package company-lsp
  ;; :quelpa (company-lsp :fetcher github :repo "tigersoldier/company-lsp")
  :after lsp-mode
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-async t))

(provide 'init-lsp)
