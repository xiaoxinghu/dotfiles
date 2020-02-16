(use-package csharp-mode
  :init
  (add-hook 'csharp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'csharp-mode-local-vars-hook #'lsp-deferred))

(provide 'feature-csharp)
