(use-package company
  :ensure t
  :init (global-company-mode))

(use-package company-box
  :when EMACS26+
  :ensure t
  :hook (company-mode . company-box-mode))

(provide 'init-company)
