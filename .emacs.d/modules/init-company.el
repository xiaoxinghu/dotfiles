(use-package company
  :init (global-company-mode))

(use-package company-box
  :when EMACS26+
  :hook (company-mode . company-box-mode))

(provide 'init-company)
