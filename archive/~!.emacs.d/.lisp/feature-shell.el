(use-package fish-mode)
(defvar eshell-directory-name (concat x/etc-dir "eshell"))
(use-package company-shell
  :after sh-script)

(provide 'feature-shell)
