;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defun x|enable-company-with-yas ()
  (interactive)
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  )

(use-package yasnippet
  :config
  (yas-global-mode 1))
(provide 'feature-snippets)
