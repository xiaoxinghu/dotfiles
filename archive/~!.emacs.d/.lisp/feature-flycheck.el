;;;###autoload
(defun +flycheck|init-popups ()
  "Activate `flycheck-posframe-mode' if available and in GUI Emacs.
Activate `flycheck-popup-tip-mode' otherwise.
Do nothing if `lsp-ui-mode' is active and `lsp-ui-sideline-enable' is non-nil."
  (unless (and (bound-and-true-p lsp-ui-mode)
               lsp-ui-sideline-enable)
    (if (and (fboundp 'flycheck-posframe-mode)
             (display-graphic-p))
        (flycheck-posframe-mode +1)
      (flycheck-popup-tip-mode +1))))

(use-package flycheck
  :init (global-flycheck-mode)
  :general
  (map!
    "e" '(hydra-flycheck/body :which-key "Errors"))
  :config
  ;; Emacs feels snappier without checks on newline
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (global-flycheck-mode +1)
  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
     :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
     :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("j"  flycheck-next-error                                       "Next")
    ("k"  flycheck-previous-error                                   "Previous")
    ("gg" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil))
  )

;; (use-package flycheck-popup-tip
;;   :commands (flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup)
;;   :init (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)
;;   :config (setq flycheck-popup-tip-error-prefix "✕ "))

(use-package flycheck-posframe
  :after flycheck
  :init (add-hook 'flycheck-mode-hook #'+flycheck|init-popups)
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;; (use-package flycheck-posframe
;;   :commands flycheck-posframe-show-posframe
;;   :config
;;   (setq flycheck-posframe-warning-prefix "⚠ "
;;         flycheck-posframe-info-prefix "··· "
;;         flycheck-posframe-error-prefix "✕ "))

(provide 'feature-flycheck)
