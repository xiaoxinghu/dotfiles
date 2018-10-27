(defvar-local +spellcheck-immediately t
  "If non-nil, spellcheck the current buffer upon starting `flyspell-mode'.

Since spellchecking can be slow in some buffers, this can be disabled with:

  (setq-hook! 'TeX-mode-hook +spellcheck-immediately nil)")

(use-package flyspell ; built-in
  :defer t
  :init (add-hook 'flyspell-mode-hook #'+spellcheck|immediately)
  :config
  (defun +spellcheck|immediately ()
    "Spellcheck the buffer when `flyspell-mode' is enabled."
    (when (and flyspell-mode +spellcheck-immediately)
      (flyspell-buffer))))

(use-package flyspell-correct
  :ensure t
  :commands (flyspell-correct-word-generic
	     flyspell-correct-previous-word-generic)
  :config)
  ;; (require 'flyspell-correct-popup)
  ;; (setq flyspell-popup-correct-delay 0.8)
  ;; (define-key popup-menu-keymap [escape] #'keyboard-quit))

(use-package flyspell-correct-ivy
  :ensure t
  :commands (flyspell-correct-ivy)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package flyspell-popup
  :defer t
  :ensure t
  :init
  (progn
    (setq flyspell-popup-correct-delay 0.8)
    (add-hook 'flyspell-mode-hook 'flyspell-popup-auto-correct-mode)))

;; (use-package flyspell-correct-popup
;;   :ensure t
;;   :commands (flyspell-correct-popup)
;;   :init
;;   (setq flyspell-correct-interface #'flyspell-correct-popup))

(provide 'init-spell)
