;; Sane defaults
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

;; (use-package zenburn-theme
;;   :config
;;   (load-theme 'zenburn t))

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;(doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Enable flashing mode-line on errors
  ;;(doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package solaire-mode
  :hook ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  :config
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (solaire-mode-swap-bg))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package hl-line ; built in
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
  :config
  ;; I don't need hl-line showing in other windows. This also offers a small
  ;; speed boost when buffer is displayed in multiple windows.
  (setq hl-line-sticky-flag nil
	global-hl-line-sticky-flag nil))

(use-package linum-mode
  :ensure nil
  :general
  (map!
    "t" '(:ignore t :which-key "Toggle")
    "t l" '(linum-mode :which-key "Line Number")))

(use-package doom-modeline
  :defer t
  :config
  :hook (after-init . doom-modeline-init))

(provide 'ui)
