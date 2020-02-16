(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(add-to-list 'default-frame-alist '(font . "Fira Code"))
(use-package all-the-icons)

(use-package doom-themes
  :config
  (load-theme 'doom-material t)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;(doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

(use-package doom-modeline
  :defer t
  :hook (after-init . doom-modeline-mode)
  :config
  (size-indication-mode +1) ; filesize in modeline
  (column-number-mode +1)   ; cursor column in modeline
  )

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

(provide 'core-ui)
