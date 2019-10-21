(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(add-to-list 'default-frame-alist '(font . "Fira Code"))
(use-package all-the-icons)

(use-package tao-theme
  :config
  (load-theme 'tao-yang t)
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

(use-package doom-modeline
  :defer t
  :hook (after-init . doom-modeline-mode))

(use-package linum-mode
  :ensure nil
  :general
  (map!
    "t" '(:ignore t :which-key "Toggle")
    "t l" '(linum-mode :which-key "Line Number")))

(use-package doom-modeline
  :defer t
  :hook (after-init . doom-modeline-mode))

(provide 'core-ui)
