(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (bind-key "C-c C-r" 'ivy-resume))

(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode)
  (setq prescient-save-file (concat x/var-dir "pres
-save.el")))

(use-package counsel
  :bind
  ("M-F" . counsel-projectile-ag)
  :general
  (map|file
    "f" '(counsel-find-file :which-key "find file")
    "y" '(x|yank-buffer-filename :which-key "yank filename")
    "r" '(counsel-recentf :which-key "recent file")
    "R" '(crux-rename-buffer-and-file :which-key "rename file")
    "d" '(dired :which-key "dired")
    "D" '(crux-delete-file-and-buffer :which-key "delete file")
    ;; "b" '(:ignore t :which-key "Buffers")
    ;; "b b" '(counsel-ibuffer :which-key "ibuffer")
    ":" 'counsel-M-x)
  :bind
  ("M-x" . counsel-M-x)
  ("C-c k" . counsel-ag))

(use-package swiper
  :bind
  ("M-f" . swiper))

(provide 'feature-ivy)
