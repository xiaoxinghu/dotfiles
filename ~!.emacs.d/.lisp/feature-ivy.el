(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-selectable-prompt t
        ivy-virtual-abbreviate 'full
        projectile-completion-system 'ivy
        ivy-on-del-error-function nil
        )
  (bind-key "C-c C-r" 'ivy-resume))

;;;###autoload
(defun +ivy-rich-buffer-icon (candidate)
  "Display the icon for CANDIDATE buffer."
  ;; NOTE This is inspired by `all-the-icons-ivy-buffer-transformer', minus the
  ;; buffer name and extra padding as those are handled by `ivy-rich'.
  (propertize "\t" 'display
              (if-let* ((buffer (get-buffer candidate))
                        (mode (buffer-local-value 'major-mode buffer)))
                  (or
                   (all-the-icons-ivy--icon-for-mode mode)
                   (all-the-icons-ivy--icon-for-mode (get mode 'derived-mode-parent))
                   (funcall
                    all-the-icons-ivy-family-fallback-for-buffer
                    all-the-icons-ivy-name-fallback-for-buffer))
                (all-the-icons-icon-for-file candidate))))

(use-package all-the-icons-ivy)

(use-package ivy-rich
  :after ivy
  :config
  (cl-pushnew '(+ivy-rich-buffer-icon)
                (cadr (plist-get ivy-rich-display-transformers-list
                                 'ivy-switch-buffer)))
  (ivy-rich-mode +1))

(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode)
  (setq prescient-save-file (concat x/var-dir "pres
-save.el")))

(use-package counsel
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
  (map|global
    "M-o" 'counsel-find-file
    "M-F" 'counsel-projectile-ag
    "M-b" 'counsel-switch-buffer
    "M-p" 'counsel-projectile-find-file
    "M-P" 'counsel-projectile-switch-project
    "M-x" 'counsel-M-x
    "C-c k" 'counsel-ag))

(use-package swiper
  :general
  (map|global
    "M-f" 'swiper))

(provide 'feature-ivy)
