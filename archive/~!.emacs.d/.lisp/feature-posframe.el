(use-package posframe
  :config
  (setq posframe-arghandler #'hemacs-posframe-arghandler)
  (defun hemacs-posframe-arghandler (posframe-buffer arg-name value)
    (let ((info '(:internal-border-width 2 :min-width 80)))
      (or (plist-get info arg-name) value))))

(use-package ivy-posframe
  :requires ivy
  :custom
  (ivy-posframe-style 'point)
  (ivy-posframe-display-functions-alist
   '((swiper . ivy-posframe-display-at-window-bottom-left)
     (ivy-switch-buffer . ivy-posframe-display-at-window-center)
     (t . ivy-posframe-display)))
  (ivy-fixed-height-minibuffer nil)
  (ivy-posframe-border-width 10)
  (ivy-posframe-parameters
   `((min-width . 80)
     (min-height . ,ivy-height)))
  :config
  (ivy-posframe-mode))

(provide 'feature-posframe)
