(use-package emmet-mode
  :ensure t
  :preface (defvar emmet-mode-keymap (make-sparse-keymap))
  :hook (css-mode web-mode html-mode haml-mode nxml-mode rjsx-mode reason-mode)
  :config
  (when (require 'yasnippet nil t)
    (add-hook 'emmet-mode-hook #'yas-minor-mode-on))
  (setq emmet-move-cursor-between-quotes t)
  :general
  (:keymaps 'emmet-mode-keymap
   :states '(visual)
   "TAB" #'emmet-wrap-with-markup)
  (:keymaps 'emmet-mode-keymap
   :states '(insert)
   "TAB" #'emmet-expand-line)
  )
;; (setq-hook! 'rjsx-mode-hook emmet-expand-jsx-className? t)
;; (map! :map emmet-mode-keymap
;; 	:v [tab] #'emmet-wrap-with-markup
;; 	:i [tab] #'+web/indent-or-yas-or-emmet-expand
;; 	:i "M-E" #'emmet-expand-line))


(use-package web-mode
  :ensure t
  :mode "\\.p?html?$")

(provide 'init-web)
