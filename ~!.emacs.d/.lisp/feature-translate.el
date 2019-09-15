(use-package google-translate
  :init
  (setq google-translate-translation-directions-alist
        '(("en" . "zh-CN") ("zh-CN" . "en")))
  :config
  (require 'google-translate-smooth-ui)
  (map!
    "xt" 'google-translate-smooth-translate))

(provide 'feature-translate)
