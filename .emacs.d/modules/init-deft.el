(use-package deft
  :commands deft
  :init
  (setq deft-extensions '("org")
	deft-default-extension "org"
	deft-directory "~/io"
	deft-recursive t
	;; de-couples filename and note title:
	deft-use-filename-as-title t
	deft-use-filter-string-for-filename t
	;; deft-org-mode-title-prefix t
	;; converts the filter string into a readable file-name using kebab-case:
	deft-file-naming-rules
	'((noslash . "-")
	  (nospace . "-")
	  (case-fn . downcase)))
  :config
  :general
  (map|open
    "n" '(deft :which-key "Deft")))
;; start filtering immediately
;; (set-evil-initial-state! 'deft-mode 'insert)
;; (map! :map deft-mode-map
;;       :localleader
;;       :n "RET" #'deft-new-file-named
;;       :n "a" #'deft-archive-file
;;       :n "c" #'deft-filter-clear
;;       :n "d" #'deft-delete-file
;;       :n "f" #'deft-find-file
;;       :n "g" #'deft-refresh
;;       :n "l" #'deft-filter
;;       :n "n" #'deft-new-file
;;       :n "r" #'deft-rename-file
;;       :n "s" #'deft-toggle-sort-method
;;       :n "t" #'deft-toggle-incremental-search))

(provide 'init-deft)
