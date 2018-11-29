;; general
(use-package general
  :config
  (general-create-definer map!
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (general-create-definer map|local
    :states '(normal visual insert emacs)
    :prefix "SPC m"
    :non-normal-prefix "C-SPC m")

  (general-create-definer map|open
    :states '(normal visual insert emacs)
    :prefix "SPC o"
    :non-normal-prefix "C-SPC o")

  (map!
    ;; simple command
    "u"   '(universal-argument :which-key "Universal argument")
    "'"   '(iterm-focus :which-key "iterm")
    "?"   '(iterm-goto-filedir-or-home :which-key "iterm - goto dir")
    "TAB" '((switch-to-buffer (other-buffer)) :which-key "prev buffer")

    ;; Applications
    "m" '(:ignore t :which-key "Major")
    "a" '(:ignore t :which-key "Applications")
    "o" '(:ignore t :which-key "Open")
    "ar" 'ranger
    "ad" 'dired
    "q" '(:ignore t :which-key "Quit")
    "qq" 'save-buffers-kill-terminal))

(use-package hydra
  ;; :bind (("s-b" . hydra-buffer/body))
  :general
  (map!
    "b" 'hydra-buffer/body
    "z" 'hydra-text-zoom/body)
  :config
  (hydra-add-font-lock)

  (defhydra hydra-buffer ()
    "buffer"
    ("b" counsel-ibuffer "buffers" :exit t)
    ("m" buffer-menu "buffer-menu" :exit t)
    ("h" switch-to-prev-buffer "prev")
    ("l" switch-to-next-buffer "next"))

  (defhydra hydra-text-zoom (:hint t :color red)
    "
      Text zoom: _j_:zoom in, _k_:zoom out, _0_:reset
"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("0" (text-scale-set 0) "reset"))

  (defhydra hydra-buffer-menu (:color pink
			       :hint nil)
    "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------                        (__)
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch                         (oo)
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch                      /------\\/
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur                 / |    ||
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only^^    *  /\\---/\\
_~_: modified      ^ ^                ^ ^                ^^                                 ~~   ~~
"
    ("m" Buffer-menu-mark)
    ("u" Buffer-menu-unmark)
    ("U" Buffer-menu-backup-unmark)
    ("d" Buffer-menu-delete)
    ("D" Buffer-menu-delete-backwards)
    ("s" Buffer-menu-save)
    ("~" Buffer-menu-not-modified)
    ("x" Buffer-menu-execute)
    ("b" Buffer-menu-bury)
    ("g" revert-buffer)
    ("T" Buffer-menu-toggle-files-only)
    ("O" Buffer-menu-multi-occur :color blue)
    ("I" Buffer-menu-isearch-buffers :color blue)
    ("R" Buffer-menu-isearch-buffers-regexp :color blue)
    ("c" nil "cancel")
    ("v" Buffer-menu-select "select" :color blue)
    ("o" Buffer-menu-other-window "other-window" :color blue)
    ("q" quit-window "quit" :color blue))

  (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)
  )

(provide 'key-bindings)