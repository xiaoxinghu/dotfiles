(defvar doom-escape-hook nil
  "A hook run after C-g is pressed (or ESC in normal mode, for evil users). Both
trigger `doom/escape'.

If any hook returns non-nil, all hooks after it are ignored.")

(defun doom/escape ()
  "Run the `doom-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((cl-find-if #'funcall doom-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'doom/escape)

(use-package general
  :config
  (general-evil-setup)
  (general-create-definer map!
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (general-create-definer map|local
    :states '(normal visual insert emacs)
    :prefix "SPC m"
    :non-normal-prefix "C-SPC m")

  (general-create-definer map|file
    :states '(normal visual insert emacs)
    :prefix "SPC f"
    :non-normal-prefix "C-SPC f")

  (general-create-definer map|insert
    :states '(normal visual insert emacs)
    :prefix "SPC i"
    :non-normal-prefix "C-SPC i")

  (general-create-definer map|notes
    :states '(normal visual insert emacs)
    :prefix "SPC n"
    :non-normal-prefix "C-SPC n")

  (general-create-definer map|global
    :states '(normal visual insert emacs)
    :keymaps 'override)

  (map!
    ;; simple command
    "u"   '(universal-argument :which-key "Universal argument")
    "'"   '(iterm-focus :which-key "iterm")
    "?"   '(iterm-goto-filedir-or-home :which-key "iterm - goto dir")
    "TAB" '(x|switch-to-other-buffer :which-key "prev buffer")

    ;; Applications
    "m" '(:ignore t :which-key "Major")
    "a" '(:ignore t :which-key "Applications")
    "f" '(:ignore t :which-key "File")
    "i" '(:ignore t :which-key "Insert")
    "n" '(:ignore t :which-key "Notes")
    "ar" 'ranger
    "ad" 'dired
    "q" '(:ignore t :which-key "Quit")
    "qq" 'save-buffers-kill-terminal

    ;; Help
    "h" '(:ignore t :which-key "Help")
    "hf" 'describe-function
    "hv" 'describe-variable
    "hk" 'describe-key
    "hh" 'help-for-help
    )

  ;; conventions
  (map|global
   "M-a" 'mark-whole-buffer
   "M-s" 'save-buffer
   "M-S" 'save-some-buffers
   "M-q" 'save-buffers-kill-terminal
   "M-w" 'delete-frame
   "M-O" 'ranger
   "M-n" 'make-frame-command
   "M-v" 'yank
   "M-=" 'text-scale-increase
   "M--" 'text-scale-decrease
   "M-0" '(lambda () (interactive)(text-scale-set 0))
   "M-RET" 'toggle-frame-fullscreen)

  (global-set-key (kbd "M-`") 'x|switch-to-other-buffer)
  )

(use-package hydra
  ;; :bind (("s-b" . hydra-buffer/body))
  :general
  (map!
    "b" 'hydra-buffer/body
    "z" 'hydra-text-zoom/body)
  :config
  (hydra-add-font-lock)

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

;;;###autoload (autoload 'hydra-buffer/body (concat x/lisp-dir "core-keys.el") nil t)
(defhydra hydra-buffer ()
  "buffer"
  ("b" counsel-ibuffer "buffers" :exit t)
  ("x" kill-this-buffer "kill buffer" :exit t)
  ("m" buffer-menu "buffer-menu" :exit t)
  ("h" switch-to-prev-buffer "prev")
  ("l" switch-to-next-buffer "next"))

;;;###autoload (autoload hydra-text-zoom/body (concat x/lisp-dir "core-keys.el") nil t)
(defhydra hydra-text-zoom (:hint t :color red)
  "
      Text zoom: _j_:zoom in, _k_:zoom out, _0_:reset
"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("0" (text-scale-set 0) "reset"))

(provide 'core-keys)
