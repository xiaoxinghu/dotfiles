(use-package avy
  :commands (avy-goto-word-1)
  :general
  (map!
    "SPC" '(avy-goto-word-or-subword-1  :which-key "go to char")))

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
  (map!
    "f" '(:ignore t :which-key "Files")
    "f f" '(counsel-find-file :which-key "find file")
    "f y" '(x|yank-buffer-filename :which-key "yank filename")
    "f r" '(counsel-recentf :which-key "recent file")
    "f R" '(crux-rename-buffer-and-file :which-key "rename file")
    "f d" '(dired :which-key "dired")
    "f D" '(crux-delete-file-and-buffer :which-key "delete file")
    ;; "b" '(:ignore t :which-key "Buffers")
    ;; "b b" '(counsel-ibuffer :which-key "ibuffer")
    ":" 'counsel-M-x)
  :bind
  ("M-x" . counsel-M-x)
  ("C-c k" . counsel-ag))

(use-package swiper
  :bind
  ("M-f" . swiper))

(use-package dired
  :ensure nil
  :config
  (defhydra hydra-dired (:hint nil :color pink)
    "
     _+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
     _C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
     _D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
     _R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
     _Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
     _S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
     _r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
     _z_ compress-file  _A_ find regexp
     _Z_ compress       _Q_ repl regexp

     T - tag prefix
     "
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff)         ;; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy)        ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer)        ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay)   ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))

  (define-key dired-mode-map "." 'hydra-dired/body))

(use-package ranger
  :after dired
  :init
  :config
  (setq ranger-override-dired t
        ranger-cleanup-on-disable t
        ranger-omit-regexp "^\.DS_Store$"
        ranger-excluded-extensions '("mkv" "iso" "mp4")
        ranger-deer-show-details nil
        ranger-max-preview-size 10
        dired-omit-verbose nil))

(provide 'core-navigation)