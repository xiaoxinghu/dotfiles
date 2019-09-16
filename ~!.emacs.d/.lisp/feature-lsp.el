(use-package lsp-mode
  ;; :quelpa (lsp-mode :fetcher github :repo "emacs-lsp/lsp-mode")
  :init
  (setq
   lsp-session-file (concat x/etc-dir "lsp-session")
   lsp-auto-guess-root t
   lsp-keep-workspace-alive nil
   lsp-eldoc-render-all nil
   lsp-inhibit-message t
   lsp-message-project-root-warning t)
  (add-hook 'kill-emacs-hook (setq lsp-restart 'ignore))

  :config
  (require 'lsp-clients)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

  (defhydra hydra-lsp (:exit t :hint nil)
    "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
    ("d" lsp-find-declaration)
    ("D" lsp-ui-peek-find-definitions)
    ("R" lsp-ui-peek-find-references)
    ("i" lsp-ui-peek-find-implementation)
    ("t" lsp-find-type-definition)
    ("s" lsp-signature-help)
    ("o" lsp-describe-thing-at-point)
    ("r" lsp-rename)

    ("f" lsp-format-buffer)
    ("m" lsp-ui-imenu)
    ("x" lsp-execute-code-action)

    ("M-s" lsp-describe-session)
    ("M-r" lsp-restart-workspace)
    ("S" lsp-shutdown-workspace))

  (map!
    "l" '(hydra-lsp/body :which-key "LSP"))
  )

(use-package lsp-ui
  :after (lsp)
  :config
  (setq
   lsp-prefer-flymake nil
   lsp-ui-doc-max-height 8
   lsp-ui-doc-max-width 35
   lsp-ui-flycheck-live-reporting nil
   lsp-ui-sideline-enable nil
   lsp-ui-sideline-show-diagnostics nil
   lsp-ui-sideline-ignore-duplicate t)
  )

(use-package company-lsp
  ;; :quelpa (company-lsp :fetcher github :repo "tigersoldier/company-lsp")
  :after lsp-mode
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-async t))

(provide 'feature-lsp)
