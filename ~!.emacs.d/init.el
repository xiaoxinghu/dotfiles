(load (concat user-emacs-directory ".lisp/bootstrap")
      nil 'nomessage)

(enable!
 'feature-ivy
 'feature-posframe
 'feature-electric
 'feature-org
 'feature-company
 'feature-spell
 'feature-translate
 'feature-ledger
 ;; 'feature-write
 'feature-git
 'feature-project
 'feature-snippets
 'feature-lsp
 'feature-lisp
 'feature-web
 'feature-markdown
 'feature-shell
 'feature-rust
 'feature-yaml
 'feature-lua
 'feature-swift
 'feature-elm
 'feature-docker)

(if x/interactive-mode
    (progn
      (x/initialize-core)
      (x/enable-features)))
