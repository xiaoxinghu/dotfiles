(use-package lsp
  :ensure lsp-mode
  :config
  (require 'lsp-clients)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  :init
  (setf lsp-eldoc-render-all nil)
  (setq lsp-inhibit-message t)
  (setq lsp-message-project-root-warning t)
  (setf lsp-prefer-flymake nil)
  (setq lsp-message-project-root-warning t)
  (setq lsp-clients-typescript-server "typescript-language-server"
	lsp-clients-typescript-server-args '("--stdio"))
  )

(use-package lsp-ui
  :after (lsp))

(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package js2-mode
  :mode "\\.\\(js\\|snap\\)\\'"
  :interpreter "node"
  :init
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  :config
  (setq js2-skip-preprocessor-directives t
    js-chain-indent t
    ;; let flycheck handle this
    js2-mode-show-parse-errors nil
    js2-mode-show-strict-warnings nil
    ;; Flycheck provides these features, so disable them: conflicting with
    ;; the eslint settings.
    js2-strict-trailing-comma-warning nil
    js2-strict-missing-semi-warning nil
    ;; maximum fontification
    js2-highlight-level 3
    js2-highlight-external-variables t)
  (add-hook 'js2-mode-hook #'rainbow-delimiters-mode)
  (with-eval-after-load 'lsp-clients
    (add-hook 'js2-mode-hook #'lsp)))

(use-package rjsx-mode
  :mode "components/.+\\.js$"
  :init
  (defun +javascript-jsx-file-p ()
    "Detect React or preact imports early in the file."
    (and buffer-file-name
      (string= (file-name-extension buffer-file-name) "js")
      (re-search-forward "\\(^\\s-*import +React\\|\\( from \\|require(\\)[\"']p?react\\)"
        magic-mode-regexp-match-limit t)
      (progn (goto-char (match-beginning 1))
        (not (sp-point-in-string-or-comment)))))
  (add-to-list 'magic-mode-alist '(+javascript-jsx-file-p . rjsx-mode))
  :config
  ;; (set-electric! 'rjsx-mode :chars '(?\} ?\) ?. ?>))
  ;; (when (featurep! :feature syntax-checker)
  ;;   (add-hook! 'rjsx-mode-hook
  ;;     ;; jshint doesn't know how to deal with jsx
  ;;     (push 'javascript-jshint flycheck-disabled-checkers)))

  ;; ;; `rjsx-electric-gt' relies on js2's parser to tell it when the cursor is in
  ;; ;; a self-closing tag, so that it can insert a matching ending tag at point.
  ;; ;; However, the parser doesn't run immediately, so a fast typist can outrun
  ;; ;; it, causing tags to stay unclosed, so we force it to parse.
  ;; (defun +javascript|reparse (n)
  ;;   ;; if n != 1, rjsx-electric-gt calls rjsx-maybe-reparse itself
  ;;   (if (= n 1) (rjsx-maybe-reparse)))
  ;; (advice-add #'rjsx-electric-gt :before #'+javascript|reparse)
  )

(use-package add-node-modules-path
  :config
  (progn
    (eval-after-load 'js2-mode
      '(add-hook 'js2-mode-hook #'add-node-modules-path))
    )
  )

(provide 'init-javascript)
