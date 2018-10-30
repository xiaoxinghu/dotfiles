(defconst EMACS26+ (> emacs-major-version 25))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defvar x/local-dir (expand-file-name ".local/" user-emacs-directory)
  "Root directory for local Emacs files. Use this as permanent storage for files
that are safe to share across systems (if this config is symlinked across
several computers).")

(defvar x/cache-dir (concat x/local-dir "cache/"))

(make-directory x/local-dir :parents)

(setq custom-file (concat x/local-dir "custom.el"))

(use-package crux
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config (when (memq window-system '(mac ns))
	    (exec-path-from-shell-initialize)))

;;;###autoload
(defun doom-project-name (&optional dir)
  "Return the name of the current project."
  (let ((project-root (or (projectile-project-root dir)
                          (if dir (expand-file-name dir)))))
    (if project-root
        (funcall projectile-project-name-function project-root)
      "-")))

;; recentf
(use-package recentf
  :config
  (setq recentf-save-file (concat x/local-dir "recentf")
	recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 300
	recentf-exclude
        (list #'file-remote-p "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
              "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$"
              ;; ignore private DOOM temp files (but not all of them)
              (lambda (file) (file-in-directory-p file x/local-dir))))
  (recentf-mode +1))

(provide 'core)
