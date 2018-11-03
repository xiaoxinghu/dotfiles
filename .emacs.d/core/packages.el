(require 'package)

(defvar x/core-packages '(use-package quelpa)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(setq package-archives
  '(("gnu"   . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org"   . "https://orgmode.org/elpa/"))
  quelpa-checkout-melpa-p nil
  quelpa-update-melpa-p nil
  quelpa-melpa-recipe-stores nil
  package-user-dir (expand-file-name "elpa" x/packages-dir)
  quelpa-dir (expand-file-name "quelpa" x/packages-dir)
  )
(package-initialize)

;; bootstrap core packages
(defun x/ensure-core-packages ()
  "Make sure `x/core-packages' are installed."
  (let ((core-packages (cl-remove-if #'package-installed-p x/core-packages)))
    (message "Installing core packages")
    (package-refresh-contents)
    (dolist (package core-packages)
      (package-install package)
      (if (package-installed-p package)
        (message "✓ Installed %s" package)
        (error "✕ Couldn't install %s" package)))
    (message "Installing core packages...done"))
  )

(x/ensure-core-packages)

(quelpa
  '(quelpa-use-package
     :fetcher git
     :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))

(require 'quelpa-use-package)

;; (require 'use-package)

(provide 'packages)
