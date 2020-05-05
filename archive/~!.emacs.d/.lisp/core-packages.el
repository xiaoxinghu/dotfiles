(require 'package)

(defvar x/core-packages '(use-package quelpa)
  "A list of packages that must be installed (and will be auto-installed if
missing) and shouldn't be deleted.")

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))

(setq
 quelpa-checkout-melpa-p nil
 ;; quelpa-stable-p t
 quelpa-update-melpa-p nil
 quelpa-melpa-recipe-stores nil
 ;; use-package-ensure-function 'quelpa
 use-package-always-ensure t
 package-user-dir (expand-file-name "elpa" x/packages-dir)
 quelpa-dir (expand-file-name "quelpa" x/packages-dir)
 )

(package-initialize)

(defun x/ensure-core-packages ()
  "Make sure `x/core-packages' are installed."
  (let ((core-packages (cl-remove-if #'package-installed-p x/core-packages)))
    (unless (= (length core-packages) 0)
      (message "Installing core packages")
      (package-refresh-contents)
      (dolist (package core-packages)
        (package-install package)
        (if (package-installed-p package)
            (message "✓ Installed %s" package)
          (error "✕ Couldn't install %s" package)))
      (message "Installing core packages...done"))))

(x/ensure-core-packages)

(quelpa
 '(quelpa-use-package
   :stable nil
   :fetcher git
   :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))

(require 'quelpa-use-package)

(provide 'core-packages)
