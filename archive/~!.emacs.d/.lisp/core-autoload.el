(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

;;;###autoload
(defun x|switch-to-other-buffer ()
  "to the other buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

;;;###autoload
(defun x|yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun x|recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch
Emacs versions."
  (interactive)
  (byte-recompile-directory package-user-dir nil t))

;;;###autoload
(defun +macos-open-with (&optional app-name path)
  "Send PATH to APP-NAME on OSX."
  (interactive)
  (let* ((path (expand-file-name
                (replace-regexp-in-string
                 "'" "\\'"
                 (or path (if (eq major-mode 'dired-mode)
                              (dired-get-file-for-visit)
                            (buffer-file-name)))
                 nil t)))
         (command (format "open %s"
                          (if app-name
                              (format "-a %s '%s'" (shell-quote-argument app-name) path)
                            (format "'%s'" path)))))
    (message "Running: %s" command)
    (shell-command command)))

;;;###autoload
(defmacro +macos!open-with (id &optional app dir)
  `(defun ,(intern (format "+macos/%s" id)) ()
     (interactive)
     (+macos-open-with ,app ,dir)))

;;;###autoload (autoload '+macos/open-in-default-program "tools/macos/autoload" nil t)
(+macos!open-with open-in-default-program)

;;;###autoload (autoload '+macos/reveal-in-finder "tools/macos/autoload" nil t)
(+macos!open-with reveal-in-finder "Finder" default-directory)

;;;###autoload (autoload '+macos/reveal-project-in-finder "tools/macos/autoload" nil t)
(+macos!open-with reveal-project-in-finder "Finder"
                  (or (doom-project-root) default-directory))

;;;###autoload (autoload '+macos/send-to-transmit "tools/macos/autoload" nil t)
(+macos!open-with send-to-transmit "Transmit")

;;;###autoload (autoload '+macos/send-cwd-to-transmit "tools/macos/autoload" nil t)
(+macos!open-with send-cwd-to-transmit "Transmit" default-directory)

;;;###autoload (autoload '+macos/send-to-launchbar "tools/macos/autoload" nil t)
(+macos!open-with send-to-launchbar "LaunchBar")

;;;###autoload (autoload '+macos/send-project-to-launchbar "tools/macos/autoload" nil t)
(+macos!open-with send-project-to-launchbar "LaunchBar"
                  (or (doom-project-root) default-directory))

(map|file
  "F" '(+macos/reveal-in-finder :which-key "Open In Finder"))

(setq mac-command-modifier 'meta)

(provide 'core-autoload)
