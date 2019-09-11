(defvar doom-gc-cons-threshold 16777216 ; 16mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this. If you experience stuttering, increase this.")

(defvar doom-gc-cons-upper-limit 268435456 ; 256mb
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar doom--file-name-handler-alist file-name-handler-alist)

(defun doom|restore-startup-optimizations ()
  "Reset garbage collection settings to reasonable defaults.
A large `gc-cons-threshold' can cause random freezes otherwise and
resets `file-name-handler-alist'."
  (setq file-name-handler-alist doom--file-name-handler-alist)
  ;; Do this on idle timer to defer a possible GC pause that could result; also
  ;; allows deferred packages to take advantage of these optimizations.
  (run-with-idle-timer
   3 nil (lambda () (setq-default gc-cons-threshold doom-gc-cons-threshold))))


(if (or after-init-time noninteractive)
    (setq gc-cons-threshold doom-gc-cons-threshold)
  ;; A big contributor to startup times is garbage collection. We up the gc
  ;; threshold to temporarily prevent it from running, then reset it later in
  ;; `doom|restore-startup-optimizations'.
  (setq gc-cons-threshold doom-gc-cons-upper-limit)
  ;; This is consulted on every `require', `load' and various path/io functions.
  ;; You get a minor speed up by nooping this.
  (setq file-name-handler-alist nil)
  ;; Not restoring these to their defaults will cause stuttering/freezes.
  (add-hook 'emacs-startup-hook #'doom|restore-startup-optimizations))

(setq user-full-name "Xiaoxing Hu"
      user-mail-address "xiaoxing@huxx.org")

(defconst EMACS26+ (> emacs-major-version 25))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defvar x/emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defvar x/lisp-dir (concat x/emacs-dir ".lisp/")
  "The root directory of Doom's core files. Must end with a slash.")

(defvar x/local-dir (expand-file-name ".local/" user-emacs-directory)
  "Root directory for local Emacs files.
     Use this as permanent storage for files
     that are safe to share across systems (if this config is symlinked across
     several computers).")

(defvar x/packages-dir (concat x/local-dir "packages/")
  "Where package.el and quelpa plugins (and their caches) are stored.")

(defvar x/cache-dir (concat x/local-dir "cache/")
  "Directory for volatile storage.

     Use this for files that change often, like cache files.")

(defvar x/etc-dir (concat x/local-dir "etc/")
  "Directory for non-volatile storage.

     Use this for files that don't change much, like servers binaries, external
     dependencies or long-term shared data.")

(defvar x/var-dir (concat x/local-dir "var/")
  "Directory for volatile storage.")
(make-directory x/local-dir :parents)

(setq custom-file (concat x/local-dir "custom.el"))

(push x/lisp-dir load-path)

(setq-default
 abbrev-file-name             (concat x/local-dir "abbrev.el")
 auto-save-list-file-name     (concat x/cache-dir "autosave")
 backup-directory-alist       (list (cons "." (concat x/cache-dir "backup/")))
 pcache-directory             (concat x/cache-dir "pcache/")
 request-storage-directory    (concat x/cache-dir "request")
 server-auth-dir              (concat x/cache-dir "server/")
 shared-game-score-directory  (concat x/etc-dir "shared-game-score/")
 tramp-auto-save-directory    (concat x/cache-dir "tramp-auto-save/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name  (concat x/cache-dir "tramp-persistency.el")
 url-cache-directory          (concat x/cache-dir "url/")
 url-configuration-directory  (concat x/etc-dir "url/")
 gamegrid-user-score-file-directory (concat x/etc-dir "games/"))

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system        'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; perdy
(setq locale-coding-system   'utf-8)   ; please
(setq-default buffer-file-coding-system 'utf-8) ; with sugar on top

(setq-default
 ;; be quiet at startup; don't load or display anything unnecessary
 inhibit-startup-message t
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil
 find-file-visit-truename t       ; resolve symlinks when opening files
 ;; History & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil  ; don't create backup~ files
 ;; Don't store authinfo in plain text!
 auth-sources (list (expand-file-name "authinfo.gpg" x/etc-dir)
                    "~/.authinfo.gpg"))

(defun x/initialize-core ()
  "Load Doom's core files for an interactive session."
  (require 'core-packages)
  (require 'core-keys)
  (require 'core-ui))
