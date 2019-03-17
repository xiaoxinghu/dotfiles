;;; core.el --- core config                          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xiaoxing Hu

;; Author: Xiaoxing Hu <dawnstar.hu@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(provide 'core)
;;; core.el ends here

(setq user-full-name "Xiaoxing Hu"
  user-mail-address "dawnstar.hu@gmail.com")

(defconst EMACS26+ (> emacs-major-version 25))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

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

(make-directory x/local-dir :parents)

(setq custom-file (concat x/local-dir "custom.el"))

;; UTF-8 as the default coding system
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
                 "~/.authinfo.gpg")
  ;; files
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

(provide 'core)
