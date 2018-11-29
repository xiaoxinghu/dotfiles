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

(defvar x/cache-dir (concat x/local-dir "cache/"))

(make-directory x/local-dir :parents)

(setq custom-file (concat x/local-dir "custom.el"))

(provide 'core)