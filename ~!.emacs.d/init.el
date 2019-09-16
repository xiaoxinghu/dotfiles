;;; .init.el --- My emacs setup                      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xiaoxing Hu

;; Author: Xiaoxing Hu <xiaoxing@huxx.org>
;; Keywords: docs

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

(load (concat user-emacs-directory ".lisp/bootstrap")
      nil 'nomessage)

(defun x/initialize-features ()
  "Load features for an interactive session."
  (require 'feature-spell)
  (require 'feature-translate)
  (require 'feature-ledger)
  ;; (require 'feature-write)
  (require 'feature-git)
  (require 'feature-project)
  (require 'feature-snippets)
  (require 'feature-lsp)
  (require 'feature-lisp)
  (require 'feature-web)
  (require 'feature-markdown)
  (require 'feature-shell)
  (require 'feature-rust)
  (require 'feature-yaml)
  (require 'feature-lua)
  (require 'feature-swift)
  (require 'feature-elm)
  (require 'feature-docker))

(if x/interactive-mode
    (progn
      (x/initialize-core)
      (x/initialize-features)))
