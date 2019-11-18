(defvar +write-text-scale nil
  "What to scale the text up to in `+write-mode'. Uses `text-scale-set'.")

(defvar +write-line-spacing nil
  "What to set `line-spacing' in `+write-mode'.")

;;;###autoload
(defvar +write-mode-map (make-sparse-keymap)
  "TODO")

(setq visual-fill-column-width 80)

;;;###autoload
(define-minor-mode +write-mode
  "Turns Emacs into a more comfortable writing environment and word processor."
  :init-value nil
  :keymap +write-mode-map
  (setq-local visual-fill-column-center-text t)
  (when +write-text-scale
    (text-scale-set (if +write-mode 2 0)))
  (when +write-line-spacing
    (setq-local line-spacing +write-line-spacing)))

;;;###autoload
(defun +write|init-org-mode ()
  "Initializes `org-mode' specific settings for `+write-mode'."
  (when (eq major-mode 'org-mode)
    (+org-pretty-mode (if +write-mode +1 -1))))

;;;###autoload
(defun +write|init-line-numbers ()
  (display-line-numbers-mode (if +write-mode +1 -1)))

;;;###autoload
(defun +write|init-mixed-pitch ()
  (mixed-pitch-mode (if +write-mode +1 -1)))

;;;###autoload
(defun +write|init-visual-fill-column ()
  (visual-fill-column-mode (if +write-mode +1 -1)))

;;;###autoload
;; (add-hook! '+write-mode-hook
;;   #'(flyspell-mode
;;      visual-line-mode
;;      +write|init-mixed-pitch
;;      +write|init-visual-fill-column
;;      +write|init-line-numbers
;;      +write|init-org-mode))
;;;###autoload
(add-hook '+write-mode-hook #'flyspell-mode)
;;;###autoload
(add-hook '+write-mode-hook #'visual-line-mode)
;;;###autoload
(add-hook '+write-mode-hook #'+write|init-mixed-pitch)
;;;###autoload
(add-hook '+write-mode-hook #'+write|init-visual-fill-column)
;;;###autoload
(add-hook '+write-mode-hook #'+write|init-line-numbers)
;;;###autoload
(add-hook '+write-mode-hook #'+write|init-org-mode)

(use-package visual-fill-column)

(use-package mixed-pitch
  :config
  (setq mixed-pitch-fixed-pitch-faces
        (append mixed-pitch-fixed-pitch-faces
                '(org-todo-keyword-todo
                  org-todo-keyword-habt
                  org-todo-keyword-done
                  org-todo-keyword-wait
                  org-todo-keyword-kill
                  org-todo-keyword-outd
                  org-todo
                  org-indent
                  line-number
                  line-number-current-line
                  org-special-keyword
                  org-date
                  org-property-value
                  org-special-keyword
                  org-property-value
                  org-ref-cite-face
                  org-tag
                  font-lock-comment-face))))

(provide 'feature-write)
