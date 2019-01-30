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

(provide 'autoloads)
