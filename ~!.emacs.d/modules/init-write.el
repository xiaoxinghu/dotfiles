(use-package writeroom-mode
  :commands (writeroom-mode)
  :config
  (add-to-list 'writeroom-global-effects 'visual-line-mode)
  (setq writeroom-restore-window-config t
    writeroom-width 100))

;;;###autoload
(defun writing-mode()
  "Enter writing mode."
  (interactive)
  (writeroom-mode 1)
  (blink-cursor-mode 1)
  )

(provide 'init-write)
