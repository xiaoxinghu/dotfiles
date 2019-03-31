(use-package writeroom-mode
  :commands (writeroom-mode)
  :config
  (setq
    writeroom-major-modes '(text-mode org-mode)
    writeroom-global-effects '(visual-line-mode)
    writeroom-extra-line-spacing 0.3
    writeroom-restore-window-config t
    writeroom-width 100))

;;;###autoload
(defun writing-mode()
  "Enter writing mode."
  (interactive)
  (writeroom-mode 1)
  (blink-cursor-mode 1)
  )

(provide 'init-write)
