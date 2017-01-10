(require 'mm-url)

;; funcs
(defun get-html-title-from-url (url)
  "Return content in <title> tag."
  (let (x1 x2 (download-buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer download-buffer)
      (beginning-of-buffer)
      (setq x1 (search-forward "<title>"))
      (search-forward "</title>")
      (setq x2 (search-backward "<"))
      (mm-url-decode-entities-string (buffer-substring-no-properties x1 x2)))))

(defun x-org/insert-link ()
  "Insert org link where default description is set to html title."
  (interactive)
  (let* ((url (read-string "URL: "))
         (title (get-html-title-from-url url)))
    (org-insert-link nil url title)))

;; config
(setq org-directory "~/io")
(with-eval-after-load 'org
  (setq org-agenda-files (list org-directory (concat org-directory "/notes")))
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  (setq org-log-into-drawer 1)
  ;; Capture Templates
  (setq org-capture-templates
        `(("t" "todo" entry
           (file (concat org-directory "/inbox.org"))
           (file , "~/.spacemacs.d/templates/todo.txt")
           ::empty-lines-before 1
           ::empty-lines-after 1)
          ("n" "note" entry
           (file (concat org-directory "/inbox.org"))
           (file , "~/.spacemacs.d/templates/note.txt")
           ::empty-lines-before 1
           ::empty-lines-after 1)
          ("l" "link" entry
           (file (concat org-directory "/inbox.org"))
           (file , "~/.spacemacs.d/templates/link.txt")
           ::empty-lines-before 1
           ::empty-lines-after 1)
          ("j" "journal" plain
           (file+datetree (concat org-directory "/journal.org"))
           (file , "~/.spacemacs.d/templates/journal.txt")
           ::empty-lines-before 1
           ::empty-lines-after 1)
          ))

  ;; Capture Window popup
  (defadvice org-capture
      (after make-full-window-frame activate)
    "Advise capture to be the only window when used as a popup"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-other-windows)))

  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-frame)))

  ;; TODO keywords
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "deep sky blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold))))

  ;; Agenda
  (setq org-agenda-custom-commands
        (quote ((" " "Home"
                 ((agenda "" nil)
                  (todo "NEXT"
                        ((org-agenda-overriding-header "NEXT")))
                  (tags "REFILE"
                        ((org-agenda-overriding-header "TO REFILE")))
                  (todo "DRAFT"
                        ((org-agenda-overriding-header "WRITING")
                         (org-agenda-sorting-strategy '(todo-state-up))
                         ))
                  (tags-todo "PROJECT+TODO=\"TODO\""
                             ((org-agenda-overriding-header "PROJECTS")
                              (org-agenda-sorting-strategy '(todo-state-up))
                              ))
                  (tags-todo "NOTE+TODO=\"TODO\""
                             ((org-agenda-overriding-header "NOTES")
                              (org-agenda-sorting-strategy '(todo-state-up))
                              ))
                  (todo "WAITING|HOLD"
                        ((org-agenda-overriding-header "PENDING")
                         (org-agenda-sorting-strategy '(todo-state-up))
                         ))
                  )))))

  ;; Archiving
  (setq org-archive-mark-done nil)
  (setq org-archive-location "%s_archive::* Archived Tasks")

  ;; Babel
  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (sh . t)
     (ruby . t)
     (sass . t)
     ))
  ;; this is for being able to edit yaml front matter with yaml-mode
  ;; and also generate raw yaml front matter in the exported file
  ;; reference: http://swaac.tamouse.org/emacs/org-mode/2015/05/25/using-emacss-org-mode-and-editing-yaml-frontmatter-in-jekyll-posts/
  (defun org-babel-execute:yaml (body params) body)
  )

;; key bindings
(global-set-key (kbd "s-k") 'x-org/insert-link)

;; journal
(setq org-journal-dir (concat org-directory "/journal/")
      org-journal-date-format "%A, %B %d %Y"
      org-journal-time-prefix "* "
      org-journal-file-format "%Y-%m-%d")
