(require 'mm-url)

(defun x-org/insert-template (&optional title uri
                                             keywords tags description)
  "Insert a template into current buffer with information for exporting.
TITLE: the title of this post
URI: the uri of this post, usually looks like: /2013/12/27/the-post-title,
the following parameters could be used:
    %y: to represent the year of creation date
    %m: to represent the month of creation date
    %d: to represent the day of creation date
KEYWORDS: the keywords of this post, used by search engine
TAGS: the tags of this post, should be separated by comma and space
DESCRIPTION: the description of this post, it will be displayed in RSS feed
Note that this function does not verify the input parameters, it is users'
responsibility to guarantee these parameters are valid."
  (interactive
   (let* ((i (read-string "Title: "))
          (u (read-string "URI(%y, %m and %d can be used to represent year, \
month and day): " (unless (string= i "")
                    (format-spec "/blog/%y/%m/%d/%t"
                                 `((?y . "%y")
                                   (?m . "%m")
                                   (?d . "%d")
                                   (?t . ,(encode-string-to-url i)))))))
          (k (read-string "Keywords(separated by comma and space [, ]): "))
          (a (read-string "Tags(separated by comma and space [, ]): "))
          (d (read-string "Description: ")))
     (list i u k a d)))
  (if (not (bolp)) (newline))
  (insert (format
           "#+TITLE:       %s
#+AUTHOR:      %s
#+EMAIL:       %s
#+DATE:        %s
#+URI:         %s
#+KEYWORDS:    %s
#+TAGS:        %s
#+LANGUAGE:    %s
#+OPTIONS:     H:%d num:%s toc:%s \\n:%s ::%s |:%s ^:%s -:%s f:%s *:%s <:%s
#+DESCRIPTION: %s
"
           (if (string= title "") (buffer-name) title)
           (user-full-name)
           user-mail-address
           (format-time-string (substring (car org-time-stamp-formats) 1 -1))
           (if (string= uri "") "<TODO: insert your uri here>" uri)
           (if (string= keywords "")
               "<TODO: insert your keywords here>"
             keywords)
           (if (string= tags "") "<TODO: insert your tags here>" tags)
           org-export-default-language
           org-export-headline-levels
           nil ;; org-export-with-section-numbers
           nil ;; org-export-with-toc
           org-export-preserve-breaks
           ;; org-export-html-expand
           org-export-with-fixed-width
           org-export-with-tables
           nil ;; org-export-with-sub-superscripts
           nil ;; org-export-with-special-strings
           org-export-with-footnotes
           org-export-with-emphasize
           org-export-with-timestamps
           (if (string= description "")
               "<TODO: insert your description here>"
             description))))

(defun x-org/quick-template()
  (interactive)
  (x-org/insert-template "" "" "" ""))

(defun op/new-post (&optional category filename)
  "Setup a new post.
CATEGORY: this post belongs to
FILENAME: the file name of this post
Note that this function does not verify the category and filename, it is users'
responsibility to guarantee the two parameters are valid."
  (interactive
   (let* ((c (read-string "Category: " "blog"))
          (f (read-string "filename: " "new-post.org")))
     (list c f)))
  (if (string= category "")
      (setq category "blog"))
  (if (string= filename "")
      (setq filename "new-post.org"))
  (unless (string-suffix-p ".org" filename)
    (setq filename (concat filename ".org")))
  (let* ((dir (concat (file-name-as-directory op/repository-directory)
                      (file-name-as-directory category)))
         (path (concat dir filename)))
    (if (file-exists-p path)
        (error "Post `%s' already exists." path))
    (unless (file-directory-p dir)
      (mkdir dir t))
    (switch-to-buffer (find-file path))
    (if (called-interactively-p 'any)
        (call-interactively 'x-org/insert-template)
      (x-org/insert-template "<Insert Your Title Here>"
                                  "/%y/%m/%d/%t/"
                                  "add, keywords, here"
                                  "add, tags, here"
                                  "add description here"))
    (save-buffer)))

(defun x-org/insert-link ()
  "Insert org link where default description is set to html title."
  (interactive)
  (let* ((url (read-string "URL: "))
         (title (get-html-title-from-url url)))
    (org-insert-link nil url title)))

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

(defun org-preamble (options)
  "The function that creates the preamble top section for the blog.
OPTIONS contains the property list from the org-mode export."
  (let ((base-directory (plist-get options :base-directory)))
    (org-babel-with-temp-filebuffer (expand-file-name ".preamble.html" base-directory) (buffer-string))))

(defun org-postamble (options)
  "The function that creates the postamble, or bottom section for the blog.
OPTIONS contains the property list from the org-mode export."
  (let ((base-directory (plist-get options :base-directory)))
    (org-babel-with-temp-filebuffer (expand-file-name ".postamble.html" base-directory) (buffer-string))))

(defun org-publish-prepare ()
  "`index.org' should always be exported so touch the file before publishing."
  (let* ((base-directory (plist-get project-plist :base-directory))
         (buffer (find-file-noselect (expand-file-name "index.org" base-directory) t)))
    (with-current-buffer buffer
      (set-buffer-modified-p t)
      (save-buffer 0))
    (kill-buffer buffer)))

(defun org-shotgun-filename(dir filename extension)
  (concat (file-name-as-directory dir)
          (file-name-nondirectory filename)
          extension))

(defun org-shotgun-to-files
    (backend output-dir &optional async visible-only body-only ext-plist
             post-process)
  "Export all subtrees that are *not* tagged with :noexport: to
separate files.

Subtrees that do not have the :EXPORT_FILE_NAME: property set
are exported to a filename derived from the headline text."
  (interactive)
  (save-excursion
    (message "output-dir-> %s" output-dir)
    (set-mark (point-min))
    (goto-char (point-max))
    (org-map-entries
     (lambda ()
       (let ((export-file
              (or (org-entry-get (point) "EXPORT_FILE_NAME")
                  (downcase
                   (replace-regexp-in-string " " "-" (nth 4 (org-heading-components)))))))
         (org-narrow-to-subtree)
         (org-export-to-file backend (org-shotgun-filename output-dir export-file ".html"))
           async subtreep visible-only body-only ext-plist post-process)
         (widen)))
     "/+PUBLISHED" 'region-start-level))


(defun org-publish-with-shotgun (backend extension plist &optional pub-dir)
  "Shotgun an Org file to a specified back-end.

BACKEND is a symbol representing the back-end used for
transcoding.  FILENAME is the filename of the Org file to be
published.  EXTENSION is the extension used for the output
string, with the leading dot.  PLIST is the property list for the
given project.

Optional argument PUB-DIR, when non-nil is the publishing
directory.

Return output file name."
  (unless (or (not pub-dir) (file-exists-p pub-dir)) (make-directory pub-dir t))
  (message "pub-dir-> %s" pub-dir)
  (message "pub-dir: %s" (plist-get project-plist :publishing-directory))
  ;; Check if a buffer visiting FILENAME is already open.
  (let* ((org-inhibit-startup t)
         (pub-dir (or pub-dir (plist-get project-plist :publishing-directory)))
	 (visitingp (find-buffer-visiting filename))
	 (work-buffer (or visitingp (find-file-noselect filename))))
    (prog1 (with-current-buffer work-buffer
       (let ((output-file
		    (org-export-output-file-name extension nil pub-dir))
		   (body-p (plist-get plist :body-only)))
	       (org-shotgun-to-files backend (plist-get project-plist :publishing-directory)
		 nil nil body-p
		 ;; Add `org-publish--collect-references' and
		 ;; `org-publish-collect-index' to final output
		 ;; filters.  The latter isn't dependent on
		 ;; `:makeindex', since we want to keep it up-to-date
		 ;; in cache anyway.
		 (org-combine-plists
		  plist
		  `(:filter-final-output
		    ,(cons 'org-publish--collect-references
			   (cons 'org-publish-collect-index
				 (plist-get plist :filter-final-output))))))))
      ;; Remove opened buffer in the process.
      (unless visitingp (kill-buffer work-buffer)))))

(defun org-publish-with-shotgun-html (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-with-shotgun 'html filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension
                                      "html"))
                      plist pub-dir))
