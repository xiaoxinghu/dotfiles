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
