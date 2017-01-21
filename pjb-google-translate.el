(defun does-not-work/google-translate-word (from to word)
  (pjb-parse-html
   (pjb-get-resource-at-url
    (url-encode-url (format "https://translate.google.com/#%s/%s/%s"
                            from to word)))))

;; (does-not-work/google-translate-word "ru" "fr" "совсем")


(defparameter *grammar-categories* '("phrase"
                                     "adverb"
                                     "noun"
                                     "adjective"))

(defun google-translate-section-split (section)
  (split-string section "\n\t\n" t "\n"))


(defun format-google-translate-results (start end)
  (interactive "r")
  (let ((title     nil)
        (sections '()))
    (save-restriction
     (narrow-to-region start end)
     (let ((re        (format "^\\(%s\\)"
                              (mapconcat (function identity)
                                         *grammar-categories* "\\|"))))
       ;; Collect sections:
       (goto-char start)
       (when (re-search-forward re (point-max) t)
         (let ((start     (match-beginning 0)))
           (setf title (buffer-substring (point-min) start))
           (while (re-search-forward re (point-max) t)
             (let ((end  (match-beginning 0)))
               (push (google-translate-section-split
                      (buffer-substring start end))
                     sections)
               (setf start end)))
           (push (google-translate-section-split
                  (buffer-substring start (point-max)))
                 sections)))
       ;; Reformat sections:
       (goto-char (point-min))
       (delete-region (point-min) (point-max))
       (insert title)
       (loop with width = (loop for (category . entries) in (reverse sections)
                                maximize (loop for (word translations)
                                                 on entries by (function cddr)
                                               maximize (length word)))
             with format = (format "  %%-%ds  %%s\n" width)
             for (category . entries) in (reverse sections)
             do (insert category "\n")
                (loop for (word translations) on entries by (function cddr)
                      do (insert (format format word translations))))
       (indent-region (point-min) (point) 20)))))
