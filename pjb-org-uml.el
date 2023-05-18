(require 'org)
(require 'cl)

(defun pjb-org-insert-use-case (title id)
  (interactive
   (list (setf title (read-from-minibuffer "Use Case Title: "))
         (read-from-minibuffer "Use Case ID: "
                               (substitute ?- 32 (string-downcase title)))))
  (let ((file (format "u-%s.org" id)))
    (insert (format "\n#+INCLUDE: %S\n" file))
    (with-temp-buffer
        (insert-file-contents "u-template.org")

      (goto-char (point-min))
      (while (re-search-forward "{TITLE}" nil t)
        (delete-region (match-beginning 0) (match-end 0))
        (insert title))

      (goto-char (point-min))
      (while (re-search-forward "{ID}" nil t)
        (delete-region (match-beginning 0) (match-end 0))
        (insert "u-" id))

      (write-file file t))))
