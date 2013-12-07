(defmacro* with-marker ((var position) &body body)
  (let ((vposition (gensym))) ; so (eq var position) still works.
    `(let* ((,vposition ,position)
            (,var (make-marker)))
       (set-marker ,var ,vposition)
       (unwind-protect (progn ,@body)
         (set-marker ,var nil)))))



(defun cl-string-justify (begin end style)
  "Set the region's justification style to STYLE, with ~ at the end of the lines.

Tilde immediately followed by a newline ignores the newline and any
following non-newline whitespace  characters. With a :, the newline is
ignored, but any following whitespace is left in place.  With an @,
the newline is left in place, but any following whitespace is
ignored. 

This commands prompts for the kind of justification to use.
If the mark is not active, this command operates on the current paragraph.
If the mark is active, it operates on the region.  However, if the
beginning and end of the region are not at paragraph breaks, they are
moved to the beginning and end (respectively) of the paragraphs they
are in.

If variable `use-hard-newlines' is true, all hard newlines are
taken to be paragraph breaks.
"
  ;;        ignore newline    ignore spaces    note
  ;;  ~\n         T                 T
  ;;  ~:\n        T                NIL         spaces may be unpreserved.
  ;;  ~@\n       NIL                T
  (interactive (list (if mark-active (region-beginning) (point))
                     (if mark-active (region-end)       (point))
                     (let ((s (completing-read
                               "Set justification to: "
                               '(("left") ("right") ("full")
                                 ("center") ("none"))
                               nil t)))
                       (if (equal s "") (error ""))
                       (intern s))))
  (with-marker (end end)
    (goto-char begin)
    (flet ((justify-section (begin end)
             ;; remove ~ and ~: 
             (goto-char begin)
             (while (re-search-forward "\\(~\n *\\|~:\n\\)" end t)
               (delete-region (match-beginning 0) (match-end 0)))
             ;; justify the section.
             (set-justification begin end style nil)
             ;; add ~
             (goto-char begin)
             (while (re-search-forward "\n" end t)
               (goto-char (match-beginning 0))
               (insert " ~")
               (forward-char 1))))
      (while (re-search-forward "~@\n" end t)
        (with-marker (section-end (match-beginning 0))
          (with-marker (next-section-start (match-end 0))
            (justify-section begin section-end)
            (goto-char (setf begin (marker-position next-section-start))))))
      (justify-section begin end))))


(defun justify-left-cl-string (point)
  (interactive "d")
  "Left-justify the CL string that is just at point or which has point inside it."
  (save-excursion
    (unless (looking-at "\"")
      (paredit-backward-up))
    (unless (looking-at "\"")
      (error "Not in a string"))
    (let ((start (1+ (point))))
      (goto-char start)
      (beginning-of-line)
      (let ((indent (- start (point))))
        (goto-char start)
        (insert (make-string indent 32))
        (goto-char (1- start))
        (forward-sexp 1)
        (backward-char 1)
        (let ((end (point)))
          (narrow-to-region start end)
          (goto-char start)
          (unwind-protect
               (cl-string-justify start end 'left)
            (widen)
            (delete-region start (+ start indent))))))))

