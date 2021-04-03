
(defvar edit-setter)

(defun edit-complete ()
  (interactive)
  (goto-char (point-min))
  (funcall edit-setter (read (current-buffer)))
  (kill-buffer))

(defun edit (sexp setter)
  "Edits sexp in a buffer, until C-c C-c is typed. Then the setter function is called."
  (let ((name (format "*edit* %S" sexp)))
    (switch-to-buffer (get-buffer-create name))
    (erase-buffer)
    (paredit-mode)
    (local-set-key (kbd "C-c C-c") 'edit-complete)
    (local-set-key (kbd "C-c c")   'edit-complete)
    (set (make-local-variable 'edit-setter) setter)
    (insert (pp sexp))))

;; (defvar *x* nil)
;; (edit '("a" "b" "c") (lambda (new-value) (setf *x* new-value)))
;; (+ 2 3)5
;; *x* ; --> ("aa" "bbb" "cccc")
