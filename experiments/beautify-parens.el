;;; Unfinished.

(defmacro handler-case (expression &rest clauses)
  "Common-Lisp
IMPLEMENTATION: The clause variable symbols are substituted by one single
                condition-case variable symbol.  This may cause problems
                if the same symbol is used as data or if it's a dynamic
                variable.
"
  (let* ((var (gensym))
         (neclause (assoc :NO-ERROR clauses))
         (nell     (cadr neclause))
         (nebody   (cddr neclause))
         (handlers (mapcar (lambda (clause)
                             (let ((typespec (car clause))
                                   (clausvar (cadr clause))
                                   (body     (cddr clause)))
                               (cons (if (and (consp typespec)
                                              (eq 'or (car typespec)))
                                         (cdr typespec)
                                         typespec)
                                     (if (null clausvar)
                                         body
                                         (subst  var (car clausvar) body)))))
                           (remove neclause clauses))))
    (if neclause
        `(condition-case ,var
             (multiple-value-bind ,nell ,expression ,@nebody)
           ,@handlers)
        `(condition-case ,var
             ,expression
           ,@handlers))))


(defun beautify-parens-in-sexp ()
  (let ((end-previous-sexp (point)))
    (handler-case (forward-sexp)
      (error () (return-from beautify-parens-in-sexp))   )
    (backward-sexp)
    (let ((start-current-sexp (point)))
      (when (= end-previous-sexp start-current-sexp)
        (insert " "))
      (when (looking-at "\\('\\|#\\([0-9]*[aA]\\)?\\)?(")
         (down-list)
         (when (looking-at " +")
           (delete-region (match-beginning 0) (match-beginning 1)))
         (while (not (looking-at ")"))
           (beautify-parens-in-sexp)
           ;; previous sexp may be a list or an atom.
           (cond
             ((looking-at "[ \t]+("))
             ((looking-at "[\n]"))
             ((looking-at "[ \t]+[\n)]")
              (delete-region (match-begining 0) (1- (match-end 0))))
             (t
              (insert " "))))
         (up-list))
      (goto-char start-current-sexp)
      (forward-sexp))))

;; (let ((r '())) (forward-sexp)(push (point) r)(forward-sexp)(backward-sexp)(push (point) r) r)
;;
;; ((abc))
;; ( (abc) )
;; (abc)
;; ( abc )

(defun beautify-parens ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-sexp)
    (while (< (point) (point-max))
      (backward-sexp)
      (if (looking-at "(")
          (progn
            (down-list)
            )
          (forward-sexp))
      (forward-sexp))))
