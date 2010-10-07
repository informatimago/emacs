;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               psql.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    psql is an interactive wrapper around pg.el
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-10-05 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2010
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(require 'pg)
(require 'cl)

;;; Some more Common Lisp functions (usually from pjb-cl, but I want
;;; this file to be stand alone).

(defmacro cl:handler-case (expression &rest clauses)
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


(defun cl:character (x)
  "Common-Lisp: Returns the character denoted by the character designator."
  (etypecase x
    (integer x)
    (string (aref x 0))
    (symbol (aref (symbol-name x) 0))))


(defun cl:char= (x y) (= x y))


(defun cl:string (x)
  "Common-Lisp: returns a string described by x.
Specifically:
  * If x is a string, it is returned.
  * If x is a symbol, its name is returned.
  * If x is a character, then a string containing that one character is returned.
"
  (etypecase x
    (integer (string x))
    (symbol (symbol-name x))
    (string  x)))


(defun cl:string-trim (character-bag string-designator)
  "Common-Lisp:  returns a substring of string, with all characters in character-bag stripped off the beginning and end.
"
  (unless (sequencep character-bag)
    (signal 'type-error  "Expected a sequence for `character-bag'."))
  (let* ((string (cl:string string-designator))
         (margin (format "[%s]*" (regexp-quote
                                  (if (stringp character-bag)
                                      character-bag
                                      (map 'string 'identity character-bag)))))
         (trimer (format "\\`%s\\(\\(.\\|\n\\)*?\\)%s\\'" margin margin)))
    (replace-regexp-in-string  trimer "\\1" string)))


(defun* cl:make-string (size &key (initial-element ??) (element-type 'character))
  "Common-Lisp: MAKE-STRING takes a key :initial-element argument
"
  (declare (ignore element-type))
  (make-string size initial-element))

(defun cl:princ-to-string (object)
  (let ((*print-circle* nil))
   (with-output-to-string (princ object))))



;;;
;;; psql buffer local variables:
;;;

(defvar psql:*db* nil
  "Buffer local variable holding the psql database.")

(defvar psql:*history* nil
  "Buffer local variable holding the command history.")

(defvar psql:*input-start* nil
  "Buffer local variable holding the position of start of the user input.")


(defun psql:format-table (ll)
  (let* ((titles   (car ll))
         (lines    (cdr ll))
         (widths   (mapcar (lambda (line) (mapcar (function length) line)) lines))
         col-widths top-cols)
    (macrolet ((h-bars 
                   () `(do ((col-widths col-widths (cdr col-widths)))
                           ((null col-widths))
                         (insert (cl:make-string (car col-widths)
                                               :initial-element (cl:character "-")))
                         (insert (if (null (cdr col-widths)) "\n" " ")))))
      ;; compute the column widths:
      (setq col-widths (copy-seq (car widths)))
      (dolist (lin-widths widths)
        (do ((col-widths col-widths (cdr col-widths))
             (lin-widths lin-widths (cdr lin-widths)))
            ((null col-widths))
          (setf (car col-widths) (max (car col-widths) (car lin-widths)))))
      ;; compute the top-lines columns:
      (setq top-cols
            (do ((col-widths col-widths (cdr col-widths))
                 (cur-col 0 (+ 1 cur-col (car col-widths))) 
                 (top-cols '()))
                ((null col-widths) (nreverse top-cols))
              (push (+ (/ (car col-widths) 2) cur-col) top-cols)))
      ;; print the titles
      (do ((titles   (reverse titles)   (cdr titles))
           (top-cols (reverse top-cols) (cdr top-cols))
           (right    "" (format "%s|%s"
                                (if (endp (rest titles))
                                    0
                                    (cl:make-string (- (first top-cols) 
                                                     (or (second top-cols) 0) 1)
                                                  :initial-element (cl:character " ")))
                                right))
           name col)
          ((null titles))
        (setq name (car titles))
        (setq col (car top-cols))
        (insert
         (if (<= (length name) col)
             (format "%s%s+%s\n"
                     name 
                     (cl:make-string (- col (length name))
                                   :initial-element (cl:character "-"))
                     right)
             (format "%s%s\n%sV%s\n"
                     name
                     (subseq right (- (length name) col 1))
                     (cl:make-string col  :initial-element (cl:character " "))
                     right))))
      ;; print the lines
      (do ((lines lines (cdr lines))
           (i 0 (1+ i)))
          ((null lines))
        (when (zerop (mod i 5)) (h-bars))
        (do ((line     (car lines) (cdr line))
             (col-widths col-widths (cdr col-widths)))
            ((null line))
          (insert (format (format "%%%ds%s"
                                  (car col-widths)
                                  (if (null (cdr line)) "" " "))
                          (car line))))
        (insert "\n"))
      (h-bars))))



(defun psql:insert-prompt ()
  "Inserts a prompt at the end of the psql buffer."
  (end-of-buffer)
  (insert "\npsql> ")
  (let ((inhibit-read-only t))
    (add-text-properties (point-min) (1- (point-max))
                         '(read-only t)))
  (make-variable-buffer-local 'psql:*input-start*)
  (goto-char (setf psql:*input-start* (point-max))))


(defun psql:insert-error (control-string &rest arguments)
  (goto-char (point-max))
  (let ((start (point)))
    (insert "\nERROR: " (apply (function format) control-string arguments) "\n")
    (add-text-properties start (point)'(face comment)))
  (psql:insert-prompt))

(defmacro* cl:string-case (string-expression &body clauses)
  (let ((value (gensym)))
    `(let ((,value ,string-expression))
       (cond
         ,@(mapcar (lambda (clause)
                     (destructuring-bind (constants &rest body) clause
                       (if (member* constants '(t otherwise) :test (function eql))
                           `(t ,@body)
                           `((member* ,value ',(ensure-list constants)
                                      :test (function string-equal*))
                             ,@body))))
                   clauses)))))

(defun psql:insert-result (result)
  (goto-char (point-max))
  (let ((status (pg:result result :status)))
    (if (string= status "SELECT")
        (let ((rows '()))
          (dolist (row (pg:result result :tuples))
            (push (mapcar (function cl:princ-to-string) row) rows))
          (insert "\n")
          (unless (null rows)
            (psql:format-table (cons (mapcar (function first) (pg:result result :attributes)) (nreverse rows)))))
        (insert (format "\n%s\n" (pg:result result :status))))
    (psql:insert-prompt)))


;;;
;;; The history
;;;

(defstruct (psql:history (:constructor psql:make-history))
  head tail current)

(defun psql:history-current-entry (history)
  (if (eq  (psql:history-current history) :tail)
      nil
      (car (psql:history-current history))))

(defun psql:write-history   (history item)
  (cond
    ((null (psql:history-tail history))
     (setf (psql:history-head history) (setf (psql:history-tail history)  (list item))))
    ((equalp item (car (psql:history-tail history))))
    (t
     (setf (cdr (psql:history-tail history)) (list item)
           (psql:history-tail history)       (cdr (psql:history-tail history)))))
  (setf (psql:history-current history) :new)
  history)

(defun psql:history-previous (history)
  (cond
    ((null (psql:history-current history)))
    ((eq (psql:history-current history) :new)
     (setf (psql:history-current history) (psql:history-tail history)))
    ((eq (psql:history-current history) :tail)
     (setf (psql:history-current history) (psql:history-tail history))
     (psql:history-previous history))
    ((eq (psql:history-current history)  (psql:history-head history))
     (setf (psql:history-current history) nil))
    (t
     (loop
        for previous = (psql:history-head history) then (cdr previous)
        until (eq (cdr previous) (psql:history-current history))
        finally (progn
                  (setf (psql:history-current history) previous)))))
  (psql:history-current-entry history))


(defun psql:history-next (history)
  (cond
    ((null (psql:history-current history))
     (setf (psql:history-current history) (cdr (psql:history-head history)))
     (when (null (psql:history-current history))
       (setf (psql:history-current history) :tail)))
    ((member (psql:history-current history) '(:tail :new))
     nil)
    (t
     (setf (psql:history-current history) (cdr (psql:history-current history)))
     (when (null (psql:history-current history))
       (setf (psql:history-current history) :tail))))
  (psql:history-current-entry history))

;; (let ((h (psql:make-history)))
;;   (psql:write-history h 'one)
;;   (psql:write-history h 'two)
;;   (psql:write-history h 'three)
;;   (psql:history-previous h)
;;   (psql:history-previous h)
;;   (psql:write-history h 'four)
;;   (psql:history-previous h)
;;   (psql:history-previous h)
;;   (psql:history-next h)
;;   (psql:history-next h)
;;   (psql:history-next h)
;;   (psql:history-current-entry h))



;;;
;;; The commands:
;;;

(defun psql-buffer-name (user host port dbname)
  "Returns the name of the psql buffer."
   (format "*postgres://%s@%s:%s/%s*" user host port dbname))

(defvar psql:*databases* '() "Database history for completion.")
(defvar psql:*users*     '() "User history for completion.")
(defvar psql:*hosts*     '() "Host history for completion.")
(defvar psql:*ports*     '() "Port history for completion.")
(defvar psql:*codings*   '() "Coding history for completion.")

(defmacro* psql:history-completion (title history &key (test ''string=) (postprocessing ''identity))
  (let ((form `(completing-read
                ,title
                (mapcar (lambda (x) (cons x nil)) ,history)
                (lambda (object) (pushnew (funcall ,postprocessing object) ,history :test ,test) t)
                nil
                nil
                ',history)))
    (if (eq postprocessing ''identity)
        form
        `(funcall ,postprocessing ,form))))

                     (macroexpand 
                      '(psql:history-completion "Encoding: " psql:*codings* :test (function eql)
                                               :postprocessing (function intern)))

(defun psql (dbname user &optional password host port coding)
  "Creates a psql buffer where the user can insert sql queries and get the results."
  ;; (interactive "sDatabase: \nsUser: \nsPassword: \nsHost: \nnPort: \nzEncoding: ")
  (interactive (list (psql:history-completion "Database: " psql:*databases*)
                     (psql:history-completion "User: "     psql:*users*)
                     (read-passwd "Password: ")
                     (psql:history-completion "Host: "     psql:*hosts*)
                     (psql:history-completion "Port: "     psql:*ports*   :test (function eql))
                     (psql:history-completion "Encoding: " psql:*codings* :test (function eql)
                                              :postprocessing (function intern))))
  (let ((password (or password ""))
        (host     (or host "localhost"))
        (port     (or port 5432))
        (coding   (or coding 'latin-1)))
    (let ((db (pg:connect dbname user password host port coding)))
      (switch-to-buffer (get-buffer-create (psql-buffer-name user host port dbname)))
      (local-set-key (kbd "RET") 'psql-eval-last-expression)
      (local-set-key (kbd "M-p") 'psql-previous-input)
      (local-set-key (kbd "M-n") 'psql-next-input)
      (local-set-key (kbd "C-a") 'psql-beginning-of-line)
      (make-variable-buffer-local 'psql:*db*)
      (setf psql:*db* db)
      (make-variable-buffer-local 'psql:*history*)
      (unless (and (boundp 'psql:*history*) (psql:history-p psql:*history*))
        (setf psql:*history* (psql:make-history)))
      (psql:insert-prompt))))



(defvar *psql-command-map*
  '()

  "An a-list mapping command names (symbols, without the backslash) to
functions (taking any number of arguments, that are parsed as string
from the rest of the line). Example:

\df

")

(defun psql-eval-last-expression ()
  "This command is usually bound to RET, to evaluate the previous SQL query."
  (interactive)
  (let ((sql (cl:string-trim " ;\n\t\v\f" (buffer-substring psql:*input-start* (point-max)))))
    (if (and (plusp (length sql))
             (cl:char= (cl:character "\\") (aref sql 0)))
        (psql:insert-error "Commands are not implemented yet.")
        (progn
          (psql:write-history psql:*history* sql)
          (cl:handler-case (psql:insert-result (pg:exec psql:*db* sql))
            (error (err) (psql:insert-error (cl:princ-to-string err))))))))


(defun psql-previous-input ()
  (interactive)
  (let ((previous (psql:history-previous psql:*history*)))
    (if previous
        (progn
          (delete-region psql:*input-start* (point-max))
          (goto-char (point-max))
          (insert previous))
        (beep))))

(defun psql-next-input ()
  (interactive)
  (let ((next (psql:history-next psql:*history*)))
    (if next
        (progn
          (delete-region psql:*input-start* (point-max))
          (goto-char (point-max))
          (insert next))
        (beep))))

(defun psql-bol-point (&optional point)
  "Return the point of the beginning of line (current line, of line at `point')."
  (save-excursion
    (when point (goto-char point))
    (beginning-of-line)
    (point)))

(defun psql-eol-point (&optional point)
  "Return the point of the end of line (current line, of line at `point')."
  (save-excursion
    (when point (goto-char point))
    (end-of-line)
    (point)))


(defun psql-beginning-of-line ()
  (interactive)
  (if (and (<= (psql-bol-point psql:*input-start*) (point))
           (<= (point) (psql-eol-point psql:*input-start*)))
      (goto-char psql:*input-start*)
      (beginning-of-line)))

(defun psql-kill-ring-save-history ()
  "Put the whole psql command history into the kill ring."
  (interactive)
  (let ((history psql:*history*))
    (with-temp-buffer
      (dolist (item (psql:history-head history))
        (insert item ";\n"))
      (kill-ring-save (point-min) (point-max)))))



;;;; THE END ;;;;

