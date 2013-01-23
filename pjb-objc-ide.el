;;;; -*- mode:emacs-lisp; coding:utf-8; lexical-binding:t -*-
;;;;**************************************************************************
;;;;FILE:               pjb-objc-ide.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Some Objective-C refactoring tools.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pbourguignon@dxo.com>
;;;;MODIFICATIONS
;;;;    2013-01-22 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2013 - 2013
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(require 'cl)
(require 'cedet)
(require 'pjb-strings)
(require 'pjb-emacs)
(require 'pjb-objc-parser)


(defvar *pjb-objc-ide--nslog-function* "NSLog"
  "The Objective-C function to us to log messages.
It should have the same signature as NSLog.")

(defvar *pjb-objc-ide--entry-log-tag* "DEBUG"
  "A tag that will be put in comment on the lines inserted by
`pjb-objc-ide--add-method-entry-log'.")



(defun pjb-objc-ide--ns-class-type-p (type)
  ;; TODO: It would be better to collect the exact list of NS classes…
  (and (listp type)
       (eq (second type) '*)
       (null (cddr type))
       (symbolp (first type))
       (string= "NS" (substring (symbol-name (first type)) 0 2))))



(defun pjb-objc-ide--rect-formatter-arguments (argument)
  (mapcar (lambda (fmt) (intern (format fmt argument)))
          '("%s.origin.x" "%s.origin.y" "%s.size.width" "%s.size.height")))

(defun pjb-objc-ide--point-formatter-arguments (argument)
  (mapcar (lambda (fmt) (intern (format fmt argument)))
          '("%s.x" "%s.y")))

(defun pjb-objc-ide--size-formatter-arguments (argument)
  (mapcar (lambda (fmt) (intern (format fmt argument)))
          '("%s.width" "%s.height")))

(defun pjb-objc-ide--range-formatter-arguments (argument)
  (mapcar (lambda (fmt) (intern (format fmt argument)))
          '("%s.location" "%s.length")))


(defparameter *pjb-objc-ide--type-formatter-map*
  '(((NSUInteger)                  . "%d")
    ((NSInteger)                   . "%d")
    ((BOOL)                        . "%d")
    ((CGFloat)                     . "%f")
    ((int)                         . "%d")
    ((unsigned)                    . "%u")
    ((unsigned int)                . "%u")
    ((char)                        . "%c")
    ((char *)                      . "%s")
    ((const char *)                . "%s")
    ((id)                          . "%@")
    ((NSRange)                     . ("{l=%d,c=%d}" pjb-objc-ide--range-formatter-arguments))
    ((NSPoint)                     . ("{x=%f,y=%f}" pjb-objc-ide--point-formatter-arguments))
    ((NSSize)                      . ("{w=%f,h=%f}" pjb-objc-ide--size-formatter-arguments))
    ((NSRect)                      . ("{{x=%f,y=%f},{w=%f,h=%f}}" pjb-objc-ide--rect-formatter-arguments))
    (pjb-objc-ide--ns-class-type-p . "%@")) 
  "A list of conses.

The car slots are lists representing Objective-C types,
or a predicate name to be called to match the type.

The cdr slots are NSLog formatters: either a string to be inserted
into the formatter string of NSLog, or a list containing a formatter
string, and a function to build a list of arguments to NSLog for this
formatter string, from the expression to be formatted.
")


(defun pjb-objc-ide--formatter-for-type (type)
  (loop
     for (type-match . formatter) in *pjb-objc-ide--type-formatter-map*
     do (when (if (and (atom type-match)
                       (fboundp type-match))
                  (funcall type-match type)
                  (equal type-match type))
          (return formatter))
     finally (return nil)))


(assert (equal (mapcar 'pjb-objc-ide--formatter-for-type
                 '((NSInteger) (NSUInteter) (BOOL) (char) (char *) (const char *) (id)
                   (NSString *) (NSMutableString *) (NSDictionary *) (NSMutableArray *)
                   (something *) (something)))
               '("%d" nil "%d" "%c" "%s" "%s" "%@" "%@" "%@" "%@" "%@" nil nil)))



(defun pjb-objc-ide--princ-to-string (x)
  (let ((print-circle nil))
    (format "%s" x)))


(defun pjb-objc-ide--process-method (processor &optional count)

  "Process method implementations.

If `count' is negative then processes all the implementation methods
from the point down to the end of the buffer, otherwise process the
given number of methods (default 1).

Calls the function `processor' with as arguments, the method
signature, the start point and end point of the body region (including
braces), and with the current point before the opening brace (=
start-pt).

The excursion is saved while running `processor'.
"
  (loop
     with count = (or count 1)
     for i from 0         
     while (or (minusp count) (< i count))
     do
       (incf i)
       (if (re-search-forward "\\(\\s-\\|\n\\)*[-+]\\(\\s-\\|\n\\)*(" nil t)
           (progn
             (goto-char (match-beginning 0))
             (let ((signature (pjb-objc-parser--parse-method-signature)))
               (when (looking-at "\\(\\s-\\|\n\\)*{")
                 (forward-sexp)
                 (let ((end-pt (point)))
                   (backward-sexp)
                   (save-excursion
                     (funcall processor signature (point) end-pt))))))
           (return))))




(defun pjb-objc-ide--insert-tag ()
  "Insert a tag comment on a newline."
  (insert (format "  /*** %s ***/ " *pjb-objc-ide--entry-log-tag*)))


(defun pjb-objc-ide--add-method-entry-log (&optional count)
  (interactive "p")
  (let ((nil-types '()))
    (pjb-objc-ide--process-method
     (lambda (signature start-pt end-pt)
       (down-list)
       (insert "\n")
       (pjb-objc-ide--insert-tag)
       (insert (format "%s(@\"%%s" *pjb-objc-ide--nslog-function*))
       (let ((formatter-parameters '()))
         (dolist (param (pjb-objc-method-signature-parameters signature))
           (let* ((type      (pjb-objc-parameter-type param))
                  (formatter (pjb-objc-ide--formatter-for-type type)))
             (if formatter
                 (let ((args      (if (listp formatter)
                                      (funcall (second formatter) (pjb-objc-parameter-name param))
                                      (list (pjb-objc-parameter-name param))))
                       (formatter (if (listp formatter)
                                      (first formatter)
                                      formatter)))
                   (push (cons (pjb-objc-parameter-name param) args) formatter-parameters)
                   (insert (format ", %%@ = %s" formatter)))
                 (pushnew type nil-types :test (function equal)))))
         (insert "\", __PRETTY_FUNCTION__")
         (dolist (param (reverse formatter-parameters))
           (insert (format ", @\"%s\", %s"
                           (car param)
                           (mapconcat (function pjb-objc-ide--princ-to-string) (cdr param)  ", ")))))
       (insert ");"))
     count)
    (when nil-types
      (message "Objective-C types without a formatter (see `*pjb-objc-ide--type-formatter-map*'): %S"
               nil-types))))



(defun pjb-objc-ide--add-method-return-log (&optional count)
  (interactive "p")
  (let ((nil-types '()))
    (pjb-objc-ide--process-method
     (lambda (signature start-pt end-pt)
       ;; We assume return statements are never left alone, but are
       ;; always in a block.  Therefore we can insert before them as
       ;; many statements we want, including another return statement.
       ;;
       ;; {
       ;;     return a+b;
       ;; }
       ;; --> 
       ;; {
       ;;     /*** TAG ***/ ResultTypeFromMethodSignature result=a+b;
       ;;     /*** TAG ***/ NSLog(@"%p returns %d", __PRETTY_FUNCTION__, result);
       ;;     /*** TAG ***/ return result; 
       ;;     return a+b;
       ;; }    
       (with-marker (end-pt end-pt)
         (loop
            while (re-search-forward "^\\s-*return \\([^;]*\\);" end-pt t)
            do
              (with-marker (mend (match-end 0))
                (let ((result-type (pjb-objc-method-signature-result-type signature))
                      (result-variable (gensym "result_"))
                      (result-expression (match-string 1)))
                  (goto-char (match-beginning 0))
                  (pjb-objc-ide--insert-tag) (insert (format "%s %s=%s;\n"
                                                             (mapconcat (function pjb-objc-ide--princ-to-string) result-type " ")
                                                             result-variable
                                                             result-expression))
                  (pjb-objc-ide--insert-tag) (insert (format "%s(@\"%%s returns "
                                                             *pjb-objc-ide--nslog-function*))
                  (let ((formatter (pjb-objc-ide--formatter-for-type result-type)))
                    (if formatter
                        (let ((results   (if (listp formatter)
                                             (funcall (second formatter) result-variable)
                                             (list result-variable)))
                              (formatter (if (listp formatter)
                                             (first formatter)
                                             formatter)))
                          (insert (format "%s\", __PRETTY_FUNCTION__, %s"
                                          formatter
                                          (mapconcat (function pjb-objc-ide--princ-to-string) results  ", "))))
                        (pushnew result-type nil-types :test (function equal))))
                  (insert ");\n")
                  (pjb-objc-ide--insert-tag) (insert (format "return %s;\n" result-variable)))
                (goto-char mend)))))
     count)
    (when nil-types
      (message "Objective-C types without a formatter (see `*pjb-objc-ide--type-formatter-map*'): %S"
               nil-types))))






(provide 'pjb-objc-ide)




(defun dxo-objc-ide--dxo-class-type-p (type)
  ;; TODO: It would be better to collect the exact list of OPM classes…
  (and (listp type)
       (eq (second type) '*)
       (null (cddr type))
       (symbolp (first type))
       (member* (substring (symbol-name (first type)) 0 3) '("DOP" "DXF")
                :test (function string=))))


(defun dxo-objc-ide-settings ()
  (interactive)
  (setf *pjb-objc-ide--nslog-function* "DXFLogDebug"
        *pjb-objc-ide--entry-log-tag*  "PJB-DEBUG")
  (pushnew '((ZoomMode) . "%d")  *pjb-objc-ide--type-formatter-map*
           :test (function equal))
  (pushnew '(dxo-objc-ide--dxo-class-type-p . "%@") *pjb-objc-ide--type-formatter-map*
           :test (function equal)))


(dxo-objc-ide-settings)

