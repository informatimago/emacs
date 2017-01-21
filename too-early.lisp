;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               too-early.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A proof-of-concept-so-far tracing CL package.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-19 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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
;;;;

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.TRACING-COMMON-LISP"
  (:nicknames "TRACING-COMMON-LISP" "TRACING-CL" "TOO-EARLY")
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM")
  (:shadow "LET" "LET*" "SETQ" "SETF"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.TRACING-COMMON-LISP")


(defmacro trace-places (op &rest places)
  `(progn
     ,@(mapcar (lambda (place)
                 `(format *trace-output*
                         "~&After ~8S  ~12S = ~S~%" ',op
                         ',(if (listp place) (first place) place)
                         ,(if (listp place) (second place) place)))
               places)))


(defmacro LET (bindings &body body)
  `(cl:let ,bindings
     (trace-places cl:let ,@(mapcar (lambda (binding) (if (listp binding) (first binding) binding))
                                    bindings))
     ,@body))


(defmacro LET* (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      `(cl:let (,(first bindings))
         (trace-places cl:let* ,(if (listp (first bindings)) (first (first bindings)) (first bindings)))
         (let* ,(cdr bindings)
           ,@body))))


(defmacro SETQ (&rest var-vals)
  (if (null var-vals)
      `(cl:setq)
      (destructuring-bind (var val &rest others) var-vals
        (if others
            `(progn
               (cl:setq ,var ,val)
               (trace-places cl:setq ,var)
               (setq ,@others))
            `(prog1
                 (cl:setq ,var ,val)
               (trace-places cl:setq ,var))))))


(defmacro SETF (&rest place-vals)
  (flet ((gen-setf (place val)
           (multiple-value-bind (dummies vals new setter getter) (get-setf-expansion place)
             (when (cdr new) (error "Can't expand ~S" place))
             `(cl:let* (,@(mapcar #'list dummies vals)
                     (,(car new) ,getter))
                (multiple-value-prog1
                    (cl:setf ,setter ,val)
                  (trace-places cl:setf ,(list place getter)))))))
    (if (null place-vals)
        `(cl:setf)
        (destructuring-bind (place val &rest others) place-vals
          (if others
              `(progn
                 ,(gen-setf place val)
                 (setf ,@others))
              (gen-setf place val))))))



(defun make-argument-list (lambda-list)
  (cl:let ((lambda-list (parse-lambda-list lambda-list)))
    (values (append
             (mapcar (function parameter-name) (lambda-list-mandatory-parameters lambda-list))
             (mapcar (function parameter-name) (lambda-list-optional-parameters lambda-list))
             (cond
               ((lambda-list-keyword-parameters lambda-list)
                (mapcan (lambda (parameter)
                          (list (ENSURE-PARAMETER-KEYWORD parameter) (parameter-name parameter)))
                        (lambda-list-keyword-parameters lambda-list)))

               (t '())))
            (when (lambda-list-rest-parameter-p lambda-list)
              (parameter-name (lambda-list-rest-parameter lambda-list)))
            (append
             (mapcar (function parameter-name) (lambda-list-mandatory-parameters lambda-list))
             (mapcar (function parameter-name) (lambda-list-optional-parameters lambda-list))
             (cond
               ((lambda-list-keyword-parameters lambda-list)
                (mapcan (lambda (parameter)
                          (list (ENSURE-PARAMETER-KEYWORD parameter) (parameter-name parameter)))
                        (lambda-list-keyword-parameters lambda-list)))
               ((lambda-list-rest-parameter-p lambda-list)
                (list (parameter-name (lambda-list-rest-parameter lambda-list))))
               (t '()))))))


(defun make-test-call (defun-form bindings)
  (if (and (listp defun-form)
           (eq 'defun (first defun-form))
           (symbolp (second defun-form))
           (listp (third defun-form)))
      `(cl:let ,bindings
         ,(multiple-value-bind (arguments rest) (make-argument-list (third defun-form))
                               (if rest
                                   `(apply (function ,(second defun-form)) ,@arguments ,rest)
                                   `(,(second defun-form) ,@arguments))))
      `(progn)))

(declaim (inline make-test-call))


(defun eval-and-trace (defun-text &optional bindings)
  (cl:multiple-value-bind (form error)
      (ignore-errors (values
                      (cl:let* ((*package* (find-package "TRACING-CL")))
                        (read-from-string defun-text))))
    (or error
        (cl:multiple-value-bind (results error)
            (ignore-errors (multiple-value-list (eval form)))
          (if error
              (values error nil
                      (nth-value 2 (make-argument-list (third form))))
              (values nil results
                      (nth-value 2 (make-argument-list (third form)))
                      (ignore-errors (multiple-value-list (eval (make-test-call form bindings))))))))))


;; (eval-and-trace "(defun f (x) (if (zerop x) x (* x (f (1- x)))))")
;; nil
;; (f)
;; (x)
;; nil
;;
;; (eval-and-trace "(defun f (x) (if (zerop x) x (* x (f (1- x)))))"
;;                 '((x 42)))
;; nil
;; (f)
;; (x)
;; (0)
;;
;; (eval-and-trace "(defun f (x) (if (zerop x) 1 (* x (f (1- x)))))"
;;                 '((x 42)))
;; nil
;; (f)
;; (x)
;; (1405006117752879898543142606244511569936384000000000)


;; (eval-and-trace "(defun g (a b) (let ((c (+ a b))) (* c c)))")
;; ;Compiler warnings :
;; ;   In an anonymous lambda form: Undeclared free variable a
;; ;   In an anonymous lambda form: Undeclared free variable b
;; nil
;; (g)
;; (a b)
;; nil
;;
;; (eval-and-trace "(defun g (a b) (let ((c (+ a b))) (* c c)))"
;;                 '((a 2) (b 3)))
;;
;; After common-lisp:let,  c            = 5
;; nil
;; (g)
;; (a b)
;; (5)


;; tracing-common-lisp>  (eval-and-trace "(defun f (x) (if (zerop x) 1 (let ((r (* x (f (1- x))))) r)))"
;;                                       '((x 10)))
;; After common-lisp:let  r            = 1
;; After common-lisp:let  r            = 2
;; After common-lisp:let  r            = 6
;; After common-lisp:let  r            = 24
;; After common-lisp:let  r            = 120
;; After common-lisp:let  r            = 720
;; After common-lisp:let  r            = 5040
;; After common-lisp:let  r            = 40320
;; After common-lisp:let  r            = 362880
;; After common-lisp:let  r            = 3628800
;; nil
;; (f)
;; (x)
;; (3628800)
;; tracing-common-lisp>
