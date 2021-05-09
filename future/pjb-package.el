;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               package.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;NOTE
;;;;
;;;;    To implement a package system, we need foremost a hook in the
;;;;    lisp reader, to be able to interpret symbol names and
;;;;    qualified symbol names as we wish.
;;;;
;;;;    Given a reader hooks, we can use the native intern or
;;;;    implement it otherwise (emacs:intern vs. cl:intern).
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-12-17 <PJB> Created.
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

(require 'cl)


(defstruct cl:package
  name
  (external-table   (make-hash-table))
  (present-table    (make-hash-table))
  (shadowing-table  (make-hash-table))
  (used-packs       '())
  (used-by-packs    '())
  (nicknames        '())
  (documentation    nil)
  obarray)

(defun make-obarray (length) (make-vector length 0))

(defvar cl::*emacs-lisp-package*
  (make-cl:package
   :name "EMACS-LISP"
   :nicknames '("emacs-lisp"  "ELISP" "elisp")
   :documentation "The package exporting the emacs lisp language symbols."
   :obarray (make-obarray (length obarray))))

(defvar cl::*emacs-package*
  (make-cl:package
   :name "EMACS"
   :nicknames '("emacs")
   :documentation "The package exporting the emacs symbols."
   :obarray (make-obarray (length obarray))))

(defvar cl::*emacs-user-package*
  (make-cl:package
   :name "EMACS-USER"
   :nicknames '("emacs-user")
   :documentation "The package where emacs user symbols are interned into."
   :obarray obarray))


(defvar cl:*package* cl::*emacs-user-package*)

(defun cl:export (symbols &optional (package cl:*package*))
  )

(defun cl:use-package (used-package &optional (using-package cl:*package*))
  )

(defmacro cl:in-package (new-package)
  `(eval-when (compile load eval)
     (let ((new-package (cl:find-package new-package)))
       (when new-package
         (setf obarray (cl:package-obarray new-package)
               cl:*package* new-package)))))

(defvar cl::*package-list* (list
                            cl::*emacs-lisp-package*
                            cl::*emacs-package*
                            cl::*emacs-user-package*))

(defun cl:list-all-packages ()
  (copy-list cl::*package-list*))


(defun cl:intern (name &optional (package cl:*package*)))

(cl:export '(lambda if cond car rplaca #| … |#)
           cl::*emacs-lisp-package*)

(cl:use-package cl::*emacs-lisp-package* cl::*emacs-package*)

(cl:export '(buffer find-file make-frame #| … |#
             )
           cl::*emacs-package*)

(cl:use-package cl::*emacs-package* cl::*emacs-user-package*)






;; (defvar obarray (make-obarray 113) "Symbol table for use by `intern' and `read'.")


;; (defun intern (STRING &optional OBARRAY))
;; (defun intern-soft (STRING &optional OBARRAY))
;; (defun mapatoms (FUNCTION &optional OBARRAY))
;; (defun unintern (SYMBOL-OR-STRING OBARRAY))
;;
;; (defun read (&optional STREAM))
;;
;;
;; (defun symbol-plist (symbol))
;; (defun setplist (symbol plist))
;; (defun get (symbol property))
;; (defun put (symbol property value))


(defun obarray-symbols (obarray)
  (let ((symbols '()))
    (mapatoms (lambda (symbol) (push symbol symbols)) obarray)
    (sort symbols (function string<))))

;; (length (obarray-symbols obarray))
;; 89918


;; (let ((results '()))
;;   (do-symbols (n)
;;     (when (and (fboundp n)
;;                (not (symbolp (symbol-function n)))
;;                (not (subrp   (symbol-function n))))
;;       (let ((pl (function-parameter-list n)))
;;         ;; (insert (format "%S %S\n" n pl))
;;         (when (and (listp pl) (ignore-errors (member 'obarray pl)))
;;           (push (cons n pl) results)))))
;;   results)



;; (defvar ob1 (make-obarray 113))
;; (defvar ob2 (make-obarray 113))
;;
;; (defvar s1 (intern "Hello" ob1))
;; (intern "World" ob1)
;; (mapatoms 'print ob1)
;; (eq 'Hello s1)

;; (defparameter *table*
;;   (let* ((symbols (obarray-symbols obarray))
;;          (table (make-hash-table :test 'eql :size (* 3 (length symbols)))))
;;     (dolist (sym symbols table)
;;       (setf (gethash (symbol-name sym) table) sym))))
;;
;;
;; (let* ((names (mapcar 'symbol-name (obarray-symbols obarray))))
;;   (insert (time (dolist (name names)
;;                   (intern name obarray)))
;;           (time (dolist (name names)
;;                   (gethash name *table*)))))
;;
;;
;; (intern 'hello ob1)
;; (hash-table-count *table*)

