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


(defstruct package
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

(defvar obarray (make-obarray 113) "Symbol table for use by `intern' and `read'.")

(defun intern (STRING &optional OBARRAY))
(defun intern-soft (STRING &optional OBARRAY))
(defun mapatoms (FUNCTION &optional OBARRAY))
(defun unintern (SYMBOL-OR-STRING OBARRAY))

(defun read (&optional STREAM))


(defun symbol-plist (symbol))
(defun setplist (symbol plist))
(defun get (symbol property))
(defun put (symbol property value))



(defun obarray-symbols (obarray)
  (let ((symbols '()))
    (mapatoms (lambda (symbol) (push symbol symbols)) obarray)
    (sort symbols (function string<))))

(obarray-symbols obarray)


(let ((results '()))
  (do-symbols (n)
    (when (and (fboundp n)
               (not (symbolp (symbol-function n)))
               (not (subrp   (symbol-function n))))
      (let ((pl (function-parameter-list n)))
        ;; (insert (format "%S %S\n" n pl))
        (when (member 'OBARRAY pl)
          (push (cons n pl) results)))))
  results)


(defvar ob1 (make-obarray 113))
(defvar ob2 (make-obarray 113))

(defvar s1 (intern "Hello" ob1))
(intern "World" ob1)
(mapatoms 'print ob1)
(eq 'Hello s1)

(defparameter *table*
  (let* ((symbols (obarray-symbols obarray))
         (table (make-hash-table :test 'eql :size (* 3 (length symbols)))))
    (dolist (sym symbols table)
      (setf (gethash (symbol-name sym) table) sym))))


(let* ((names (mapcar 'symbol-name (obarray-symbols obarray))))
  (insert (time (dolist (name names)
                  (intern name obarray)))
          (time (dolist (name names)
                  (gethash name *table*)))))


(intern 'hello ob1)
(hash-table-count *table*)

