;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:               pjb-object.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This is a root class for my classes. 
;;;;    The main purpose is to implement here compatibility stuff.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon 
;;;;MODIFICATIONS
;;;;    2002-09-08 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2002 - 2011
;;;;
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This library is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    Lesser General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Lesser General Public
;;;;    License along with this library; if not, write to the Free Software
;;;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
;;;;
;;;;******************************************************************************
(require 'pjb-cl)
(require 'eieio)

(provide 'pjb-object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'make-instance 'lisp-indent-function 2)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PjbObject

(defclass PjbObject nil
  (;;attributes
   (object-id
    :initform nil
    :initarg  :object-id
    :accessor object-id
    :type (or null string)
    :documentation 
    "The id of this object (eieio has it in its internals, but not CLOS).")
   );;end attributes
  (:documentation
   "This is a root class for my classes. 
    The main purpose is to implement here compatibility stuff.")
  );;PjbObject



(defmacro defmethod* (name &rest things)
  "In emacs-24 eieio rejects defmethod with more than one specialization."
  (let* ((qualifiers (loop while (and (atom (car things)) (not (null (car things))))
			collect (pop things)))
	 (lambda-list (pop things))
	 (body things)
	 (mandatories (loop
			 while (and lambda-list
				    (not (member (car lambda-list) '(&optional &rest &body &key &allow-other-keys &aux))))
			 collect (pop lambda-list)))
	 (other-parameters lambda-list)
	 (first-parameter (pop mandatories))
	 (checks '()))
    `(defmethod ,name ,@qualifiers
       (,first-parameter
	,@(mapcar (lambda (parameter)
		    (if (listp parameter)
			(progn
			  (push parameter checks)
			  (first parameter))
			parameter))
		  mandatories)
	,@other-parameters)
       ,@(mapcar (lambda (check)
		   (cons 'check-type check))
		 checks)
       ,@body)))


;;;; THE END ;;;;
