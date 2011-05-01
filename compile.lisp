;;;;****************************************************************************
;;;;FILE:               compile.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Supplements the emacs Makefile.
;;;;    
;;;;    Usage:  (load "compile.lisp")
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-08-01 <PJB> Created to generate summary.html
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2006 - 2011
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
;;;;****************************************************************************

;; (defpackage "COM.INFORMATIMAGO.COMMON-LISP.COMPILE"
;;   (:use "COMMON-LISP")
;;   (:export "MAIN"))
;; (in-package "COM.INFORMATIMAGO.COMMON-LISP.COMPILE")


;;; Not used yet:
(defvar *PREFIX* "/usr/local/")
(defvar *MODULE* "emacs")
(defvar *PACKAGE-PATH* "com/informatimago/emacs")
;;; ----


(load "init.lisp")
;; package.lisp is loaded by init.lisp.
;;(package:load-package :com.informatimago.common-lisp.make-depends)
(setf package:*PACKAGE-VERBOSE* nil) 
(package:load-package "COM.INFORMATIMAGO.COMMON-LISP.LIST")
;;(package:load-package "COM.INFORMATIMAGO.COMMON-LISP.MAKE-DEPENDS")
(load "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;MAKE-DEPENDS.LISP")
(LOAD "PACKAGES:NET;SOURCEFORGE;CCLAN;ASDF;ASDF.LISP")
(push (function package:PACKAGE-SYSTEM-DEFINITION)
      ASDF:*SYSTEM-DEFINITION-SEARCH-FUNCTIONS*)

(defparameter *source-type* "el")

(COM.INFORMATIMAGO.COMMON-LISP.MAKE-DEPENDS:generate-summary
 *sources*
 :source-type *source-type*
 :summary-path "summary.html"
 ;; :comment-start ";;"
 :verbose t
 :repository-url (lambda (pp)
                   (format nil
                           ;; "http://darcs.informatimago.com~
                           ;;  /darcs/public/emacs/~(~A/~A~).el"
                           "~(~A~)"
                           (file-namestring
                            (merge-pathnames
                             (make-pathname :type *source-type* :defaults pp)
                             pp nil)))))



 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
