;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;;;**************************************************************************
;;;;FILE:               room.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implement a room function for emacs lisp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-06 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2013
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
(require 'pjb-emacs)
(require 'pjb-strings)


(defstruct room-type-entry
  type
  (count 0)
  (total-size 0))

(defstruct room-object-entry
                object
                (reference-count 1))

;; (pp(macroexpand '))
;; (progn
;;   (defvar cl-struct-room-object-entry-tags)
;;   (cl-defsubst room-object-entry-object #1=(cl-x)
;;                (or #2=(and #4=(vectorp cl-x)
;;                            #5=(>=
;;                                (length cl-x)
;;                                3)
;;                            #6=(memq
;;                                (aref cl-x 0)
;;                                cl-struct-room-object-entry-tags))
;;                    (error #3="%s accessing a non-%s" 'room-object-entry-object 'room-object-entry))
;;                (aref cl-x 1))
;;   (cl-defsubst room-object-entry-reference-count #1#
;;     (or #2#
;;         (error #3# 'room-object-entry-reference-count 'room-object-entry))
;;     (aref cl-x 2))
;;   (cl-defsubst room-object-entry-p
;;       (cl-x)
;;     (and #4# #5# #6# t))
;;   (defun copy-room-object-entry
;;       (x)
;;     (copy-sequence x))
;;   (cl-defsubst make-room-object-entry
;;       (&cl-defs
;;        '(nil . #7=((cl-tag-slot)
;;                    (object)
;;                    (reference-count 1)))
;;        &key object reference-count)
;;     (vector 'cl-struct-room-object-entry object reference-count))
;;   (setq cl-struct-room-object-entry-tags
;;         (list 'cl-struct-room-object-entry))
;;   (cl-eval-when
;;       (compile load eval)
;;     (put 'room-object-entry 'cl-struct-slots '#7#)
;;     (put 'room-object-entry 'cl-struct-type
;;          '(vector nil))
;;     (put 'room-object-entry 'cl-struct-include 'nil)
;;     (put 'room-object-entry 'cl-struct-print t)
;;     (put 'make-room-object-entry #8='side-effect-free 't)
;;     (put 'copy-room-object-entry #8# 't)
;;     (put 'room-object-entry-p #8# 'error-free)
;;     (put 'room-object-entry-reference-count #8# 't)
;;     (put 'room-object-entry-object #8# 't))
;;   'room-object-entry)




(defparameter *emacs-lisp-types*
  '(symbol
    integer string cons marker overlay float
    window-configuration process window subr compiled-function buffer
    char-table bool-vector frame hash-table font-spec font-entity font-object
    vector))

(defconstant +word-size+ (if (< most-positive-fixnum 4294967296.0)
                             4 ; bytes
                             8))

(defun type-size (type)
  (ecase type
    (float                (* 1 +word-size+))
    (integer              (* 1 +word-size+))
    (cons                 (* 2 +word-size+))
    (marker               (* 2 +word-size+))
    (symbol               (* 4 +word-size+))
    (overlay              'unknown)
    (window-configuration 'unknown)
    (process              'unknown)
    (window               'unknown)
    (frame                'unknown)
    (font-spec            'unknown)
    (font-entity          'unknown)
    (font-object          'unknown)
    (string               'variable)
    (subr                 'variable)
    (compiled-function    'variable)
    (buffer               'variable)
    (char-table           'variable)
    (bool-vector          'variable)
    (hash-table           'variable)
    (vector               (let ((slots (get type 'cl-struct-slots)))
                            (if slots
                                (length slots)
                                'variable)))))

(defun object-size (object)
  (let ((size (type-size (type-of object))))
    (cond
      ((integerp size)
       size)
      ((eq 'unknown size)
       1)
      (t
       (* +word-size+
          (ceiling
           (typecase object
             (string      (+ (* 1 +word-size+) (length object)))
             (vector      (+ (* 1 +word-size+) (* +word-size+       (length object))))
             (bool-vector (+ (* 1 +word-size+) (* (/ +word-size+ 8) (length object))))
             (hash-table  (+ (* 4 +word-size+) (* 2 +word-size+     (hash-table-size object))))
             (buffer      (+ (* 32 +word-size+) (length (buffer-name object)) (buffer-size object)))
             (t           1))
           +word-size+))))))


(defun structure-type-p (symbol)
  (and (symbolp symbol)
       (prefixp "cl-struct-" (symbol-name symbol))))

(defun structure-predicate (symbol)
  (if (structure-type-p symbol)
      (let ((predicate  (intern (concat (subseq (symbol-name symbol) 10) "-p"))))
        (if (fboundp predicate)
            predicate
            (constantly nil)))
      (constantly nil)))

(defun structurep (object)
  (and (vectorp object)
       (plusp (length object))
       (symbolp (aref object 0))
       (funcall (structure-predicate (aref object 0)) object)
       (intern (subseq (symbol-name (aref object 0)) 10))))


;; (structurep (make-room-object-entry))
;; room-object-entry
;; 
;; (structurep [cl-struct-room-object-entry nil])
;; nil
;; 
;; (structurep [cl-struct-XYZ nil 1 2 3])
;; nil

(put 'cl-labels 'lisp-indent-function  '((&whole 4 &rest (&whole 1 &lambda &body)) &body))
(put 'cl-flet   'lisp-indent-function  '((&whole 4 &rest (&whole 1 &lambda &body)) &body))


(defun room ()
  "Returns a description of the memory use."
  (let ((all-objects (make-hash-table :size 100000))
        (all-types   (make-hash-table :size 100)))
    (dolist (type *emacs-lisp-types*)
      (setf (gethash type all-types) (make-room-type-entry :type type)))
    (cl-labels ((walk-root (first-object)
                  (loop
                     with objects = (list first-object)
                     for object = (pop objects)
                     do (cl-flet ((walk (object) (push object objects)))
                          (let ((entry (gethash object all-objects)))
                            (if entry
                                (incf (room-object-entry-reference-count entry))
                                (progn
                                  (setf (gethash object all-objects) (make-room-object-entry :object object))
                                  (let* ((type   (or (structurep object) (type-of object)))
                                         (tentry (gethash type all-types)))
                                    (unless tentry
                                      (setf tentry (setf (gethash type all-types) (make-room-type-entry :type type))))
                                    (incf (room-type-entry-count tentry))
                                    (incf (room-type-entry-total-size tentry) (object-size object)))
                                  (ecase (type-of object)
                                    ((integer))
                                    ((float))
                                    ((string) ; properties?
                                     )
                                    ((marker)
                                     (walk (marker-buffer object))
                                     (walk (marker-insertion-type object))
                                     (walk (marker-position object)))
                                    ((cons)
                                     (walk (car object))
                                     (walk (cdr object)))
                                    ((overlay))
                                    ((window-configuration))
                                    ((process))
                                    ((window))
                                    ((subr))
                                    ((compiled-function))
                                    ((buffer)
                                     (walk (buffer-name object))
                                     (walk (buffer-file-name object))
                                     (walk (buffer-base-buffer object)))
                                    ((char-table))
                                    ((bool-vector))
                                    ((frame)   (walk (frame-buffer-list object))
                                     (walk (frame-parameters object))
                                     (walk (frame-title object))
                                     ;; …
                                     )
                                    ((hash-table)  (maphash (lambda (k v)
                                                              (walk k)
                                                              (walk v)) object)
                                     (walk (hash-table-test object)))
                                    ((font-spec))
                                    ((font-entity))
                                    ((font-object))
                                    ((vector)  (map nil (function walk) object))
                                    ((symbol)  (when (fboundp object)
                                                 (walk (symbol-function object)))
                                     (when (boundp object)
                                       (walk (symbol-value object)))
                                     (walk (symbol-plist object))
                                     (dolist (buffer (buffer-list))
                                       (walk (symbol-value-in-buffer 'x buffer)))))))))
                     while objects)))
      (map nil (function walk-root) (frame-list))
      (map nil (function walk-root) (buffer-list))
      (do-symbols (symbol) (walk-root symbol))
      (let ((results '()))
        (maphash (lambda (type entry)
                   (declare (ignore type))
                   (push (list (room-type-entry-type entry)
                               (room-type-entry-count entry)
                               (room-type-entry-total-size entry)) results))
                 all-types)
        results))))


;; (load (progn (byte-compile-file "room.el") "room.elc"))
;; (room)
;; ((room-type-entry 21 840)
;;  (room-object-entry 83 2656)
;;  (slime-test 58 4176)
;;  (isearch--state 15 1680)
;;  (ftree 695 22240)
;;  (fo-cache 1 56)
;;  (ossn 4096 360448)
;;  (lambda-list 752 72192)
;;  (slime-contrib 20 1440)
;;  (slime-repl-shortcut 26 1248)
;;  (palette 37 2368)
;;  (lisp-implementation 8 704)
;;  (vector 10082 676720)
;;  (font-object 0 0)
;;  (font-entity 0 0)
;;  (font-spec 0 0)
;;  (hash-table 25 1673056)
;;  (frame 1 1)
;;  (bool-vector 0 0)
;;  (char-table 137 1096)
;;  (buffer 25 2916080)
;;  (compiled-function 14859 118872)
;;  (subr 1105 8840)
;;  (window 3 3)
;;  (process 1 1)
;;  (window-configuration 0 0)
;;  (float 304 2432)
;;  (overlay 11 11)
;;  (marker 57 912)
;;  (cons 655489 10487824)
;;  (string 67635 2927056)
;;  (integer 23823 190584)
;;  (symbol 58332 1866624))



