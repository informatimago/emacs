;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:               pjb-transpose.el
;;;;LANGUAGE:           emacs-lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This file exports functions to transpose or rotate the
;;;;    characters of a region.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon
;;;;MODIFICATIONS
;;;;    2003-02-14 <PJB> Created.
;;;;BUGS
;;;;    TODO:  implement a region-of-word-to-array to be  able to transpose
;;;;           word by word instead of char by char.
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2003 - 2014
;;;;    mailto:pjb@informatimago.com
;;;;
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later
;;;;    version.
;;;;
;;;;    This library is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU Lesser General Public License for more
;;;;    details.
;;;;
;;;;    You should have received a copy of the GNU Lesser General
;;;;    Public License along with this library; if not, write to the
;;;;    Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;******************************************************************************
(require 'pjb-cl)
(require 'pjb-strings)

(defun region-to-array (start end)
  (interactive "r")
  (let* ((lines  (split-string (buffer-substring-no-properties start end) "\n"))
         (lincnt (length lines))
         (maxlen (apply (function max)
                        (mapcar (function length) lines)))
         (array  (make-array (list  lincnt maxlen))) )
    (do* ((lines lines (cdr lines))
          (line (car lines) (car lines))
          (y 0 (1+ y)))
        ((>= y lincnt))
      (do ((x 0 (1+ x)))
          ((>= x maxlen))
        (setf (aref (aref array y) x)
              (if (< x (length line)) (aref line x) (character " ")))))
    array))


(defun transpose (matrix)
  (let* ((n (array-dimension matrix 0))
         (p (array-dimension matrix 1))
         (transposed (make-array (list p n))) )
    (do ((x 0 (1+ x)))
        ((>= x p))
      (do ((y 0 (1+ y)))
          ((>= y n))
        (setf (aref (aref transposed x) y) (aref (aref matrix y) x))))
    transposed))


(defun rotate (matrix)
  (let* ((n (array-dimension matrix 0))
         (p (array-dimension matrix 1))
         (rotated (make-array (list p n))) )
    (do ((x 0 (1+ x)))
        ((>= x p))
      (do ((y 0 (1+ y)))
          ((>= y n))
        (setf (aref (aref rotated (- p x 1)) y) (aref (aref matrix y) x))))
    rotated))


(defun array-to-string (array)
  (unsplit-string (mapcar (function concat) array) "\n"))


(defun rotate-ccw-region (start end)
  (interactive "r")
  (let ((replacement (array-to-string (rotate (region-to-array start end)))))
    (delete-region start end)
    (goto-char start)
    (set-mark-command nil)
    (insert replacement)))


;;;; THE END ;;;;
