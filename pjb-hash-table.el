;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-hash-table.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements hash table functions.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-01-25 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2017 - 2017
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

(defun* hashtable (&key (test (function eql))
                        (size nil sizep)
                        (rehash-size nil rehash-size-p)
                        (rehash-threshold nil rehash-threshold-p)
                        elements)
  "Creates a new hash-table, filled with the given ELEMENTS.
ELEMENTS must be a list of lists of two items, the key and the value.
Note: we use the name HASHTABLE to avoid name collision."
  (let ((table (apply (function make-hash-table)
                      :test test
                      (append (when sizep
                                (list :size size))
                              (when rehash-size-p
                                (list :rehash-size rehash-size))
                              (when rehash-threshold-p
                                (list :rehash-threshold rehash-threshold))))))
    (dolist (item elements table)
      (setf (gethash (first item) table) (second item)))))


(provide 'pjb-hash-tabl)
