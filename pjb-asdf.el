;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-asdf.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    ASDF utilities.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pbourguignon@dxo.com>
;;;;MODIFICATIONS
;;;;    2023-08-03 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal Bourguignon 2023 - 2023
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

(defun goto-asdf-form ()
  (interactive)
  (goto-char (point-min))
  (when (re-search-forward "^(\\(defsystem\\|asdf:defsystem\\) ")
    (goto-char (match-beginning 0))))

(defun goto-asdf-version ()
  (interactive)
  (goto-asdf-form)
  (forward-char)
  (forward-sexp) (backward-sexp)
  (while (not (looking-at ":version"))
    (forward-sexp) (forward-sexp) (backward-sexp))
  (forward-sexp) (forward-sexp) (backward-sexp))

(defun next-date-version (version)
  "version is a list of 3 or 4 integers,
representing (YEAR MONTH DAY [INCREMENT]
If it's the current date, then the increment is added or augmented,
otherwise the current date is returned."
  (let* ((date (nreverse (subseq (decode-time (current-time)) 3 6)))
         (m    (mismatch date version)))
    (cond
      ((null m) ; same date, add a new increment
       (nconc (subseq date 0 3) (list 1)))
      ((eql m 3) ; same date, increment
       (nconc (subseq date 0 3) (list (+ (fourth version) 1))))
      (t ; different date, reset it
        date))))

(assert (equal (list (next-date-version '(2012 12 31 0))
                     (next-date-version '(2012 12 31 12))
                     (next-date-version '(2012 12 31))
                     (next-date-version '(2023 8 3))
                     (next-date-version '(2023 8 3 12)))
               '((2023 8 3)
                 (2023 8 3)
                 (2023 8 3)
                 (2023 8 3 1)
                 (2023 8 3 13))))

(defun next-usual-version (version)
  "version is a list of 1 or more integers,
we just increment the last one."
  (nconc (subseq version 0 (1- (length version)))
         (list (1+ (car (last version))))))

(assert (equal (list (next-usual-version '(1))
                     (next-usual-version '(1 2))
                     (next-usual-version '(1 2 3))
                     (next-usual-version '(1 2 3 4)))
               '((2) (1 3) (1 2 4) (1 2 3 5))))

(defun bump-asdf-version ()
  (interactive)
  (goto-asdf-version)
  (unless (looking-at "\"\\(\\([0-9]+\\.\\)*[0-9]+\\)\"")
    (error "Not a valid ASDF version"))
  (let* ((start   (match-beginning 1))
         (end     (match-end 1))
         (version (mapcar (lambda (item)
                            (car (read-from-string item)))
                          (split-string (buffer-substring start end) "\\.")))
         (new-version (cond
                          ((and (<= 3 (length version) 4)
                                (< 1990 (first version) 2100)
                                (<= 1 (second version) 12)
                                (<= 1 (third version) 31))
                           (next-date-version version))
                          (t
                           (next-usual-version version)))))
    (delete-region start end)
    (goto-char start)
    (insert (mapconcat (function prin1-to-string) new-version "."))))
