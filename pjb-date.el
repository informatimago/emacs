;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-date.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Date formats.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-10-19 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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
(require 'parse-time)

(defun parse-iso8601-date (string)
  ;; For now, we parse only "yyyymmddThhmmss"
  (encode-time (parse-integer string 13 15)
               (parse-integer string 11 13)
               (parse-integer string 9 11)
               (parse-integer string 6 8)
               (parse-integer string 4 6)
               (parse-integer string 0 4)))

(defun format-rfc822-date (universal-time)
  (format-time-string "%a, %d %b %Y %H:%M:%S %z" universal-time))

(defun iso8601-to-rfc822 (start end)
  (interactive "r")
  (let ((date (buffer-substring start end)))
    (delete-region start end)
    (insert (format-rfc822-date (parse-iso8601-date date)))))




(provide 'pjb-date)
;;;; THE END ;;;;
