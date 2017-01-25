;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-http.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    HTTP and stuff.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pascal.bourguignon@ubudu.com>
;;;;MODIFICATIONS
;;;;    2013-09-12 <PJB> Created.
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


(defun pjb-url-encode-string (string)
  "Return a string where all the characters but alphanumerics are converted to %xx"
  (with-output-to-string
      (loop for ch across string
         do (princ (format (if (alphanumericp ch) "%c"  "%%%02X") ch)))))

(defun pjb-url-encode-region (start end)
  (interactive "*r")
  (let ((url (buffer-substring start end)))
    (delete-region start end)
    (insert (pjb-url-encode-string url))))
;; (pjb-url-encode-string "2013/08/26-17:51:37")
;; "2013%2F08%2F26%2D17%3A51%3A37"


(defun pjb-http-get (url)
  "Fetches a resource at URL, and returns it."
  (shell-command-to-string
   (format "wget --no-convert-links -q -nv -o /dev/null -t 3  -O -  %s" (shell-quote-argument url))))

(defun pjb-parse-xml (xml)
  "Parse the XML string."
  (with-temp-buffer
    (insert xml)
    (xml-parse-region (point-min) (point-max))))

(defun pjb-parse-html (html)
  "Parse the HTML string."
  (pjb-parse-xml html))

(provide 'pjb-http)


