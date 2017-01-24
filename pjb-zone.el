;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-zone.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Format DNS zone files.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-20 <PJB> Created.
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
(require 'rx)


(defparameter re/domain-name '(seq (char "-a-zA-Z0-9") (* (char "-a-zA-Z0-9."))))
(defparameter re/spaces      '(+ (char " ")))
(defparameter re/ttl         '(+ digit))
(defparameter re/rest        '(* nonl))

(defparameter re/zone-line
  '(seq
    bol
    (group (or (eval re/domain-name) (char "@")))
    (eval re/spaces)
    (opt (group (eval re/ttl)) (eval re/spaces))
    "IN"  (eval re/spaces)
    (group (or "NS" "A" "CNAME" "MX" "TXT" "PTR")) (eval re/spaces)
    (group (eval re/rest))
    eol))

(defparameter *zone-line-regexp* (rx (eval re/zone-line)))
(defparameter *zone-line-format* "%-30s %5d  IN  %-6s %s")

(defun zone-reformat-buffer ()
  (interactive)
  (zone-reformat-region (point-min) (point-max)))

(defun zone-reformat-region (start end)
  (interactive "r")
  (goto-char start)
  (let ((end (let ((m (make-marker)))
               (set-marker m end)
               m)))
    (while (re-search-forward *zone-line-regexp* end t)
      (let ((dn   (match-string 1))
            (ttl  (match-string 2))
            (type (match-string 3))
            (rest (match-string 4)))
        (delete-region (match-beginning 0) (match-end 0))
        (insert (format *zone-line-format* dn (if (or (null ttl)
                                                      (string= "" ttl))
                                                  3600
                                                  (nth-value 0 (cl:parse-integer ttl))) type rest))))))





(provide 'pjb-zone)
;;;; THE END ;;;;
