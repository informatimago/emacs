;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-java.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Commands to help editing java code.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal. Bourguignon <pascal.bourguignon@ubudu.com>
;;;;MODIFICATIONS
;;;;    2013-06-14 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal. Bourguignon 2013 - 2013
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


(defun pjb-java--single-line (start end)
  (interactive "*r")
  (with-marker (end (if (= ?\n (char-before end)) (1- end) end))
    (goto-char start)
    (while (and (< (point) end) (re-search-forward "\n[ \t]*" end t))
      (delete-region (match-beginning 0) (match-end 0))
      (when (and (alphanumericp (char-before))
                 (alphanumericp (char-after)))
        (insert " ")))))

(provide 'pjb-java)
