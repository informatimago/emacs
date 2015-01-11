;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-speak.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Call up the speak script to say something.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-01-10 <PJB> Extracted from ~/rc/emacs-common.el
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
(require 'cl)


(defvar *tempdir*  (format "/tmp/emacs%d" (user-uid)))
(defvar *pjb-speak-file-counter* 0)

(defun pjb-speak-file ()
  (format "%s/speak-%d.txt" *tempdir* (incf *pjb-speak-file-counter*)))


(defvar *pjb-speak-last-message* nil)

(defun* speak (message &key voice language)
  (interactive "sMessage: ")
  (let ((file (pjb-speak-file)))
    (with-current-buffer (get-buffer-create " *speak text*")
      (erase-buffer)
      (insert message)
      (setf *pjb-speak-last-message* message)
      (write-region (point-min) (point-max) file))
    (let ((command  (format "speak %s %s -f %s"
                            (if voice    (format "-v %s" voice)    "")
                            (if language (format "-l %s" language) "")
                            file)))
      (message "%S" command)
      (shell-command command))))

(defalias 'say 'speak)

(defun speak-repeat ()
  (interactive)
  (speak *pjb-speak-last-message*))


(provide 'pjb-speak)
;;;; THE END ;;;;;
