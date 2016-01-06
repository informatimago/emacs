;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-milliways.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    schedule functions to run at the end of ~/.emacs
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-08-20 <PJB> Extracted from ~/rc/emacs-common.el
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
(require 'pjb-cl)

(defparameter *milliways* '())


(defun milliways-run ()
  (interactive)
  (while *milliways*
    (ignore-errors (funcall (pop *milliways*)))))

(defun milliways-activate (&optional delay)
  "Called at the end of ~/.emacs"
  (run-at-time (or delay 5)
               1
               'milliways-run))

(defun milliways-schedule (function)
  "Schedule the function to be called after emacs started."
  (push function *milliways*))

(milliways-schedule
 (lambda ()
   ;; (speak   "Welcome to the Restaurant at the End of the Universe!")
   (message "Welcome to the Restaurant at the End of the Universe!")))



;;;; THE END ;;;;
