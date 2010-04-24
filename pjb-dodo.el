;;;;****************************************************************************
;;;;FILE:               pjb-dodo.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Trace daily start and stop times and plot them.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-09-22 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2005
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(require 'pjb-cl)
(defparameter *schedule-file* "~/.sleep-schedule")

(defun pjb-log-sleep (file tag)
  (set-buffer (get-buffer-create " *log-time*"))
  (mark-whole-buffer)
  (erase-buffer)
  (MULTIPLE-VALUE-BIND (sec min hou day mon yea dow dst zone)
      (DECODE-UNIVERSAL-TIME (GET-UNIVERSAL-TIME))
    (insert (format "(%s %d %d %d %d %d %d %d/%d)\n"
              tag yea mon day hou min sec (truncate (* 3600 zone)) 3600)))
  (write-region (point-min) (point-max) file :append))



(defun pjb-will-sleep ()
  (interactive)
  (pjb-log-sleep *schedule-file* :start))

(defun pjb-did-sleep ()
  (interactive)
  (pjb-log-sleep *schedule-file* :stop))

(defun pjb-plot-sleep ()
  (interactive)
  (shell-command "~/bin/sleep-schedule.lisp")
  (other-window 1)
  (delete-other-windows)
  (goto-char (point-max)))



