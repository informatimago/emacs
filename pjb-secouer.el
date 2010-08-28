;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               secouer.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Exports a function "secouer" that will randomize the order of 
;;;;    internal letters of words in the selected region.
;;;;    
;;;;    Eroxtps a funtcion "scuoeer" that wlil raoindmze the oedrr of 
;;;;    intnaerl letetrs of wrods in the steceeld rgioen.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-12-04 <PJB> Added this comment.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2003 - 2003
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


(defun melange (str)
  (setq str (copy-seq str))
  (do ((count (+ 3 (random (length str))) (1- count))
       (i (random (length str))  (random (length str)))
       (j (random (length str))  (random (length str))))
      ((< count 0) str)
    (when (/= i j)
      (psetf (char str i) (char str j)
             (char str j) (char str i)))));;melange


(defun secouer (start end)
  (interactive "*r")
  (let ((start (min end start))
        (end (max end start)))
    (goto-char start)
    (while (< (point) end)
      (let* ((start-word (progn (forward-word 1) (forward-word -1) (point)))
             (end-word   (progn (forward-word 1) (point))))
        (when (and (<= end-word end) (<= 4 (- end-word start-word)))
          (let  ((word (buffer-substring (1+ start-word)  (- end-word 1))))
            (message word)
            (delete-region (1+ start-word) (- end-word 1))
            (goto-char (1+ start-word))
            (insert (melange word))
            (goto-char end-word)))))));;secouer

;;;; pjb-secouer.el                   -- 2004-02-22 02:27:40 -- pascal   ;;;;
