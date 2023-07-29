;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               pjb-computer-paper.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    XXX
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <spam@thalassa.informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-01-31 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2004 - 2011
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
;; (require 'make-overlay)


(defconst +computer-paper-colors+  '("azure" "PaleTurquoise" "LightCyan1"
                                     "LightBlue"  "LightCyan" "PowderBlue"))


(defun delete-all-overlays (&rest arguments)
  (interactive)
  (mapc (lambda (item)
          (if (listp item)
              (mapc (function delete-overlay) item)
              (delete-overlay item)))
        (overlay-lists)))


(defun put-computer-paper-overlay (modulo block)
  (delete-all-overlays)
  (goto-char (point-min))
  (let ((backf (make-array
                (list (length +computer-paper-colors+))
                :initial-contents
                (mapcar
                 (lambda (color)
                   (let* ((facesym (intern
                                    (concatenate 'string color "-face")))
                          (face (make-face facesym)))
                     (copy-face 'default face)
                     (set-face-foreground face "black")
                     (set-face-background face color)
                     face))
                 +computer-paper-colors+)))
        (line 0))
    (while (< (point) (point-max))
      (let ((extent (make-overlay (progn (beginning-of-line) (point))
                                  (progn (forward-line block)
                                         (beginning-of-line) (point)))))
        (overlay-put extent 'evaporate t)
        (overlay-put extent 'face (aref backf (mod line modulo)))
        (incf line) ))))


(defun computer-paper ()
  (interactive)
  (let (modulo block)
    (cond
     ((integerp current-prefix-arg)
      (setf modulo (min current-prefix-arg (length +computer-paper-colors+))
            block  1))
     ((null     current-prefix-arg)
      (setf modulo (min 2 (length +computer-paper-colors+))
            block  1))
     ((consp    current-prefix-arg)
      (setf modulo (min (read-minibuffer "Modulo: " "2")
                        (length +computer-paper-colors+))
            block  (read-minibuffer "Block: " "1")))
     (t (error "Invalid prefix %S" current-prefix-arg)))
    (assert (<= 1 block))
    (assert (and (<= 2 modulo) (<= modulo (length +computer-paper-colors+))))
    (put-computer-paper-overlay modulo block)))

(defun computer-paper--adjust-right-margin (width)
  (interactive "P")
  (save-excursion
   (let ((margin (if (integerp width)
                     width
                     80)))
     (goto-char (point-min))
     (while (re-search-forward "^.*$" nil t)
       (let ((width (- (match-end 0) (match-beginning 0))))
         (when (< width margin)
           (end-of-line)
           (insert (make-string (- margin width) 32))))))))

;;;; THE END ;;;;
