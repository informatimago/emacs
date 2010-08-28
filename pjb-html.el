;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-html.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Skip over <tag>...</tag>.
;;;;
;;;;    We need also up-tag and down-tag, etc.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pbourguignon@ravenpack.com>
;;;;MODIFICATIONS
;;;;    2007-04-04 <PJB> Created.
;;;;BUGS
;;;;
;;;;    Doesn't work at beginning or end of buffer for tests are not implemented.
;;;;
;;;;    Doesn't like non-balanced tags, unfortunately some DTD prevent some
;;;;    tags to be closed AFAIK.  This is not handled.
;;;;
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2007 - 2007
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
;;;;**************************************************************************


(defun beginning-of-buffer-p (point)
  ;; (message "Please implement beginning-of-buffer-p again...")
  nil)

(defun end-of-buffer-p (point)
  ;; (message "Please implement end-of-buffer-p again...")
  nil)


(defun skip-to-next-tag ()
  (interactive)
  (loop
     for end   = (progn (forward-sexp  1) (point))
     for start = (prog1 (progn (forward-sexp -1) (point)) (forward-sexp 1))
     until (or (end-of-buffer-p end)
               (string-match "</?[A-Za-z]+[ >]" (buffer-substring start end)))
     finally (return (buffer-substring start end))))


(defun skip-to-previous-tag ()
  (interactive)
  (loop
     for start = (progn (forward-sexp  -1) (point))
     for end   = (prog1 (progn (forward-sexp 1) (point)) (forward-sexp -1))
       do (message "%S" (list start end))
     until (or (beginning-of-buffer-p end)
               (string-match "</?[A-Za-z]+[ >]" (buffer-substring start end)))
     finally (return (buffer-substring start end))))




(defun open-tag-p (tag-string)
  (string-match "\\`<[^/]" tag-string))


(defvar *auto-close-tags*
  '("link" "img" "input" "br" "hr"))

(defun close-tag-p (tag-string)
  (or (string-match "\\(\\`</\\|/ *>\\'\\)" tag-string)
      (member* (tag-name tag-string) *auto-close-tags*
               :test (function string-equal))))

(defun tag-name (tag-string)
  (when (string-match "^</?\\([A-Za-z]+\\)" tag-string)
    (match-string 1 tag-string)))


(defun move-to-tag (n skip-for skip-back openp closep end)
  (dotimes (i n)
    (let* ((open (funcall skip-for))
           (open-name (tag-name open)))
      (cond
        ((funcall openp open)
         (if (funcall closep open)
           (message "open and close %s" open)
           ;; parse until matching close-tag-p that is not open-tag-p
           (loop
              for close = (funcall skip-for)
              initially (message "open tag: %s" open)
              do (cond
                   ((funcall openp close)
                    (funcall skip-back)
                    (move-to-tag 1 skip-for skip-back openp closep end))
                   ;; If not openp, it must be closep.
                   ((string-equal open-name (tag-name close))
                    (message "close tag: %s" close)
                    (return close))
                   (t
                    (error "At %s of body of tag %s"  end open-name))))))
        ((funcall closep open)
         (error "At %s of body of tag %s"  end open-name))))))


(defun forward-tag (&optional n)
  (interactive "p")
  (setf n (or n 1))
  (if (< n 0)
    (backward-tag (- n))
    (move-to-tag n
                 (function skip-to-next-tag)
                 (function skip-to-previous-tag)
                 (function open-tag-p)
                 (function close-tag-p)
                 "end")))


(defun backward-tag (&optional n)
  (interactive "p")
  (setf n (or n 1))
  (if (< n 0)
    (forward-tag (- n))
    (move-to-tag n
                 (function skip-to-previous-tag)
                 (function skip-to-next-tag)
                 (function close-tag-p)
                 (function open-tag-p)
                 "beginning")))



(defun html-meat ()
  (interactive)
  (local-set-key "\C-c."    'forward-tag)
  (local-set-key "\C-c,"    'backward-tag))

