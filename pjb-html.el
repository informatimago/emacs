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
;;;;    Copyright Pascal Bourguignon 2007 - 2011
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

(require 'cl)
(require 'pjb-cl)



(defun pjb-parse-xml (xml)
  "Parse the XML string."
  (with-temp-buffer
    (insert xml)
    (xml-parse-region (point-min) (point-max))))

(defun pjb-parse-html (html)
  "Parse the HTML string."
  (pjb-parse-xml html))


(defun pjb-find-html-tag (tag html)
  (cond
    ((atom html) nil)
    ((eq tag (car html)) html)
    (t (or (pjb-find-html-tag tag (car html))
           (pjb-find-html-tag tag (cdr html))))))






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






(defun make-element (name attributes children) (list* name attributes children))
(defun element-name (element) (and (listp element) (car element)))
(defun element-attributes (element) (and (listp element) (cadr element)))
(defun element-children (element) (and (listp element) (cddr element)))
(defun set-element-name (element new-name) (setf (car element) new-name))
(defun set-element-attributes (element new-attributes) (setf (cadr element) new-attributes))
(defun set-element-children (new-element children) (setf (cddr element) new-children))
(defsetf element-name set-element-name)
(defsetf element-attributes set-element-attributes)
(defsetf element-children set-element-children)


(defun make-attribute (name value) (list* name value))
(defun attribute-name (attribute) (car attribute))
(defun attribute-value (attribute) (cdr attribute))
(defun set-attribute-name (attribute new-name) (setf (car attribute) new-name))
(defun set-attribute-value (new-attribute value) (setf (cdr attribute) new-value))
(defsetf attribute-name set-attribute-name)
(defsetf attribute-value set-attribute-value)


(defun entity-name-equal-p (a b)
  "xmls entity name may go in namespaces in which case they're lists: (name namespace)"
  (cond
   ((and (stringp a) (stringp b)) (string= a b))
   ((and (stringp a) (symbolp b)) (string= a b))
   ((and (symbolp a) (stringp b)) (string= a b))
   ((and (symbolp a) (symbolp b)) (string= a b))
   ((and (consp a)   (consp b))   (entity-name-equal-p (car a) (car b)))
   ((and (consp a)   (stringp b)) (entity-name-equal-p (car a) b))
   ((and (consp a)   (symbolp b)) (entity-name-equal-p (car a) b))
   ((and (stringp a) (consp b))   (entity-name-equal-p a (car b)))
   ((and (symbolp a) (consp b))   (entity-name-equal-p a (car b)))))


(defun get-attribute-named (element attribute-name)
  (find attribute-name (element-attributes element)
        :test (function string=)
        :key (function attribute-name)))

(defun value-of-attribute-named (element attribute-name)
  (attribute-value (get-attribute-named element attribute-name)))

(defun get-first-child (element)
  (first (element-children element)))

(defun single-string-child-p (element)
  (and (= 1 (length (element-children element)))
       (stringp (get-first-child element))))


(defun get-first-child-tagged (element element-name)
  (find element-name
        (element-children element)
        :test (function entity-name-equal-p)
        :key (function element-name)))

(defun get-first-child-valued (element attribute value)
  (find-if
   (lambda (child) (string= value (value-of-attribute-named child attribute)))
   (element-children element)))

(defun get-children-tagged (element element-name)
  (remove* element-name
          (element-children element)
          :test-not (function entity-name-equal-p)
          :key (lambda (x) (if (consp x) (element-name x) ""))))


(defun get-children-with-tag-and-attribute (element element-name attribute-name attribute-value)
  (remove-if-not (lambda (child)
                   (and (consp child)
                        (entity-name-equal-p (element-name child) element-name)
                        (string= (value-of-attribute-named child attribute-name) attribute-value)))
                 (element-children element)))


(defun find-children-tagged (element element-name)
  (append (get-children-tagged element element-name)
          (mapcan (lambda (child) (find-children-tagged child element-name))
                  (element-children element))))


(defun value-to-boolean (value)
  (string= "true" value))


(defun element-at-path (root path)
  (if (null path)
      root
      (element-at-path (get-first-child-tagged root (first path)) (rest path))))


(provide 'pjb-html)
;;;; THE END ;;;;
