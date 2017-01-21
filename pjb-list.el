;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:              pjb-list.lisp
;;;;LANGUAGE:          common-lisp
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;    This module exports some list functions.
;;;;USAGE
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2017-01-21 <pjb> Some cleanup, prunned deprecated functions.
;;;;    2011-06-01 <PJB> Added clean-alist, clean-plist, alist->plist and plist->alist.
;;;;    2006-03-23 <PJB> Added maptree.
;;;;    2002-12-03 <PJB> Common-Lisp'ized.
;;;;    2001-11-30 <PJB> Added list-remove-elements.
;;;;    199?-??-?? <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    Copyright Pascal J. Bourguignon 2002 - 2017
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU  General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This script is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this library; see the file COPYING.LIB.
;;;;    If not, write to the Free Software Foundation,
;;;;    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;******************************************************************************
(require 'pjb-cl)

(defun ensure-list (x) (if (listp x) x (list x)))

(defun iota (count &optional start step)
  "
RETURN:   A list containing the elements
          (start start+step ... start+(count-1)*step)
          The start and step parameters default to 0 and 1, respectively.
          This procedure takes its name from the APL primitive.
EXAMPLES: (iota 5) => (0 1 2 3 4)
          (iota 5 0 -0.1) => (0 -0.1 -0.2 -0.3 -0.4)
"
  (setf start (or start 0) step (or step 1))
  (when (< 0 count)
    (do ((result '())
         (item (+ start (* step (1- count))) (- item step)))
        ((< item start) result)
      (push item result))))

(defun flatten (tree)
  "
RETURN: A tree containing all the elements of the `tree'.
"
  (do ((result nil)
       (stack  nil))
      ((not (or tree stack)) (nreverse result))
    (cond
      ((null tree)
       (setq tree (pop stack)))
      ((atom tree)
       (push tree result)
       (setq tree (pop stack)))
      ((listp (car tree))
       (push (cdr tree) stack)
       (setq tree (car tree)))
      (t
       (push (car tree) result)
       (setq tree (cdr tree))))))

(defun maptree (function tree)
  "Call FUNCTION on all atoms in the cons tree TREE, building a similar
tree in the process which is returned."
  (cond ((null tree) tree)
        ((atom tree) (funcall function tree))
        (t (cons (maptree function (car tree))
                 (maptree function (cdr tree))))))

(defun depth (tree)
  "
RETURN:     The depth of the tree.
"
  (if (atom tree)
      0
      (1+ (apply (function max)
                 0
                 (do ((tree tree (cdr tree))
                      (results '()))
                     ((atom tree) results)
                   (if (listp (car tree)) (push (depth (car tree)) results)))))))

(defun deepest-rec (tree)
  "
RETURN:     The deepest list in the tree.
NOTE:       Recursive algorithm.
SEE-ALSO:   deepest-iti
"
  (let ((subtree (delete-if (function atom) tree)))
    (cond
      ((null subtree)    tree)
      ((every (lambda (item) (every (function atom) item)) subtree)
       (car subtree))
      (t
       (deepest (apply 'concatenate 'list subtree))))))

(defun deepest-iti (tree)
  "
RETURN:     The deepest list in the tree.
NOTE:       Iterative algorithm.
SEE-ALSO:   deepest-rec
"
  (do* ((tree tree (apply 'concatenate 'list subtree))
        (subtree (delete-if (function atom) tree)
                 (delete-if (function atom) tree)))
       ((or (null subtree)
            (every (lambda (item) (every (function atom) item)) subtree))
        (if (null subtree) tree (car subtree)))))

(defalias 'deepest 'deepest-iti)

(defun nsplit-list-on-indicator (list indicator)
  "
RETURN: a list of sublists of list (the conses from list are reused),
        the list is splited between items a and b for which (indicator a b).
"
  (let* ((result nil)
         (sublist list)
         (current list)
         (next    (cdr current)))
    (loop while next do
         (if (funcall indicator (car current) (car next))
             (progn ;; split
               (setf (cdr current) nil)
               (push sublist result)
               (setq current next)
               (setq next (cdr current))
               (setq sublist current)
               )
             (progn ;; keep
               (setq current next)
               (setq next (cdr current))
               )))
    (push sublist result)
    (nreverse result)))

(defun list-insert-separator (list separator)
  "
RETURN:  A list composed of all the elements in `list'
         with `separator' in-between.
EXAMPLE: (list-insert-separator '(a b (d e f)  c) 'x)
         ==> (a x b x (d e f) x c)
"
  (do* ((result (if list (list (car list)) nil))
        (rest (cdr list) (cdr rest)) )
       ((null rest) (nreverse result))
    (push separator result)
    (push (car rest) result)))

(defun make-list-of-random-numbers (length)
  "
RETURN:  A list of length `length' filled with random numbers.
"
  (loop while (< 0 length)
     collect (random most-positive-fixnum) into result
     do (setq length (1- length))
     finally return result))

(defun assoc-setq (key value alist)
  "
RETURN:  A new alist with the assoc in alist with the KEY
         replaced by the new assoc (KEY . VALUE).
"
  (let ((old (assoc key alist))
        (new (cons key value)))
    (if old (list-replace-member alist old new)
        (cons new alist))))

(defun assoc-val (key alist)
  "
RETURN: (cdr (assoc key alist)).
"
  (cdr (assoc key alist)))

(defun list-remove-elements (list elements)
  "
DO:      Remove a list of elements from the list.
RETURN:  A new list with the elements removed.
POST:    (not (apply 'or
                (let ((list-wo-elements (remove-elements elements list)))
                 (mapcar (lambda (e) (member e list-wo-elements)) elements))))
NOTE:    The order of the elements is not conserved.
EXAMPLE: (list-remove-elements '(a b c d e f) '(a c e))
         ==> (b d f)
"
  (remove-if (lambda (x) (member x elements)) list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Double-Linked Lists

(defun list-to-double-linked-list (single)
  "
RETURN:  A double-linked-list.
NOTE:    Use dll-node, dll-next and dll-previous to walk the double-linked-list.
EXAMPLE: (setq d (list-to-double-linked-list '( a b c)))
         ==> (a nil b #0 c (b #0 c #1))
         (dll-node d)
         ==> a
         (dll-next d)
         ==> (b (a nil b #1 c #0) c #0)
         (dll-previous (dll-next d))
         ==> (a nil b #0 c (b #0 c #1))
"
  (loop with head = nil
     for previous = nil then current
     for element in single
     for current = (list element previous)
     unless head do (setq head current)
     when previous do (setf (cdr (cdr previous))  current)
     finally return head))

(defun dll-node     (dll-cons)
  "The node in the `dll-cons' double-linked-list node."
  (car  dll-cons))

(defun dll-previous (dll-cons)
  "The previous dll-cons in the `dll-cons' double-linked-list node."
  (cadr dll-cons))

(defun dll-next     (dll-cons)
  "The next dll-cons in the `dll-cons' double-linked-list node."
  (cddr dll-cons))

(defun equiv (&rest args)
  (if (null args)
      t
      (let ((val (not (pop args))))
        (while (and args (eq val (not (car args)))) (pop args))
        (not args))))

(defun equivalence-classes (set equalf)
  (loop with classes = (quote ())
     for item in set
     for class = (car (member* item classes
                               :test equalf  :key (function second)))
     do (if class
            (push item (cdr class))
            (push (list :class item) classes))
     finally (return (mapcar (function cdr) classes))))

(defun clean-alist (a-list)
  "A new a-list with the shadowed associations removed."
  (mapcon (lambda (a-list)
             (if (member* (caar a-list) (cdr a-list) :key (function car) :test (function equal))
                 '()
                 (list (car a-list))))
          (reverse a-list)))

(defun alist->plist (alist)
  "Converts an a-list into a p-list.
Warning: the keys in p-list should be only symbols. p-lists getf and get use eq."
  (loop for (k . v) in alist collect k collect v))

(defun plist->alist (plist)
  "Converts an p-list into a a-list."
  (loop for (k v) on plist by (function cddr) collect (cons k v)))

(defun clean-plist (plist)
  "A new p-list with the shadowed associations removed."
  (alist->plist (clean-alist (plist->alist plist))))


(provide 'pjb-list)
;;;; THE END ;;;;
