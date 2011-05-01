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
;;;;    2006-03-23 <PJB> Added maptree.
;;;;    199?-??-?? <PJB> Creation.
;;;;    2001-11-30 <PJB> Added list-remove-elements.
;;;;    2002-12-03 <PJB> Common-Lisp'ized.
;;;;BUGS
;;;;LEGAL
;;;;    Copyright Pascal J. Bourguignon 2002 - 2011
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
(provide 'pjb-list)

;; list-to-set list-to-set-sorted  ==> Use REMOVE-DUPLICATES

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
       (setq tree (cdr tree)))))
  ) ;;flatten

;;; (flatten '((((a) (b c) d) e) (f g) (h (i j (k) l) (()) (()()()))))


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
                   (if (listp (car tree)) (push (depth (car tree)) results))))))
  ) ;;depth


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
       (deepest (apply 'concatenate 'list subtree)))))
  ) ;;deepest-rec


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
        (if (null subtree) tree (car subtree))))
  ) ;;deepest-iti

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
               ))
         ) ;;while
    (push sublist result)
    (nreverse result))
  ) ;;nsplit-list-on-indicator


(defun list-extract-predicate (liste predicate)
  "
RETURN:  A new list containing all the member of LISTE for which PREDICATE
         returned non-nil. The elements are in the same order in the result
         than they were in LISTE.
EXAMPLE: (list-extract-predicate '( 1 2 3 4 5 6 ) 'oddp)
         ==> (1 3 5)
"
  (message "Use CL remove-if-not instead of list-extract-predicate !")
  (remove-if-not predicate liste)
  ;;   (loop for item in liste
  ;;         when (funcall predicate item)
  ;;         collect item into result
  ;;         finally return result)
  ) ;;list-extract-predicate


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
    (push (car rest) result))
  ) ;;list-insert-separator


(defun list-replace-member (list old new)
  "
RETURN:  A new list copy of `list' where the _first_ occurence of `old' (eq)
         is replaced with `new'.
EXAMPLE: (list-replace-member '(a b c a b c) 'b 'x)
         ==> (a x c a b c)
"
  (message "Use CL substitute instead of list-replace-member !")
  (substitute new old list)) ;;list-replace-member


(defun list-replace-member-in-place (list old new)
  "
DO:      Modifies the list, replacing the _first_ occurence of `old' (equal)
         with `new'.
RETURN:  `list'
EXAMPLE: (list-replace-member-in-place (list (concatenate 'string \"a\" \"b\")
                                                \"c\" \"ab\") \"ab\" \"x\")
         ==> (\"x\" \"c\" \"ab\")
"
  (message "Use CL nsubstitute instead of list-replace-member-in-place !")
  (nsubstitute new old list)) ;;list-replace-member-in-place
  

(defun list-replace-member-in-place-eq (list old new)
  "
DO:      Modifies the list, replacing the _first_ occurence of `old' (eq)
         with `new'.
RETURN:  `list'
EXAMPLE: (list-replace-member-in-place-eq (list \"ab\" (setq s \"ab\") \"c\" s)
                                           s \"x\")
         ==> (\"ab\" \"x\" \"c\" \"ab\")
"
  (message "Use CL nsubstitute instead of list-replace-member-in-place-eq !")
  (nsubstitute new old list :test (function eq))) ;;list-replace-member-in-place-eq
         


(defun make-list-of-random-numbers (length)
  "
RETURN:  A list of length `length' filled with random numbers.
"
  (loop while (< 0 length)
     collect (random most-positive-fixnum) into result
     do (setq length (1- length))
     finally return result)
  ) ;;make-list-of-random-numbers



(defun assoc-setq (key value alist)
  "
RETURN:  A new alist with the assoc in alist with the KEY
         replaced by the new assoc (KEY . VALUE).
"
  (let ((old (assoc key alist))
        (new (cons key value)))
    (if old (list-replace-member alist old new)
        (cons new alist)))
  ) ;;assoc-setq


(defun assoc-val (key alist)
  "
RETURN: (cdr (assoc key alist)).
"
  (cdr (assoc key alist))
  ) ;;assoc-val




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
  (remove-if (lambda (x) (member x elements)) list)
  ;;   (loop for item in list
  ;;         unless (member item elements)
  ;;         collect item into result
  ;;         finally return result)
  ) ;;list-remove-elements



(defun remove-nil (list)
  "
DO:      Remove all nil items in the `list'.
RETURN:  A new list with the same elements as `list' but the nils.
NOTE:    This is not a 'deep' function.
EXAMPLE: (remove-nil '( a nil b ( c nil d ) e nil))
         ==> (a b (c nil d) e)
"
  (message "Use CL (remove nil list) instead of (remove-nil list) !")
  (remove nil list)
  ;;   (loop for item in list
  ;;         unless (null item)
  ;;         collect item into result
  ;;         finally return result)
  ) ;;remove-nil


(defun nremove-nil (list)
  "
DO:      Remove all nil items in-place in the `list'.
RETURN:  The modified `list'.
WARNING: As usual for these class of functions, the result may be a cons
         different to the old `list' when the firsts conses are removed;
         use: (setq list (nremove-nil list))
EXAMPLE: (nremove-nil (setq s (copy-seq '( a nil b ( c nil d ) nil e nil))))
         ==> (a b (c nil d) e)
         s
         ==> (a b (c nil d) e)
         (nremove-nil (setq s (copy-seq '(nil nil b ( c nil d ) e nil))))
         ==> (b (c nil d) e)
         s
         ==> (nil nil b (c nil d) e)
"
  (message "Use CL (delete nil list) instead of (nremove-nil list) !")
  (delete nil list)
  ;;   (unless (atom list)
  ;;     ;; skip to first non-nil car.
  ;;     (setq list (loop for first = list then (cdr first)
  ;;                      while (and first (null (car first)))
  ;;                      finally return first))
  ;;     ;; since (not (null (car list))),
  ;;     ;; we can take prev == list without problem.
  ;;     (loop with prev = list
  ;;           with rest = (cdr prev)
  ;;           while rest
  ;;           do (loop while (and rest (null (car rest)))
  ;;                    do (setq rest (cdr rest)))
  ;;           (setf (cdr prev) rest)
  ;;           (setq prev rest
  ;;                 rest (cdr rest)))
  ;;     ) ;;unless
  ;;   list
  ) ;;nremove-nil



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
     finally return head)
  ) ;;list-to-double-linked-list

;; (setq x (list-to-double-linked-list '(a b c d e f)))
;; (list (dll-node x) '/ (dll-previous x) '/ (dll-next x))
;; (setq x (dll-next x))
;; --> #1=(a nil . #2=(b #1# . #3=(c #2# . #4=(d #3# . #5=(e #4# f #5#)))))


(defun dll-node     (dll-cons)
  "
RETURN:  The node in the `dll-cons' double-linked-list node.
"
  (car  dll-cons)
  ) ;;dll-node


(defun dll-previous (dll-cons)
  "
RETURN:  The previous dll-cons in the `dll-cons' double-linked-list node.
"
  (cadr dll-cons)
  ) ;;dll-previous


(defun dll-next     (dll-cons)
  "
RETURN:  The next dll-cons in the `dll-cons' double-linked-list node.
"
  (cddr dll-cons)
  ) ;;dll-next


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;; Sets


;;; SEE: delete-duplicate remove-duplicate
;;; (defun list-to-set (list)
;;;   "
;;; RETURN: a set, that is a list where duplicate elements from list are removed.
;;; NOTE:   The complexity of this implementation is O(N^2) [N==(length list)],
;;; "
;;;   (loop with set = nil
;;;         while list
;;;         do
;;;         (if (not (member (car list) set))
;;;             (setq set (cons (car list) set)))
;;;         (setq list (cdr list))
;;;         finally return set)
;;;   ) ;;list-to-set



;;; (defun cons-lessp (a b)
;;;   "PRIVATE.
;;; RETURN: a<=b
;;; "
;;;   (do* ( (ap a (cdr ap))
;;;          (ai (car ap) (car ap))
;;;          (bp b (cdr bp)) 
;;;          (bi (car bp) (car bp)) )
;;;       ( (not (and ai bi (eq ai bi)))
;;;         (any-lessp ai bi) )
;;;     )
;;;   ) ;;cons-lessp




;;; (defun formated-lessp (a b)
;;;   "PRIVATE.
;;; RETURN: a<=b
;;; "
;;;   (string-lessp (format nil "~S" a) (format nil "~S" b))
;;;   );;formated-lessp


;;; (defun symbol-lessp (a b)
;;;   "PRIVATE.
;;; RETURN: a<=b
;;; "
;;;   (string-lessp (symbol-name a) (symbol-name b))
;;;   );;symbol-lessp


;;; (defun vector-lessp (a b)
;;;   "PRIVATE.
;;; RETURN: a<=b
;;; "
;;;   (if (= (length a) (length b))
;;;       (loop for i from 0 below (length a)
;;;             for ai = (aref a i)
;;;             for bi = (aref b i)
;;;             while (eq ai bi)
;;;             ;;do (show ai bi)
;;;             ;;finally (show ai bi) (show (or bi (not ai)))
;;;             finally return (any-lessp ai bi))
;;;     (< (length a) (length b)))
;;;   );;vector-lessp


;;; (defun any-lessp (a b)
;;;   "PRIVATE.
;;; RETURN: a<=b
;;; "
;;;   (if (eq (type-of a) (type-of b))
;;;       (funcall
;;;        (cdr (assoc
;;;              (type-of a)
;;;              '((bool-vector . vector-lessp)
;;;                (buffer . formated-lessp)
;;;                (char-table . vector-lessp)
;;;                (compiled-function . vector-lessp)
;;;                (cons . cons-lessp)
;;;                (float . <=)
;;;                (frame . formated-lessp)
;;;                (integer . <=)
;;;                (marker . <=)
;;;                (overlay . formated-lessp)
;;;                (process . formated-lessp)
;;;                (string . string-lessp)
;;;                (subr . formated-lessp)
;;;                (symbol . symbol-lessp)
;;;                (vector . vector-lessp)
;;;                (window . formated-lessp)
;;;                (window-configuration . formated-lessp)
;;;                ))) a b)
;;;     (string-lessp (symbol-name (type-of a))
;;;                   (symbol-name (type-of b))))
;;;   );;any-lessp


;;; (defun list-to-set-sorted (list)
;;;   "
;;; RETURN: A set, that is a list where duplicate elements from `list' are removed.
;;; NOTE:   This implementation first sorts the list, so its complexity should be
;;;         of the order of O(N*(1+log(N))) [N==(length list)]
;;;         BUT, it's still slower than list-to-set 
;;; "
;;;   (if (null list)
;;;       nil
;;;     (let* ((sorted-list (sort list 'any-lessp))
;;;            (first (car sorted-list))
;;;            (rest  (cdr sorted-list))
;;;            (set nil))
;;;       (loop while rest do
;;;         (if (eq first (car rest))
;;;             (setq rest (cdr rest))
;;;           (progn
;;;             (push first set)
;;;             (setq first (car rest)
;;;                   rest  (cdr rest)))))
;;;       set)));;list-to-set-sorted


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



;;;; pjb-list.el                      --                     --          ;;;;
