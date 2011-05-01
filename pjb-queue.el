;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:               pjb-queue.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This module exports a queue type. This is a structure optimized for
;;;;    FIFO operations, keeping a pointer to the head and the tail of a list.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon 
;;;;MODIFICATIONS
;;;;    2001-11-12 <PJB> Creation.
;;;;    2001-12-31 <PJB> Added pjb-queue-requeue. 
;;;;                     Corrected the return value of some methods.
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 1990 - 2011
;;;;
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This library is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    Lesser General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Lesser General Public
;;;;    License along with this library; if not, write to the Free Software
;;;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
;;;;
;;;;******************************************************************************
(provide 'pjb-queue)
(require 'pjb-utilities)  ;; import commented-out


(defun pjb-queue-new ()
  "DO:      Creates a new pjb-queue.
POST:    (and (pjb-queue-p (pjb-queue-new)) 
              (= 0 (pjb-queue-length queue)))
RETURN:  The new queue.
NOTE:    The structure of a pjb-queue is as follow:

            +------+------+
queue ----> | head | tail |
            +------+------+
               |      |
               V      |
       'pjb-queue  +--+
                   |
                   V
            +------+------+
            | head | tail |--------------------------+
            +------+------+                          |
               |                                     |
               V                                     V
        +------+------+    +------+------+    +------+------+
        | elem | next |--->| elem | next |--->| elem | next |--->nil
        +------+------+    +------+------+    +------+------+
           |                  |                  |
           V                  V                  V
        +------+           +------+           +------+
        | elem |           | elem |           | elem |
        +------+           +------+           +------+
"
  (cons 'pjb-queue (cons nil nil))
  );;pjb-queue-new


(defun pjb-queue-p (queue)
  "RETURN:  Whether queue is a queue.
DO:      Checks that head points to a list whose last element is tail"
  (and
   (consp queue)
   (eq 'pjb-queue (car queue))
   (let ( (head (cadr queue))
          (tail (cddr queue)) )
     (and 
      (listp head)
      (listp tail)
      (or (and (null head)             (null tail))
          (and (not (null (cdr head))) (null (cdr tail)))
          (let ( (current head) )
            (if (eq current tail)
                t ;; only one element
              (while (and (not (or (eq current head) (eq current tail))))
                (setq current (cdr current)))
              (eq current tail)))))))
  );;pjb-queue-p


(defun pjb-queue-length (queue)
  "PRE:     (pjb-queue-p queue)
RETURN:  The number of elements in the queue."
  (if (not (eq 'pjb-queue (car queue))) (error "Parameter must be a queue."))
  (length (cadr queue))
  );;pjb-queue-length


(defun pjb-queue-first-element (queue)
  "PRE:     (pjb-queue-p queue)
RETURN:  The first element of the queue."
  (if (not (eq 'pjb-queue (car queue))) (error "Parameter must be a queue."))
  (caar (cdr queue))
  );;pjb-queue-first-element


(defun pjb-queue-last-element (queue)
  "PRE:     (pjb-queue-p queue)
RETURN:  The last element of the queue."
  (if (not (eq 'pjb-queue (car queue))) (error "Parameter must be a queue."))
  (cadr (cdr queue))
  );;pjb-queue-last-element


(defun pjb-queue-enqueue  (queue element)
  "PRE:     (pjb-queue-p queue)
            l=(pjb-queue-length queue)
POST:    (eq (pjb-queue-last-element queue) element),
         (pjb-queue-p queue),
         l+1=(pjb-queue-length queue)
RETURN:  queue"
  (if (not (eq 'pjb-queue (car queue))) (error "Parameter must be a queue."))
  (let ( (q (cdr queue)) )
    ;; (car q) = head      (cdr q) = tail
    (if (car q)
        (progn
          ;; There's already an element, just add to the tail.
          (setcdr (cdr q) (cons element nil))
          (setcdr q       (cddr q)) )
      ;; The queue is empty, let's set the head.
      (setcar q (cons element nil))
      (setcdr q (car q)))
    );;let
  queue
  );;pjb-queue-enqueue


(defun pjb-queue-dequeue (queue)
  "PRE:     (pjb-queue-p queue)
            l=(pjb-queue-length queue)
            f=(pjb-queue-first-element queue)
POST:    l>0 ==> l-1=(pjb-queue-length queue)
         l=0 ==> 0=(pjb-queue-length queue)
RETURN:  f"
  (let* ( (result   (pjb-queue-first-element queue)) 
          (q        (cdr queue))
          (second   (cdar q)) )
    (if second
        ;; remains at least one element.
        (setcar q second)
      ;; removing the last element, now queue is empty.
      (setcar q nil)
      (setcdr q nil))
    result
    );;let
  );;pjb-queue-dequeue


(defun pjb-queue-requeue (queue element)
  "DO:      Insert the element at the beginning of the queue.
PRE:     (pjb-queue-p queue)
         l=(pjb-queue-length queue)
POST:    (eq (pjb-queue-first-element queue) element)
         (pjb-queue-p queue),
         l+1=(pjb-queue-length queue)
RETURN:  queue"
  (if (not (eq 'pjb-queue (car queue))) (error "Parameter must be a queue."))
  (let ( (q (cdr queue)) )
    (if (car q)
        ;; There's already an element, just insert before the head.
        (setcar q (cons element (car q)))
      ;; queue is empty, let's set the head.
      (setcar q (cons element nil))
      (setcdr q (car q)))
    );;let
  queue
  );;pjb-queue-requeue;


(defun pjb-queue-test ()
  "DO:     Test the queue data type. Insert test log at the point."
  (let (q
        (check (lambda (q)
                 (insert
                  (if (not (pjb-queue-p q))
                      (format "   NOT A QUEUE !\n%S\n" q)
                    (concat 
                     (format "   Length=%2d\n" (pjb-queue-length q))
                     (if (< 0 (pjb-queue-length q))
                         (format "      Head=%S\n      Tail=%S\n" 
                                 (pjb-queue-first-element q)
                                 (pjb-queue-last-element q))
                       "")
                     (format "   Queue=%S\n" q)
                     )))) )
        )

    (insert  "Creating a queue\n")
    (setq q (pjb-queue-new))
    (funcall check  q)

    (insert  "Dequeuing empty queue\n")
    (insert (format "%S\n" (pjb-queue-dequeue q)))
    (funcall check  q)

    (insert  "Enqueuing...\n")
    (pjb-queue-enqueue q '(first))
    (funcall check  q)

    (insert  "Enqueuing...\n")
    (pjb-queue-enqueue q '(second))
    (funcall check  q)

    (insert  "Enqueuing...\n")
    (pjb-queue-enqueue q '(third))
    (funcall check  q)

    (insert  "Enqueuing...\n")
    (pjb-queue-enqueue q '(fourth))
    (funcall check  q)

    (insert  "Requeuing...\n")
    (pjb-queue-requeue q '(zeroeth))
    (funcall check  q)

    (while (< 0 (pjb-queue-length q))
      (insert  "Dequeuing queue\n")
      (insert (format "%S\n" (pjb-queue-dequeue q)))
      (funcall check  q)
      )

    (insert  "Requeuing empty queue...\n")
    (pjb-queue-requeue q '(first))
    (funcall check  q)

    (insert  "Requeuing...\n")
    (pjb-queue-requeue q '(second))
    (funcall check  q)

    (insert  "Enqueuing...\n")
    (pjb-queue-enqueue q '(last))
    (funcall check  q)

    (while (< 0 (pjb-queue-length q))
      (insert  "Dequeuing queue\n")
      (insert (format "%S\n" (pjb-queue-dequeue q)))
      (funcall check  q)
      )

    ));;pjb-queue-test;



;;;; pjb-queue.el                     -- 2001-12-31 04:15:29 -- pascal   ;;;;


