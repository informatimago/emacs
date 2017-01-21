;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               window-pane.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A Sketch for split-bottom
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-07-23 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2010 - 2011
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
(require 'eieio)


(defclass pane ()
  ((left   :initform nil :initarg :left   :accessor pane-left)
   (top    :initform nil :initarg :top    :accessor pane-top)
   (width  :initform nil :initarg :width  :accessor pane-width)
   (height :initform nil :initarg :height :accessor pane-height)))

(defclass emacs-window (pane)
  ((window :initarg :window :accessor emacs-window-window)))

(defclass horizontal-splits (pane)
  ((subpanes :initarg :subpanes :accessor pane-subpanes))
  (:documentation "Subpanes are one above the other, the first topmost."))

(defclass frame-pane (horizontal-splits)
  ()
  ;; TODO: what about frames without a mini-buffer?
  (:documentation "Contains only the main pane and the mini-buffer"))

(defclass vertical-splits (pane)
  ((subpanes :initarg :subpanes :accessor pane-subpanes))
  (:documentation "Subpanes are side by side, the first leftmost."))


(defun window-tree-to-pane (tree)
  "Convert a window tree into a pane tree."
  (if (windowp tree)
      (make-instance 'emacs-window
          :width (window-width tree)
          :height (window-height tree)
          :window tree)
      (destructuring-bind (horizontalp (left top width height) &rest subpanes) tree
        (make-instance (if horizontalp 'horizontal-splits 'vertical-splits)
            :left left :top top :width width :height height
            :subpanes (mapcar (function window-tree-to-pane) subpanes)))))

(defun window-pane (&optional frame)
  (let* ((frame (or frame (selected-frame)))
         (tree  (window-tree frame)))
    ;; TODO: see what window-tree returns for frames without a mini-buffer.
    (make-instance 'frame-pane
        :width (frame-width frame) :height (frame-height frame)
        :subpanes (list (window-tree-to-pane (first tree))
                        (window-tree-to-pane (second tree))))))



(defmethod split-bottom ((self frame-pane) size)
  "SIZE is the size of the bottom frame to be created."
  (split-bottom (first (pane-subpanes self)) size))

(defmethod split-bottom ((self horizontal-splits) size)
  (eval (split-bottom-command self  size)))

(defmethod split-bottom ((self vertical-splits) size)
  (eval (split-bottom-command self  size)))

(defmethod split-bottom ((self emacs-window) size)
  (eval (split-bottom-command self  size)))



(defmethod split-bottom-command ((self horizontal-splits) size)
  (split-bottom-command (first (last (pane-subpanes self))) size))

(defmethod split-bottom-command ((self vertical-splits) size)
  (let ((deletes (delete-subpane-command self)))
    `(progn
       ,@(cddr deletes)
       (split-window ,(second (second deletes)) ,(- (pane-height self) size) nil)
       ,(recreate-subpane-command self))))

(defmethod split-bottom-command ((self emacs-window) size)
  `(split-window ,(emacs-window-window self) ,(- (pane-height self) size) nil))



(defmethod delete-subpane-command ((self horizontal-splits))
  `(progn ,@(mapcar (function delete-subpane-command) (pane-subpanes self))))

(defmethod delete-subpane-command ((self vertical-splits))
  `(progn ,@(mapcar (function delete-subpane-command) (pane-subpanes self))))

(defmethod delete-subpane-command ((self emacs-window))
  `(delete-window ,(emacs-window-window self)))


(defmethod recreate-subpane-command ((self horizontal-splits))
  (let ((height (reduce (function +) (pane-subpanes self)
                        :key (function pane-height))))
    `(progn
       ,@(mapcar (lambda (pane)
                   `(progn ,@(when (plusp (decf height (pane-height pane)))
                                `((split-window nil ,(pane-height pane) nil)))
                           ,(recreate-subpane-command pane)
                           (other-window 1)))
                 (pane-subpanes self)))))

(defmethod recreate-subpane-command ((self vertical-splits))
  (let ((width (reduce (function +) (pane-subpanes self)
                       :key (function pane-width))))
    `(progn
       ,@(mapcar (lambda (pane)
                   `(progn ,@(when (plusp (decf width (pane-width pane)))
                                `((split-window nil ,(pane-width pane) t)))
                           ,(recreate-subpane-command pane)
                           (other-window 1)))
                 (pane-subpanes self)))))


(defmethod recreate-subpane-command ((self emacs-window))
  `(progn
     (switch-to-buffer ,(window-buffer (emacs-window-window self)))
     ;; TODO: set other window parameters from the old window.
     ))



;; (split-bottom (window-pane) 10) ; makes a window 10 lines high at the bottom.
;; You may use C-x r w a to store the current window configuration in register 'a'
;; and         C-x r j a to restore the saved window configuration.



