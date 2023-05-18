;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-browser.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A column-based browser.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-05-08 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2021 - 2021
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
(require 'org)
(require 'org-table)

;;;---------------------------------------------------------------------
;;; Utilities
;;;---------------------------------------------------------------------

(defun emptyp (sequence)
  "Predicate: the sequence is empty."
  (or (null sequence)
      (and (not (consp sequence))
           (zerop (length sequence)))))

(defmacro with-temp-mode (mode &body body)
  (let ((saved-mode (gensym)))
    `(let ((,saved-mode major-mode))
       (unless (eq ,saved-mode ',mode)
         (,mode))
       (unwind-protect
            (progn ,@body)
         (unless (eq ,saved-mode ',mode)
           (funcall ,saved-mode))))))


;;;---------------------------------------------------------------------
;;; Functions missing from org-table
;;;---------------------------------------------------------------------

(defun org-table-columns ()
  "Return the number of column in the current org-table."
  (org-table-analyze)
  org-table-current-ncol)

(defun org-table-clean-column (&optional column)
  "Empty all the data cells in the `column` number or the current column."
  (interactive)
  ;; TODO: perhaps org-table would want to update computed stuff from the new table?
  (let ((column (org-table-current-column)))
    (org-table-goto-column column nil t)
    (org-table-insert-column)
    (org-table-goto-column (+ 1 column) nil t)
    (org-table-delete-column)))

(defun org-table-delete-row (&optional row)
  "Delete the `row` number or the current row."
  (interactive)
  ;; TODO: perhaps org-table would want to update computed stuff from the new table?
  (when row
    (org-table-goto-line row))
  (beginning-of-line)
  (kill-line)
  (org-table-next-field))

(defun org-table-remove-empty-rows ()
  "Delete all the rows in the current table that have only empty cells."
  (interactive)
  ;; TODO: perhaps org-table would want to update computed stuff from the new table?
  (loop
    with row = 1
    while (ignore-errors (org-table-goto-line row))
    if (loop for column from 1 to (org-table-columns)
             for cell = (org-table-get row column)
             ;; do (message "cell[%d,%d]= %S" row column cell)
             always (emptyp cell))
      do (org-table-delete-row row)
    else
      do (incf row)))


;;;---------------------------------------------------------------------
;;; Browser functions
;;;---------------------------------------------------------------------

(defun pjb-insert-browser-table ()
  "Insert at the point a new empty browser table."
  (org-table-create "1x1")
  (org-table-next-field)
  (org-table-hline-and-move nil)
  (org-table-goto-line 2))

(defun pjb-insert-browser-column (column title browser-items)
  "Insert the `browser-items` in the `column` number in the current org-table, under the `title`.
If the table dosn't have that number of columns, they're automatically created."
  ;; browser-item ::= (title action data)
  (org-table-clean-column column)
  (when title
    (org-table-goto-line 1)
    (org-table-goto-column column)
    (org-table-blank-field)
    (insert title))
  (org-table-goto-line 2)
  (org-table-goto-column column)
  (let ((head-pos (point)))
    (loop
      with first-row = t
      for (title action data) in browser-items
      do (if first-row
             (setf first-row nil)
             (org-table-next-row))
         (org-table-blank-field)
         (insert-text-button title
                             'action action
                             'button-data (cons title data)))
    (org-table-remove-empty-rows)
    (goto-char head-pos)
    (org-table-align)))


(defun pjb-browser-browser-window (name)
  (switch-to-buffer (get-buffer-create (format "*%s Browser*" name)))
  (org-mode)

  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)

    (local-set-key "n" 'org-table-next-row)
    (define-key map "p" 'org-table-previous-row)
    (define-key map "n"   'next-item)
    (define-key map "C-n" 'next-item)
    (define-key map "p"   'previous-item)
    (define-key map "C-p" 'previous-item)
    map))

;; (local-set-key (kbd "C-c i") *clage-mode-map*)

(defun pjb-browser-item-window (fname)
  "Locate the window for the file fname, or split the current window for it."
  (let ((buffer (find fname (buffer-list)
                      :key (function buffer-file-name)
                      :test (function equal)))
        (size -10))
    (select-window
     (or
      ;; If there is already a window with the buffer for the file:
      (and buffer (find buffer (window-list) :key (function window-buffer)))
      ;; otherwise, find a good window:
      (let ((window (or (window-in-direction 'below)
                        (split-window (selected-window) size 'above))))
        ;; and visit the buffer there:
        (if buffer
            (switch-to-buffer buffer)
            (find-file fname))
        window)))))


(defun pjb-select-category-item (data)
  (destructuring-bind (category-title &rest items-browser-items) data
    (pjb-insert-browser-column 2 category-title items-browser-items)))

(defun pjb-select-item (data)
  (let ((item (cdr data)))
    (pjb-browser-item-window (item-file item))
    (with-current-buffer (current-buffer)
      (goto-char (point-min))
      (search-forward (item-code item) nil t))))





(defvar *categories* '(requirement specification analysis
                       design code unit-test integration-test
                       documentation))

(defun make-item-browser-tree (buffer)
  (let ((categories (make-hash-table))
        (result '()))
    (dolist (category *categories*)
      (setf (gethash category categories) '()))
    (dolist (item (extract-trace-items-from-buffer buffer))
      (push (list (item-identification item) 'pjb-select-item item)
            (gethash  (item-category item) categories '())))
    (maphash (lambda (category items)
               (push (list (symbol-name category)
                           'pjb-select-category-item
                           (sort* items
                                  (function string<)
                                  :key (function first)))
                     result))
             categories)
    (nreverse result)))

(defun pjb-browse-items ()
  (interactive)
  (let ((item-buffer (current-buffer)))
    (pjb-browser-browser-window (format "Items of %s" (buffer-name)))
    (erase-buffer)
    (pjb-insert-browser-table)
    (pjb-insert-browser-column
     1 "categories"
     (make-item-browser-tree item-buffer))))


;;;; THE END ;;;;
