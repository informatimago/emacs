;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-ruby.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Help for Ruby editing.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-09-22 <PJB> Created.
;;;;BUGS
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


(defun pjb-ruby-send-block ()
  "Send the current block to the inferior Ruby process."
  (interactive)
  (save-excursion
    (ruby-end-of-block)
    (end-of-line)
    (let ((end (point)))
      (ruby-beginning-of-block)
      (message "sending to ruby: %S"
               (buffer-substring-no-properties (point) end))
      (ruby-send-region (point) end))))


(defun pjb-ruby-eval-last-sexp ()
  (interactive)
  (let ((curbuf (current-buffer)))
    (save-excursion
      (let ((start (progn (ruby-backward-sexp) (point)))
            (end   (progn (ruby-forward-sexp)  (point))))
         ;; (message "sending to ruby: %S" (buffer-substring-no-properties start end))
        (ruby-send-region start end))
      (pop-to-buffer ruby-buffer)
      (goto-char (point-max))
      (pop-to-buffer curbuf))))


(defun pjb-ruby-mode-meat ()
  (interactive)
  (local-set-key (kbd "C-c ,")    (function ruby-backward-sexp))
  (local-set-key (kbd "C-c .")    (function ruby-forward-sexp))
  (local-set-key (kbd "C-x C-e") (function pjb-ruby-eval-last-sexp))
  (setf inferior-ruby-first-prompt-pattern "^irb\\(.*\\)[>*\"'] *")
  (values))

(add-hook 'ruby-mode-hook (function pjb-ruby-mode-meat))
