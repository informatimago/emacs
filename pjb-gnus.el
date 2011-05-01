;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-gnus.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines some GNU commands.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-06-07 <PJB> Created.
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


(defvar *pjb-gnus-trash-mailbox* nil
  "Should be a string naming a mailbox, such as: \"nnimap+imap.example.com:INBOX.Trash\".")

(defun pjb-gnus-summary-move-article-to-trash (n)
  "Moves the messages to the Trash mailbox specified in `*pjb-gnus-trash-mailbox*'."
  (interactive "p")
  (gnus-summary-move-article n *pjb-gnus-trash-mailbox*)
  (gnus-summary-next-unread-article))


(defvar *pjb-gnus-junk-mailbox* nil
  "Should be a string naming a mailbox, such as: \"nnimap+imap.example.com:INBOX.Junk\".")

(defun pjb-gnus-summary-move-article-to-junk (n)
  "Moves the messages to the Junk mailbox specified in `*pjb-gnus-junk-mailbox*'."
  (interactive "p")
  (gnus-summary-move-article n *pjb-gnus-junk-mailbox*)
  (gnus-summary-next-unread-article))


(defun pjb-gnus-mark-unread (&optional n)
  (interactive "p")
  (cond
    ((null n)  (gnus-summary-tick-article))
    ((zerop n))
    ((=  1 n)   (gnus-summary-tick-article))
    ((= -1 n)   (gnus-summary-tick-article))
    ((minusp n)
     (gnus-summary-tick-article)
     (previous-line)          ; (gnus-summary-previous-unread-article)
     (pjb-gnus-mark-unread (1+ n)))
    (t
     (gnus-summary-tick-article)
     (next-line)                  ; (gnus-summary-next-unread-article)
     (pjb-gnus-mark-unread (1- n)))))



;;;; THE END ;;;;
