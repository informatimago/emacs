;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:               pjb-advices.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This module patches various emacs functions.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2012-11-27 <PJB> Updated mail-setup advice for emacs-24.
;;;;    2002-08-13 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2002 - 2012
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
(require 'cl)
(require 'pjb-strings)


;;;----------------------------------------------------------------------------
;;; shell

(defadvice switch-to-buffer (after pjb-nshell-switch-to-buffer nil activate)
  "When switching to a shell buffer, go down to the max position."
  (when (string-match "^\\([0-9][0-9]*<shell>\\|\\*shell\\*\\)$" (buffer-name))
    (goto-char (point-max))))
(ad-activate 'switch-to-buffer)


;; (defadvice shell (around pjb-nshell nil activate)
;;   "Create a new shell when none exist, or switch to the last created one."
;;   (interactive)
;;   (let ((i 0))
;;     (while (get-buffer (format "%d<shell>" i))
;;       (setq i (1+ i)))
;;     (if (= 0 i)
;;       (progn ;; no shell, let's make the first one
;;         ad-do-it
;;         (rename-buffer "0<shell>"))
;;       (progn ;; already have some shells, let's jump to one.
;;         (switch-to-buffer (format "%d<shell>" (- i 1))))
;;       )));;shell
;; (ad-activate 'shell)
;;
;;
;; (defun nshell ()
;;   "Create a new shell."
;;   (interactive)
;;   (ad-with-originals (shell) (shell))
;;   (let ((i 0))
;;     (while (get-buffer (format "%d<shell>" i))
;;       (setq i (1+ i)))
;;     (rename-buffer (format "%d<shell>" i)))
;;   );;nshell


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mouse

(require 'mouse)

(defadvice mouse-drag-vertical-line
  (around pjb-catch-errors (start-EVENT))
  "This advice prevents errors reporting from mouse-drag-vertical-line.
It's not nice to report errors (or drop in the debugger), while mouse-dragging.
"
  ;; leave interactive to mouse-drag-vertical-line itself.
  (condition-case signal
      ad-do-it
    ('error nil)))
(ad-activate 'mouse-drag-vertical-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mail-utils

(require 'sendmail)
(require 'mail-utils)

(defvar bcc-user-mail-address user-mail-address
  "The address used for BCC: when creating a new mail with mail-self-blind set.")

(defadvice mail-setup
  (after pjb-mail-setup-bcc (&rest args))
  "This advice replace the BCC: user-mail-address by BCC:bcc-user-mail-address."
  ;; not interactive
  (when mail-self-blind
    (save-excursion
      (goto-char (point-min))
      (if (search-forward (format "BCC: %s" user-mail-address)
                          (mail-text-start) t)
          (replace-match (format "BCC: %s" bcc-user-mail-address) t t)))))
(ad-activate 'mail-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rmail

(require 'message)
(require 'rmailsort)
(load "rmailsum"  nil t)  ;; no provide in rmailsum.

(defadvice rmail-sort-by-correspondent
  (around pjb-rmail-sort-by-correspondant (reverse))
  "Sort messages of current Rmail file by other correspondent.
If prefix argument REVERSE is non-nil, sort them in reverse order.
The correspondant domain name is heavier than the correspondant name.
Email addresses are not case sensitive."
  (interactive "P")
  (rmail-sort-messages
   reverse
   (function
    (lambda (msg)
      (let* ((address
              (rmail-select-correspondent
               msg
               '("From" "Sender" "To" "Apparently-To")))
             (atpos (string-index address ?@)))
        (downcase
         (if atpos
             (concat
              (unsplit-string
               (reverse (split-string
                         (substring address (+ 1 atpos)) "[.]")) ".")
              "@"
              (substring address 0 atpos))
           address)))))))
(ad-activate 'rmail-sort-by-correspondent)


;;;----------------------------------------------------------------------------
;;; faces

(defun pjb-chop-spaces (string)
  "Returns a substring of `string' with spaces removed left and right."
  (let ((i 0)
        (l (1- (length string)))
        (space 32))
    (while (and (< 0 l) (eq (aref string l) space))
      (setq l (1- l)))
    (setq l (1+ l))
    (while (and (< i l) (eq (aref string i) space))
      (setq i (1+ i)))
    (substring string i l)))


(defadvice x-parse-geometry
  (before pjb-parse-geometry-chop-spaces (string))
  "This advice remove unwanted spaces from the argument."
  ;; not interactive
  (ad-set-arg 0 (pjb-chop-spaces (ad-get-arg 0))))
(ad-activate 'x-parse-geometry)


(defadvice set-face-attribute
  (before pjb-set-face-attribute (face frame &rest args))
  (ad-set-args
   2 (let ((result nil) (couples args) key value)
       (while couples
         (setq key     (car couples)
               value   (cadr couples)
               couples (cddr couples))
         (when (if (eq key :height)
                   (if (numberp value) (/= value 0) value)
                 value)
           (setq result (cons  key (cons value result)))))
       result)))
(ad-activate 'set-face-attribute)

;;;----------------------------------------------------------------------------
;;; frames
(when (< emacs-major-version 22)
  (defadvice other-frame
      (around pjb-other-frame-redirect-frame-focus (arg))
    "This advice tries to ensure, that the new frame gets input focus."
    ;; leave interactive to other-frame
    (let ( (original-frame (selected-frame)) )
      ad-do-it
      (unless (eq window-system 'w32)
        (if focus-follows-mouse
            (set-mouse-position (selected-frame) (1- (frame-width)) 0)
            (redirect-frame-focus original-frame (selected-frame))))))
  (ad-activate 'other-frame))

;;;----------------------------------------------------------------------------
;;; cus-edit
(require 'cus-edit)


(defadvice custom-save-variables
  (around pjb-custom-save-variables-sorted ())
  ;; we won't call ad-do-it because we mustn't let the original write any
  ;; unsorted output.
  "Save all customized variables in `custom-file', sorted."
  ;; not interactive
  (save-excursion
    (custom-save-delete 'custom-set-variables)
    (let ((standard-output (current-buffer)))
      (unless (bolp)
        (princ "\n"))
      (princ "(custom-set-variables")
      (let ((customized-atoms nil))
        (mapatoms (lambda (symbol)
                    (when (get symbol 'saved-value)
                      (setq customized-atoms (cons symbol customized-atoms)))))
        (mapc
         (lambda (symbol)
           (let ((value (get symbol 'saved-value))
                 (requests (get symbol 'custom-requests))
                 (now (not (or (get symbol 'standard-value)
                               (and (not (boundp symbol))
                                    (not (get symbol 'force-value)))))))

             (when value
               (princ "\n '(")
               (princ symbol)
               (princ " ")
               (prin1 (car value))
               (cond (requests
                      (if now
                          (princ " t ")
                        (princ " nil "))
                      (prin1 requests)
                      (princ ")"))
                     (now
                      (princ " t)"))
                     (t
                      (princ ")"))))))
         (sort customized-atoms 'string-lessp)))
      (princ ")")
      (unless (looking-at "\n")
        (princ "\n")))))
(ad-activate 'custom-save-variables)



;;;----------------------------------------------------------------------------
;;; w3

;; (defadvice w3-parse-buffer
;;   (before pjb-w3-parse-buffer-invalid-chars (buffer))
;;   (save-excursion
;;     (set-buffer buffer)
;;     (goto-char (point-min))
;;     (while (re-search-forward "&#\\(8217\\|8230\\);" nil t)
;;       (cond
;;        ((STRING= "8217" (match-string 1)) (replace-match "'" t t))
;;        ((STRING= "8230" (match-string 1)) (replace-match "-" t t))
;;        (t                                 (replace-match "?" t t))))))
;; (ad-activate 'w3-parse-buffer)



;;;----------------------------------------------------------------------------
;;; gnus
(require 'gnus)
(require 'gnus-art)
(require 'message)

(when (fboundp 'gnus-mule-add-group) ; in some old version of emacs...
  (gnus-mule-add-group ""          'iso-8859-1)
  (gnus-mule-add-group "pl"        'iso-8859-2)
  (gnus-mule-add-group "fido7.ru"  'cyrillic-koi8)
  (gnus-mule-add-group "israel"    'hebrew-iso-8bit))


(defadvice gnus
  (before pjb-gnus-email (&optional arg dont-connect slave))
  "This advice reset email-from."
  ;; leave interactive to gnus.
  (email-from "usenet@informatimago.com"))




(defadvice gnus-summary-reply
  (around pjb-gnus-summary-reply-is-followup! (&optional YANK WIDE))
  "This advice replace reply by followup!"
  ;; no ad-do-it!
  (interactive (list (and current-prefix-arg
                          (gnus-summary-work-articles 1))))
  (gnus-summary-followup nil))
(ad-activate 'gnus-summary-reply)




;; mail-default-reply-to "<pjb@informatimago.com>"
;; user-mail-address     "pjb@informatimago.com"
;; bcc-user-mail-address "pascal"

(defvar pjb-mail-default-reply-to nil "Saved value")
(defvar pjb-user-mail-address     nil "Saved value")


(defun reset-email-from ()
 "Resets mail-default-reply-to  and  user-mail-address to their default values."
 (interactive)
 (when pjb-user-mail-address
   (setq mail-default-reply-to     pjb-mail-default-reply-to)
   (setq user-mail-address         pjb-user-mail-address)
   (setq pjb-mail-default-reply-to nil)
   (setq pjb-user-mail-address     nil)
   (message "Reverted email from %S." user-mail-address)))


(defun email-from (address &optional comment)
  "Sets mail-default-reply-to  and  user-mail-address to the given adress."
  (interactive "sNew email: ")
  (message "Changing email from %S." address)
  (unless pjb-user-mail-address
   (setq pjb-mail-default-reply-to     mail-default-reply-to)
   (setq pjb-user-mail-address         user-mail-address))
  (when address
    (setq mail-default-reply-to   (if comment
                                    (format "%s <%s>" comment address)
                                    (format "<%s>"  address)))
    (setq user-mail-address       address)))


(defadvice message-make-sender
  (around pjb-message-make-sender ())
  "This advice doesn't return the \"real\" user address.
Instead, it returns the user address the user wants to return.
There's too much spam sent to addresses flowing on the newsgroups..."
  (if pjb-user-mail-address
      ;; when pjb-user-mail-address is not nil, it's the real user address saved
      ;; and user-mail-address contains the fake user address.
      (setq ad-return-value user-mail-address)
    ;; if the user did not fake his address, then do default stuff:
    ad-do-it))
(ad-activate 'message-make-sender)


;;;----------------------------------------------------------------------------
;;; debug
(require 'debug)

(defadvice backtrace
  (around pjb-backtrace ())
  ""
  (message "backtrace %s %s %s" print-escape-newlines print-level print-length)
  (let ((print-escape-newlines t)
        (print-level  nil)
        (print-depth  nil)
        (print-length nil))
    ad-do-it))
(ad-activate 'backtrace)


;;;----------------------------------------------------------------------------
;;; jumping to register
(require 'register)

(defvar *jump-to-register-offset* (lambda () 10)
  "A function that returns the number of lines that should show above the
register point. (You could implement here a proportional rule like 30%.")


(defadvice jump-to-register (after pjb-jump-to-register
                                    (register &optional delete))
  "Move point to location stored in a register.
If the register contains a file name, find that file.
 \(To put a file name in a register, you must use `set-register'.)
If the register contains a window configuration (one frame) or a frame
configuration (all frames), restore that frame or all frames accordingly.
First argument is a character, naming the register.
Optional second arg non-nil (interactively, prefix argument) says to
delete any existing frames that the frame configuration doesn't mention.
\(Otherwise, these frames are iconified.)"
  (interactive "cJump to register: \nP")
  (recenter (funcall *jump-to-register-offset*)))

;; (provide 'pjb-advices)
;;;; THE END ;;;;

