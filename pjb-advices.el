;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;;;******************************************************************************
;;;;FILE:               pjb-advices.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This module installs PJB-specific advices on a handful of
;;;;    Emacs commands.  Phase 2 of MODERNIZATION.md ported the file
;;;;    from the deprecated `defadvice' / `ad-activate' API to
;;;;    `advice-add'.  A few entries that used to be body-replacing
;;;;    advices have been demoted to plain commands the user can
;;;;    invoke or rebind explicitly.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2026-04-06 <PJB> Phase 2: defadvice -> advice-add.
;;;;    2012-11-27 <PJB> Updated mail-setup advice for emacs-24.
;;;;    2002-08-13 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;    Copyright Pascal J. Bourguignon 2002 - 2026
;;;;******************************************************************************
(require 'cl-lib)
(require 'pjb-strings)


;;;----------------------------------------------------------------------------
;;; shell

(defun pjb-advices--switch-to-shell-end (&rest _args)
  "When switching to a shell buffer, jump to `point-max'."
  (when (string-match "^\\([0-9][0-9]*<shell>\\|\\*shell\\*\\)$"
                      (buffer-name))
    (goto-char (point-max))))
(advice-add 'switch-to-buffer :after #'pjb-advices--switch-to-shell-end)


;;;----------------------------------------------------------------------------
;;; mouse

(require 'mouse)

(defun pjb-advices--mouse-drag-vertical-line-no-error (orig-fun &rest args)
  "Swallow errors from `mouse-drag-vertical-line' so we don't drop into
the debugger mid-drag."
  (condition-case _err
      (apply orig-fun args)
    (error nil)))
(advice-add 'mouse-drag-vertical-line :around
            #'pjb-advices--mouse-drag-vertical-line-no-error)


;;;----------------------------------------------------------------------------
;;; mail-utils

(require 'sendmail)
(require 'mail-utils)

(defvar bcc-user-mail-address user-mail-address
  "The address used for BCC: when creating a new mail with `mail-self-blind' set.")

(defun pjb-advices--mail-setup-bcc (&rest _args)
  "After `mail-setup', rewrite a `BCC: <user-mail-address>' header to
use `bcc-user-mail-address' instead."
  (when mail-self-blind
    (save-excursion
      (goto-char (point-min))
      (when (search-forward (format "BCC: %s" user-mail-address)
                            (mail-text-start) t)
        (replace-match (format "BCC: %s" bcc-user-mail-address) t t)))))
(advice-add 'mail-setup :after #'pjb-advices--mail-setup-bcc)


;;;----------------------------------------------------------------------------
;;; rmail

(require 'message)
(require 'rmailsort)
(load "rmailsum"  nil t)  ;; no provide in rmailsum.

(defun pjb-advices--rmail-sort-key-by-domain (msg)
  "Sort key used by `pjb-rmail-sort-by-correspondent'."
  (let* ((address (rmail-select-correspondent
                   msg '("From" "Sender" "To" "Apparently-To")))
         (atpos (string-index address ?@)))
    (downcase
     (if atpos
         (concat
          (unsplit-string
           (reverse (split-string (substring address (+ 1 atpos)) "[.]")) ".")
          "@"
          (substring address 0 atpos))
       address))))

(defun pjb-rmail-sort-by-correspondent (reverse)
  "Sort messages of current Rmail file by correspondent.
The correspondent's domain is the heaviest part of the key (so e.g.
`alice@example.com' and `bob@example.com' sort together)."
  (interactive "P")
  (rmail-sort-messages reverse #'pjb-advices--rmail-sort-key-by-domain))


;;;----------------------------------------------------------------------------
;;; faces / X geometry

(defun pjb-chop-spaces (string)
  "Return STRING with leading and trailing space characters stripped."
  (let ((i 0)
        (l (1- (length string)))
        (space 32))
    (while (and (< 0 l) (eq (aref string l) space))
      (setq l (1- l)))
    (setq l (1+ l))
    (while (and (< i l) (eq (aref string i) space))
      (setq i (1+ i)))
    (substring string i l)))

(defun pjb-advices--x-parse-geometry-chop-spaces (args)
  "Strip stray whitespace from the geometry string before passing it on."
  (cons (pjb-chop-spaces (car args)) (cdr args)))
(advice-add 'x-parse-geometry :filter-args
            #'pjb-advices--x-parse-geometry-chop-spaces)

(defun pjb-advices--set-face-attribute-drop-bad-height (face frame &rest args)
  "Drop spurious `:height 0' / `:height nil' pairs before they reach
`set-face-attribute' (some themes pass them and break the face)."
  (let (result couples key value)
    (setq couples args)
    (while couples
      (setq key     (car couples)
            value   (cadr couples)
            couples (cddr couples))
      (when (if (eq key :height)
                (if (numberp value) (/= value 0) value)
              value)
        (push value result)
        (push key result)))
    (cons face (cons frame result))))
(advice-add 'set-face-attribute :filter-args
            (lambda (args)
              (apply #'pjb-advices--set-face-attribute-drop-bad-height args)))


;;;----------------------------------------------------------------------------
;;; cus-edit
(require 'cus-edit)

(defun pjb-custom-save-variables-sorted ()
  "Save all customized variables in `custom-file', sorted by symbol name.
This used to be a `defadvice' on `custom-save-variables' that replaced
its body wholesale; it is now an explicit command — call it instead of
`custom-save-variables' when you want a stable, sorted custom file."
  (interactive)
  (save-excursion
    (custom-save-delete 'custom-set-variables)
    (let ((standard-output (current-buffer)))
      (unless (bolp) (princ "\n"))
      (princ "(custom-set-variables")
      (let ((customized-atoms nil))
        (mapatoms (lambda (symbol)
                    (when (get symbol 'saved-value)
                      (push symbol customized-atoms))))
        (mapc
         (lambda (symbol)
           (let ((value    (get symbol 'saved-value))
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
                      (princ (if now " t " " nil "))
                      (prin1 requests)
                      (princ ")"))
                     (now (princ " t)"))
                     (t   (princ ")"))))))
         (sort customized-atoms 'string-lessp)))
      (princ ")")
      (unless (looking-at "\n") (princ "\n")))))


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

(defun pjb-advices--gnus-reset-email (&rest _args)
  "Reset `email-from' before entering Gnus."
  (email-from "usenet@informatimago.com"))
(advice-add 'gnus :before #'pjb-advices--gnus-reset-email)

(defun pjb-gnus-summary-reply-as-followup (&optional _yank _wide)
  "Posted as a `gnus-summary-followup' instead of a private reply.
This used to be a body-replacing `defadvice' on `gnus-summary-reply';
the user is now expected to bind this command to `R' explicitly when
they want followup-by-default behaviour."
  (interactive (list (and current-prefix-arg
                          (gnus-summary-work-articles 1))))
  (gnus-summary-followup nil))


;; mail-default-reply-to "<pjb@informatimago.com>"
;; user-mail-address     "pjb@informatimago.com"
;; bcc-user-mail-address "pascal"

(defvar pjb-mail-default-reply-to nil "Saved value")
(defvar pjb-user-mail-address     nil "Saved value")

(defun reset-email-from ()
  "Reset `mail-default-reply-to' and `user-mail-address' to their
saved (real) values."
  (interactive)
  (when pjb-user-mail-address
    (setq mail-default-reply-to     pjb-mail-default-reply-to)
    (setq user-mail-address         pjb-user-mail-address)
    (setq pjb-mail-default-reply-to nil)
    (setq pjb-user-mail-address     nil)
    (message "Reverted email from %S." user-mail-address)))

(defun email-from (address &optional comment)
  "Set `mail-default-reply-to' and `user-mail-address' to ADDRESS."
  (interactive "sNew email: ")
  (message "Changing email from %S." address)
  (unless pjb-user-mail-address
    (setq pjb-mail-default-reply-to mail-default-reply-to)
    (setq pjb-user-mail-address     user-mail-address))
  (when address
    (setq mail-default-reply-to (if comment
                                    (format "%s <%s>" comment address)
                                  (format "<%s>" address)))
    (setq user-mail-address     address)))

(defun pjb-advices--message-make-sender (orig-fun &rest args)
  "If the user has faked their email-from, return the fake address;
otherwise let `message-make-sender' do its real work."
  (if pjb-user-mail-address
      ;; pjb-user-mail-address holds the real address; user-mail-address
      ;; holds the fake one we want to send out as.
      user-mail-address
    (apply orig-fun args)))
(advice-add 'message-make-sender :around #'pjb-advices--message-make-sender)


;;;----------------------------------------------------------------------------
;;; debug
(require 'debug)

(defun pjb-advices--backtrace-print-everything (orig-fun &rest args)
  "Run `backtrace' with print-* knobs cranked up so we get the full data."
  (let ((print-escape-newlines t)
        (print-level  nil)
        (print-depth  nil)
        (print-length nil))
    (apply orig-fun args)))
(advice-add 'backtrace :around #'pjb-advices--backtrace-print-everything)


;;;----------------------------------------------------------------------------
;;; jumping to register
(require 'register)

(defvar *jump-to-register-offset* (lambda () 10)
  "Function returning the number of context lines to show above the
register point after `jump-to-register'.")

(defun pjb-advices--jump-to-register-recenter (&rest _args)
  (recenter (funcall *jump-to-register-offset*)))
(advice-add 'jump-to-register :after #'pjb-advices--jump-to-register-recenter)


(provide 'pjb-advices)
;;;; THE END ;;;;
