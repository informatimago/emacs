;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;*****************************************************************************
;;;;FILE:               pjb-vm-kill-file.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    A kill file feature for vm.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon 
;;;;MODIFICATIONS
;;;;    2002-11-10 <PJB> Converted from pjb-rmail-kill-file.
;;;;
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2002 - 2011
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
;;;;*****************************************************************************

(require 'cl)
(require 'pjb-cl)
(require 'pjb-strings)

(when (require 'vm nil t)


  (defvar pjb-vm-killed-ip-list   nil
    "List of IP to kill from Received:.")
  (defvar pjb-vm-killed-ip-regexp nil
    "deduced form pjb-vm-killed-ip-list.
String regexp matching IPs to be killed from Received:.")

  (defvar pjb-vm-kill-file "~/.emacs-kill-file" 
    "The path to the kill file.")



  (defun pjb-vm-kill-address (ip)
    "
DO:    Add ip to the `pjb-vm-killed-ip-list'.
"
    (unless pjb-vm-killed-ip-list
      (load pjb-vm-kill-file t t))
    (unless (member ip pjb-vm-killed-ip-list)
      (setq pjb-vm-killed-ip-regexp nil)
      (push ip pjb-vm-killed-ip-list)
      (find-file pjb-vm-kill-file)
      (erase-buffer)
      (insert (format "(setq pjb-vm-killed-ip-list \n'%S)\n"
                      pjb-vm-killed-ip-list))
      (save-buffer 0)
      (kill-buffer (current-buffer))))



  (defun pjb-vm-kill-add-this-address (beg end)
    "
DO:    Add the selected IP address to the `pjb-vm-killed-ip-list'.
"
    (interactive "r")
    (let ((selection (buffer-substring-no-properties beg end)))
      (unless (or (string-match "\\[\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)\\]"
                                selection)
                  (string-match "(\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\))"
                                selection)
                  (string-match "\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)"
                                selection))
        (error "Please select an IP address first."))
      (pjb-vm-kill-address (match-string 1 selection))))



  (defun pjb-vm-kill-ip-regexp ()
    "
DO:     Build a regexp with all the IP from pjb-vm-killed-ip-list.
RETURN: A regexp string.
"
    (if pjb-vm-killed-ip-regexp
        pjb-vm-killed-ip-regexp
        (setq pjb-vm-killed-ip-regexp
              (concat
               "[[(]\\("
               (unsplit-string (mapcar (lambda (ip)
                                         (regexp-quote (format "%s" ip)))
                                       pjb-vm-killed-ip-list)
                               "\\|")
               "\\)\\."))))




  (defmacro message-cond (condition)
    `(let ((result  ,condition))
       (if result 
           (message "%S is TRUE." (quote ,condition)))
       result))


  
  (defun pjb-vm-kill-file ()
    "
PRE:    The current buffer is narrowed to one new message.
POST:   If the message match our \"kill-file\", then it is marked deleted.
"
    ;; (message "pjb-vm-kill-file message %S" message)
    (save-restriction
      (let ((msg-min (point-min))
            (msg-max (point-max))
            )
        (goto-char msg-min)
        (narrow-to-region msg-min (search-forward "\n\n"))
        (when
            (or
             (let ((value (mail-fetch-field "From")))
               (message "*** from=%S match ark=%S" value (string-match "<arkresearch@hotmail.com>" value))
               (when value
                 (or
                  (message-cond (string-match "<onthecuttingedge2005@yahoo.com>" value))
                  (message-cond (string-match "<arkresearch@hotmail.com>" value))
                  (message-cond (string-match "evaluemail.com" value))
                  (message-cond (string-match "e-valuemail.com" value))
                  (message-cond (string-match "\\.kr>" value))
                  )))

             (let ((value (mail-fetch-field "Message-Id")))
               (message "*** message-id=%S" value)
               (when value
                 (message-cond (string-match "\\.kr>" value)) ))

             (let ((value (mail-fetch-field "Subject")))
               (message "*** subject=%S" value)
               (when value
                 (or 
                  (message-cond (string-match "Credit Repair\\|$$$" value))
                  (message-cond (string-match "=?big5?" value)) 
                  (message-cond (string-match "=?euc-kr?" value)) 
                  (message-cond (string-match "(±¤°í)" value))
                  )))

             (let ((value (mail-fetch-field "Received" nil t)))
               (message "*** received=%S" value)
               (when value
                 (message-cond (string-match " hanmail.net \\|localhost.com"  value))
                 (message-cond (string-match (pjb-vm-kill-ip-regexp)  value)) ))
             (let ((value (mail-fetch-field "Content-Type")))
               (message "*** content-type=%S" value)
               (when value
                 (or (message-cond (string-match "charset=\"?ks_c_5601-1987" value))
                     (message-cond (string-match "charset=\"?euc-kr"         value))
                     (message-cond (string-match "charset=\"?ISO-2022-KR"    value))
                     )
                 ) ;;when
               ) ;;let

             (progn
               (goto-char (point-min))
               (if (search-forward "informatimago.com" nil t)
                   (progn
                     (goto-char (point-min))
                     (not (re-search-forward "\\<pjb@informatimago.com\\>" nil t)))
                   nil)) ;;progn

             ) ;;or
          ;;(message "*** kf-4  killing %S" message)
          (vm-delete-message 1)
          ;; (run-hooks 'rmail-delete-message-hook)
          ))))



  (defun pjb-vm-kill-subject-regexp (regexp)
    "Delete all messages whose subject matches regexp
"
    (interactive "sRegexp: ")
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer)
    (vm-check-for-killed-summary)
    (vm-error-if-folder-read-only)
    (vm-error-if-folder-empty)
    (let ((mp vm-message-list)
          (n 0)
          (case-fold-search t))
      (while mp
        (if (and (not (vm-deleted-flag (car mp)))
                 (string-match regexp (vm-so-sortable-subject (car mp))))
            (progn
              (vm-set-deleted-flag (car mp) t)
              (vm-increment n)))
        (setq mp (cdr mp)))
      (and (interactive-p)
           (if (zerop n)
               (message "No messages deleted.")
               (message "%d message%s deleted" n (if (= n 1) "" "s")))))
    (vm-display nil nil '(vm-kill-subject) '(vm-kill-subject))
    (vm-update-summary-and-mode-line))

  ;;(add-hook 'vm-arrived-message-hook 'pjb-vm-kill-file)
  ;;(add-hook 'vm-arrived-message-hook
  ;;          (lambda () (pjb-vm-kill-subject-regexp "\\[SPAM\\]")))



  );; when require

(provide 'pjb-vm-kill-file)
;;;; THE END ;;;;

