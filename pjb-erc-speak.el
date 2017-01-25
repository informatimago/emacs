;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-erc-speak.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    erc speak stuff.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-02-24 <PJB> Extracted from ~/rc/emacs-common.el
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
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
(require 'cl)
(require 'erc)
(require 'pjb-speak)

;;;---------------------------------------------------------------------
;;; ERC speach
;;;---------------------------------------------------------------------

(defparameter *pjb-erc-spoken-nicks*
  '(("\\<e1f\\>"          . "elf")
    ("\\<tali[0-9]+"      . "tali")
    ("\\<fsbot\\>"        . "F. S. Bot")
    ("\\<qu1j0t3\\>"      . "quijote")
    ("\\<chromaticwt\\>"  . "chromatic W. T.")
    ("\\<jcowan\\>"       . "J. Cowan")
    ("\\<cky\\>"          . "C. K. Y.")
    ("\\<pjb\\>"          . "Pascal")
    ("\\<H4ns\\>"         . "Hans")
    ("\\<Corman[0-9]+\\>" . "Corman"))
  "An a-list mapping regexps of nicks to the corresponding text to be read aloud.")


(defun pjb-erc-spoken-nick (nick)
  "
RETURN:  The text to be read aloud for the `nick' in `*pjb-erc-spoken-nicks*'.
"
  (let ((entry (assoc* nick *pjb-erc-spoken-nicks*
                       :test (lambda (nick ref) (string-match ref nick)))))
    (if entry
        (cdr entry)
        nick)))


(defun erc-response.recipient (response)
  (first (erc-response.command-args response)))

(defun erc-response.sender-nick (response)
  (let ((sender (erc-response.sender response)))
   (subseq sender 0 (position ?! sender))))


(defparameter *pjb-erc-massage-substitutions*
  '(("\\<pjb\\>"                 "Pascal")
    ("\\<CL\\>"                  "See Ell")
    ("\\<C-"                     "Control-")
    ("\\<M-"                     "Meta-")
    ("\\<A-"                     "Alt-")
    ("\\<S-"                     "Shift-")
    ("\\<s-"                     "super-")
    ("\\<H-"                     "Hyper-")
    ("\\(:-?)\\|(-?:\\)"         "AhAhAh!")
    (":-?("                      "BooBooBoo!")
    (":-/"                       "muek")
    (":-?[Pp]"                   "bruu")
    ("\\<\\(ty\\|thx\\)\\>"      "Thank you!")
    ("\\<LOL\\>"                 "AhAhAh! Laughting Out Loud!")
    ("\\<ROFL\\>"                "AhAhAh! Rolling On the Floor!")
    ("\\<hrm\\>"                 "errrmmm")
    ("\\<btw\\>"                 "by the way")
    ("\\<wtf\\>"                 "what the fuck")
    ("\\<imo\\>"                 "in my opinion")
    ("\\<imho\\>"                "in my humble opinion")
    ("\\<imnsho\\>"              "in my not so humble opinion")))


(defun pjb-erc-massage-message (message)
  (with-current-buffer (get-buffer-create "*pjb massage text*")
    (erase-buffer)
    (insert message)
    (let ((case-fold-search nil))
      (loop
         for (reg sub) in *pjb-erc-massage-substitutions*
         do (progn
              (goto-char (point-min))
              (loop
                 while (re-search-forward reg nil t)
                 do (progn
                      (delete-region (match-beginning 0) (match-end 0))
                      (insert sub))))))
    (buffer-string)))



(defvar *pjb-erc-speak-reject-recipient* '()
  "can be:
nil   don't reject any channel.
:all  reject every channel.
or a list of nicknames or channel names \"nick\" \"\#chan\"
to reject (never speak them aloud).
See: `*pjb-erc-speak-reject-sender*', `*pjb-erc-speak-accept-sender*',
      and `pjb-erc-privmsg-meat'.

Messages are spoken if the recipient
")

(defvar *pjb-erc-speak-reject-sender* '()
  "can be:
nil   don't reject anybody.
:all  reject everybody.
or a list of nicknames or channel names \"nick\" \"\#chan\"
to reject (never speak them aloud).
See: `*pjb-erc-speak-reject-recipient*', `*pjb-erc-speak-accept-sender*',
      and `pjb-erc-privmsg-meat'.
")

(defvar *pjb-erc-speak-accept-sender* '()
  "can be:
nil   don't accept anything.
:all  accept everything.
or a list of nicknames or channel names \"nick\" \"\#chan\"
to accept (speak them aloud).
See: `*pjb-erc-speak-reject-recipient*', `*pjb-erc-speak-reject-sender*',
      and `pjb-erc-privmsg-meat'.
")


(setf *pjb-erc-speak-accept-sender*    :all
      *pjb-erc-speak-reject-sender*    '("minion" "clhs" "specbot")
      *pjb-erc-speak-reject-recipient* '("minion" "clhs" "specbot"))

(setf *pjb-erc-speak-reject-recipient* '("#emacs")
      *pjb-erc-speak-reject-recipient* :all
      *pjb-erc-speak-reject-sender*    :all
      *pjb-erc-speak-accept-sender*    '("Posterdati" "pjb-"))



(defun pjb-erc-member (item list-designator)
  "Return whether the `item' is in a virtual list designated by `list-designator'.
`list-designator' may be nil for an empty list),
:all or t for a list containing everything, or
an actual list.
items are compared with `string='.
"
  (case list-designator
    ((nil)     nil)
    ((:all t)  t)
    (otherwise (member* item list-designator :test 'string=))))


(defun pjb-erc-prepare-message (reponse last-speaker)
  "Return a list (formated-message new-last-speaker)"
  (let* ((nick (pjb-erc-spoken-nick (erc-response.sender-nick response)))
         (chan (pjb-erc-spoken-nick (remove ?# (erc-response.recipient response))))
         (mesg (pjb-erc-massage-message (erc-response.contents response))))
    (if (equal last-speaker (cons nick chan))
        (list (format "%s" mesg)
              last-speaker)
        (list (format "%s said to %s: ... %s" nick chan mesg)
              (cons nick chan)))))


(defvar *pjb-erc-speak-last-speaker* nil)
(defun pjb-erc-speak-privmsg-meat (process response)
  "The messages are spoken if the sender is in `*pjb-erc-speak-accept-sender*',
or the sender is not in `*pjb-erc-speak-reject-sender*',
or the recipient is not in `*pjb-erc-speak-reject-recipient*',
"
  (when (or (pjb-erc-member (erc-response.sender-nick response)
                            *pjb-erc-speak-accept-sender*)
            (pjb-erc-member (erc-response.sender-nick response)
                            *pjb-erc-speak-reject-sender*)
            (pjb-erc-member (erc-response.recipient response)
                            *pjb-erc-speak-reject-recipient*))
    (let ((message (pjb-erc-prepare-message response *pjb-erc-speak-last-speaker*)))
      (setf *pjb-erc-speak-last-speaker* (second message))
      (speak (first message))))
  nil)

(defun pjb-erc-speak-on ()
  (interactive)
  (pushnew 'pjb-erc-speak-privmsg-meat  erc-server-PRIVMSG-functions))

(defun pjb-erc-speak-off  ()
  (interactive)
  (setf erc-server-PRIVMSG-functions
        (remove 'pjb-erc-speak-privmsg-meat erc-server-PRIVMSG-functions)))




(defvar *pjb-erc-tooltip-last-tooltiper* nil)
(defun pjb-erc-tooltip-privmsg-meat (process response)
  "The messages are displayed in tooltips."
  (let ((message (pjb-erc-prepare-message response *pjb-erc-tooltip-last-tooltiper*)))
    (setf *pjb-erc-tooltip-last-tooltiper* (second message))
    (tooltip-show (first message)))
  nil)

(defun pjb-erc-tooltip-on ()
  (interactive)
  (pushnew 'pjb-erc-tooltip-privmsg-meat  erc-server-PRIVMSG-functions))

(defun pjb-erc-tooltip-off  ()
  (interactive)
  (setf erc-server-PRIVMSG-functions
        (remove 'pjb-erc-tooltip-privmsg-meat erc-server-PRIVMSG-functions)))



(provide 'pjb-erc-speak)
;;;; THE END ;;;;
