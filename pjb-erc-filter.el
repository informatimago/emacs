;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-erc-filter.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Filters irc nicks.
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

(defcustom erc-ignore-per-channel-alist nil
  "*A-List of regexps matching user identifiers to ignore, for each channel.

Some users are obnoxious only in some channels (eg. rudybot on #emacs).

A user identifier has the form \"nick!login@host\".  If an
identifier matches, the message from the person will not be
processed."
  :group 'erc-ignore
  :type '(repeat (cons string regexp)))

(defcustom erc-ignore-per-channel-reply-alist nil
  "*A-List of regexps matching user identifiers to ignore completely, for each channel.

Some users are obnoxious only in some channels (eg. rudybot on #emacs).


This differs from `erc-ignore-list' in that it also ignores any
messages directed at the user.

A user identifier has the form \"nick!login@host\".

If an identifier matches, or a message is addressed to a nick
whose identifier matches, the message will not be processed.

CAVEAT: ERC doesn't know about the user and host of anyone who
was already in the channel when you joined, but never said
anything, so it won't be able to match the user and host of those
people.  You can update the ERC internal info using /WHO *."
  :group 'erc-ignore
  :type '(repeat (cons string regexp)))

;; ;; Note: it would be better to have  per-server-per-channel variables…
;; (make-variable-buffer-local 'erc-ignore-per-channel-list) ; in server buffers.
;; (make-variable-buffer-local 'erc-ignore-per-channel-reply-list) ; in server buffers.


(defun erc-ignored-user-in-channel-p (msg tgt spec)
  "Return non-nil if SPEC matches something in `erc-ignore-list'.

Takes a full SPEC of a user in the form \"nick!login@host\", and
matches against all the regexp's in `erc-ignore-list'.  If any
match, returns that regexp."
  (loop
     for (channel . regexp) in (erc-with-server-buffer erc-ignore-per-channel-alist)
     thereis (and (string= channel tgt)
                  (string-match regexp spec))))


(defun erc-message-target (msg)
  "Return the addressed target in MSG.

The addressed target is the string before the first colon in MSG."
  (if (string-match "^\\([^:, \n]*\\):" msg)
      (match-string 1 msg)
    nil))


(defun erc-ignored-reply-p (msg tgt proc)
  ;; FIXME: this docstring needs fixing -- Lawrence 2004-01-08
  "Return non-nil if MSG matches something in `erc-ignore-reply-list'.

Takes a message MSG to a channel and returns non-nil if the addressed
user matches any regexp in `erc-ignore-reply-list'."
  (let ((target-nick (erc-message-target msg)))
    (if (not target-nick)
        nil
        (erc-with-buffer (tgt proc)
          (let ((user (erc-get-server-user target-nick)))
            (when user
              (let ((spec (erc-user-spec user)))
                (or (erc-list-match erc-ignore-reply-list spec)
                    (loop
                       for (channel . regexp) in (erc-with-server-buffer erc-ignore-per-channel-reply-alist)
                       thereis (and (string= channel tgt)
                                    (string-match regexp spec)))))))))))


(when (require 'erc-backend nil t)
  (define-erc-response-handler (PRIVMSG NOTICE)
      "Handle private messages, including messages in channels." nil
      (let ((sender-spec (erc-response.sender parsed))
            (cmd (erc-response.command parsed))
            (tgt (car (erc-response.command-args parsed)))
            (msg (erc-response.contents parsed)))
        (if (or (erc-ignored-user-p                    sender-spec)
                (erc-ignored-user-in-channel-p msg tgt sender-spec)
                (erc-ignored-reply-p           msg tgt proc))
            (when erc-minibuffer-ignored
              (message "Ignored %s from %s to %s for %s %s %s" cmd sender-spec tgt
                       (erc-ignored-user-p                    sender-spec)
                       (erc-ignored-user-in-channel-p msg tgt sender-spec)
                       (erc-ignored-reply-p           msg tgt proc)))
            (let* ((sndr (erc-parse-user sender-spec))
                   (nick (nth 0 sndr))
                   (login (nth 1 sndr))
                   (host (nth 2 sndr))
                   (msgp (string= cmd "PRIVMSG"))
                   (noticep (string= cmd "NOTICE"))
                   ;; S.B. downcase *both* tgt and current nick
                   (privp (erc-current-nick-p tgt))
                   s buffer
                   fnick)
              (setf (erc-response.contents parsed) msg)
              (setq buffer (erc-get-buffer (if privp nick tgt) proc))
              (when buffer
                (with-current-buffer buffer
                  ;; update the chat partner info.  Add to the list if private
                  ;; message.  We will accumulate private identities indefinitely
                  ;; at this point.
                  (erc-update-channel-member (if privp nick tgt) nick nick
                                             privp nil nil host login nil nil t)
                  (let ((cdata (erc-get-channel-user nick)))
                    (setq fnick (funcall erc-format-nick-function
                                         (car cdata) (cdr cdata))))))
              (cond
                ((erc-is-message-ctcp-p msg)
                 (setq s (if msgp
                             (erc-process-ctcp-query proc parsed nick login host)
                             (erc-process-ctcp-reply proc parsed nick login host
                                                     (match-string 1 msg)))))
                (t
                 (setcar erc-server-last-peers nick)
                 (setq s (erc-format-privmessage
                          (or fnick nick) msg
                          ;; If buffer is a query buffer,
                          ;; format the nick as for a channel.
                          (and (not (and buffer
                                         (erc-query-buffer-p buffer)
                                         erc-format-query-as-channel-p))
                               privp)
                          msgp))))
              (when s
                (if (and noticep privp)
                    (progn
                      (run-hook-with-args 'erc-echo-notice-always-hook
                                          s parsed buffer nick)
                      (run-hook-with-args-until-success
                       'erc-echo-notice-hook s parsed buffer nick))
                    (erc-display-message parsed nil buffer s)))
              (when (string= cmd "PRIVMSG")
                (erc-auto-query proc parsed)))))))

(provide 'pjb-erc-filter)
