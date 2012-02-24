;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:               pjb-server.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This module exports functions to manage a TCP server process in emacs
;;;;    (using netcat to listen).
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon 
;;;;MODIFICATIONS
;;;;    2002-04-10 <PJB> Created.
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
;;;;******************************************************************************

(require 'pjb-cl)
(require 'pjb-queue)
(require 'pjb-utilities)
(provide 'pjb-server)


(defvar netcat-process)



(defun pjb-server$open   (remote-ip remote-port read-call-back)
  "
RETURN:  An open pjb-server or nil.
"
  (let ( (stream-process (open-network-stream "pjb-server$$network-stream" 
                                              nil remote-ip remote-port)) )
    
    (set-process-filter stream-process read-call-back)
    stream-process)
  );;pjb-server$open


(defun pjb-server$listen (local-ip local-port read-call-back)
  "
NOTE:    READ-CALL-BACK must be a function (lambda (SERVER STRING)) that 
         will be called when STRING data is available from SERVER. 
RETURN:  A listening pjb-server or nil.
"
  (let ( netcat-proc
         (netcat-args '())
         )

    (when local-ip 
      (cond 
       ((stringp  local-ip) (push local-ip netcat-args))
       (t (error "Invalid LOCAL-IP, must be a string.")))
      (push "-s" netcat-args)
      );;when

    (cond 
     ((stringp  local-port) (push                   local-port netcat-args))
     ((integerp local-port) (push (number-to-string local-port) netcat-args))
     (t (error "Invalid LOCAL-PORT, must be a string or an integer.")))
    (push "-p"                           netcat-args)

    (setq netcat-process (apply 'start-process "pjb-server$$netcat" nil 
                                "netcat" "-l" netcat-args))
    (set-process-filter netcat-process read-call-back)
    netcat-process)
  );;pjb-server$listen


(defun pjb-server$send   (server data)
  "
DO:     Send DATA to the remote point of the SERVER.
"
  (let (string)
    (cond 
     ((stringp data) (setq string data))
     ((numberp data) (setq string (number-to-string data)))
     (t              (setq string (format "%S" data))))
    (process-send-string server string))
  );;pjb-server$send


(defun pjb-server$close (server)
  "
DO:     Close the remote connection with SERVER.
"
  (delete-process server)
  );;pjb-server$close


(defun pjb-server$status (server)
  "
RETURN: The status of the SERVER.
"
  (process-status server)
);;pjb-server$status

(when nil

  (defun got (server string)
    (let ( (result (eval (car (read-from-string string)))) )
      (pjb-server$send remote (format "%s\n" result)))
    );;got
  (setq remote (pjb-server$listen nil 15364 (function got)))
  (pjb-server$send remote "Howdy\n")
  (process-status remote)
  (pjb-server$status remote)

  (load "pjb-server")
  (defun got (server string)
    (insert string))
  (setq remote (pjb-server$open "localhost" 15364  (function got)))
  (pjb-server$send remote "Hello\n")
  (pjb-server$close remote)
  (pjb-server$send remote "(defun fesf (x y) (pjb-server$send myserv (format \"%S\n\" (* y x) )))")
  (pjb-server$send remote "(setq myserv SERVER)")
  (pjb-server$send remote "(fesf 2 9)")
  (pjb-server$status remote)
  );;when nil


(defvar pjb-server-process nil)
(defvar pjb-server-clients nil)
(defvar pjb-server-program nil)

(defun pjb-server-buffer-done (buffer &optional arg))
(defun server-log (ctrlstring &rest args))


(defun pjb-server-start (&optional leave-dead)
  "Allow this Emacs process to be a server for client processes.
This starts a server communications subprocess through which
client \"editors\" can send your editing commands to this Emacs job.
To use the server, set up the program `emacsclient' in the
Emacs distribution as your standard \"editor\".

Prefix arg means just kill any existing server communications subprocess."
  (interactive "P")
  ;; kill it dead!
  (if pjb-server-process
    (progn
      (set-process-sentinel pjb-server-process nil) 
      (condition-case () (delete-process pjb-server-process) (error nil))))
  ;; Delete the socket files made by previous server invocations.
  (let* ((sysname (system-name))
         (dot-index (string-match "\\." sysname)))
    (condition-case ()
        (delete-file (format "~/.emacs-server-%s" sysname))
      (error nil))
    (condition-case ()
        (delete-file (format "/tmp/esrv%d-%s" (user-uid) sysname))
      (error nil))
    ;; In case the server file name was made with a domainless hostname,
    ;; try deleting that name too.
    (if dot-index
      (let ((shortname (substring sysname 0 dot-index)))
        (condition-case ()
            (delete-file (format "~/.emacs-server-%s" shortname))
          (error nil))
        (condition-case ()
            (delete-file (format "/tmp/esrv%d-%s" (user-uid) shortname))
          (error nil)))))
  ;; If this Emacs already had a server, clear out associated status.
  (while pjb-server-clients
    (let ((buffer (nth 1 (car pjb-server-clients))))
      (pjb-server-buffer-done buffer)))
  (if leave-dead
    nil
    (if pjb-server-process
      (server-log "Restarting server"))
    ;; Using a pty is wasteful, and the separate session causes
    ;; annoyance sometimes (some systems kill idle sessions).
    (let ((process-connection-type nil))
      (setq pjb-server-process (start-process "server" nil pjb-server-program)))
    (set-process-sentinel pjb-server-process 'server-sentinel)
    (set-process-filter pjb-server-process 'pjb-server-process-filter)
    ;; We must receive file names without being decoded.  Those are
    ;; decoded by pjb-server-process-filter accoding to
    ;; file-name-coding-system.
    (set-process-coding-system pjb-server-process 'raw-text 'raw-text)
    (compiletime-cond
     ((< emacs-major-version 22) (process-kill-without-query pjb-server-process))
     (t (set-process-query-on-exit-flag pjb-server-process nil)))))


(defun pjb-server-stop ()
  (interactive)
  (dolist (process (process-list))
    (when (string= "server" (process-name process))
      (delete-process process))));;pjb-server-stop



;;;; pjb-server.el                    --                     --          ;;;;
