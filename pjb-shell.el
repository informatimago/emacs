;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               pjb-shell.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    shell/nshell defined as functions instead of advices.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-06-29 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2004 - 2011
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
;;;;****************************************************************************

(require 'shell)
(provide 'pjb-shell)

(defvar original-shell (symbol-function 'shell))

(defun shell-buffer-name (i) (format "%dshell" i))

(defadvice shell (around shell/multiple-buffers first (&optional buffer) activate)
  "Create a new shell when none exist, or switch to the last created one."
  (interactive "P")
  (if buffer
      (progn
        (ad-set-args 0 (list buffer))
        ad-do-it)
      (let ((i 0))
        (while (get-buffer (shell-buffer-name i))
          (setq i (1+ i)))
        (if (= 0 i)
            (progn ;; no shell, let's make the first one
              ad-do-it
              (rename-buffer (shell-buffer-name 0)))
            (progn ;; already have some shells, let's jump to one.
              (switch-to-buffer (shell-buffer-name (1- i))))))))


(defvar *forward-control-c-shells*
  '("ssh$"))

(defun nshell (ask-for-shell-p)
  "Create a new shell."
  (interactive "P")
  (flet ((run-shell ()
           (shell)
           (let ((i 0))
             (while (get-buffer (shell-buffer-name i))
               (setq i (1+ i)))
             (rename-buffer (shell-buffer-name i)))))
    (if ask-for-shell-p
        (let* ((command (read-from-minibuffer
                         "Command: " "/bin/bash" nil nil nil
                         "/bin/bash"))
               (args (split-string command "[ \t]+"))
               (explicit-shell-file-name (first args))
               (pname (file-name-nondirectory explicit-shell-file-name))
               (xargs-name (intern (concat "explicit-" pname "-args"))))
          (eval `(let ((,xargs-name ',(rest args)))
                   (message "pgm=%S args=%S"
                            explicit-shell-file-name
                            ,xargs-name)
                   (run-shell)))
          (message "command=%S f=%S member=%S"
                   (first args) *forward-control-c-shells*
                    (member* (first args) *forward-control-c-shells*
                       :test (lambda (string regexp) (string-match regexp string))))
          (when (member* (first args) *forward-control-c-shells*
                       :test (lambda (string regexp) (string-match regexp string)))
            (make-keymap-local)
            (local-set-key (kbd "C-c C-c")
                           (lambda ()
                              (interactive)
                              (comint-send-string
                               (get-buffer-process (current-buffer))
                               (format "%c" 3))))))
        (run-shell))))



;;;; THE END ;;;;

