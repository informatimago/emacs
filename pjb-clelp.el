;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-clelp.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Helpers for CL development.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2020-10-21 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2020 - 2020
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
(require 'slime)
(require 'pjb-cl)

(defun* pjb-find-file-not-found/cl-pathname ()
  "Resolve the `buffer-file-name' thru common-lisp via slime,
and insert the file contents when found.

This allows to find-file on CL logical-pathnames,
as displayed eg. in ccl error messages.

    Compiler warnings for \"home:foo;bar;baz.lisp.newest\" :

You can then C-x C-f and yank:
   home:foo;bar;baz.lisp.newest
   \"home:foo;bar;baz.lisp.newest\"
even after a prefix path.
"
  (with-current-buffer (current-buffer)
    (when (slime-connected-p)
      (flet ((clpath (path)
               (slime-eval
                `(cl:or (cl:ignore-errors
                         (cl:namestring
                          (cl:truename
                           (cl:pathname ,path))))
                        (cl:ignore-errors
                         (cl:namestring
                          (cl:truename
                           (cl:make-pathname :version nil :defaults (cl:pathname ,path)))))))))
        (let ((path (clpath buffer-file-name)))
          (unless (and path (file-exists-p path))
            (when (or (string-match "^ *\"\\([^/]+\\)\"$"     buffer-file-name)
                      (string-match "^/.*/ *\"\\([^/]+\\)\"$" buffer-file-name)
                      (string-match "^/.*/ *\\([^/]+\\)$"     buffer-file-name))
              (setf path (clpath  (match-string 1 buffer-file-name)))))
          (when (and path (file-exists-p path))
            (handler-case
                (let ((inhibit-read-only t))
                  (insert-file-contents path)
                  (set-visited-file-name path)
                  (message "buffer-file-name = %S" path)
                  t)
              (file-error ()
                nil))))))))

(add-hook 'find-file-not-found-functions 'pjb-find-file-not-found/cl-pathname)

(provide 'pjb-clelp)
;;;; THE END ;;;;
