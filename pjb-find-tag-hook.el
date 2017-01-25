;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-find-tag-hook.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    find-tag hook
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-08-20 <PJB> Extracted from ~/rc/emacs-common.el
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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
(require 'pjb-cl)

(defstruct location buffer file-name line-number line-contents)


(defun %find-tag-locations-in-order ()
  (message "%%find-tag-locations-in-order enters tagname=%S next-p=%S regexp-p=%S"
           tagname next-p regexp-p)
  (loop
     with locations = '()
     for (buffer err) = (cl:ignore-errors (find-tag-noselect tagname nil regexp-p))
     then (cl:ignore-errors (find-tag-noselect tagname t regexp-p))
     initially  (message "%%find-tag-locations-in-order initially")
     do (message "buffer = %S" buffer)
     while buffer
     collect (with-current-buffer buffer
               (make-location
                :buffer (current-buffer)
                :file-name (buffer-file-name (current-buffer))
                :line-number (count-lines (point-min) (point-at-bol))
                :line-contents (buffer-substring-no-properties
                                (point-at-bol) (point-at-eol))))
     finally (message "%%find-tag-locations-in-order exists %S" locations) (return (values locations err))))


(defun pjb-find-tag-meat ()
  (message "pjb-find-tag-meat enters")
  (unless next-p
    (message "pjb-find-tag-meat 1")
    (handler-case
        (multiple-value-bind (locations error) (%find-tag-locations-in-order)
          (message  " locations (2) = %S" locations)
          (if locations
              (progn
                (message "pjb-find-tag-meat 2")
                (save-excursion
                 (message "pjb-find-tag-meat 3")
                 (switch-to-buffer-other-window (get-buffer-create
                                                 (format "*tags on %s*" tagname)))
                 (erase-buffer)
                 (compilation-mode 1)
                 (message "pjb-find-tag-meat 4")
                 (dolist (loc locations)
                   (insert (format "%s:%s %s\n"
                                   (location-file-name loc)
                                   (location-line-number loc)
                                   (location-line-contents loc))))
                 (message "pjb-find-tag-meat 5"))
                (message "pjb-find-tag-meat 6")
                (message "pjb-find-tag-meat exits %S" (location-buffer (first locations)))
                (location-buffer (first locations)))
              (when error
                (signal (car error) (cdr error)))))
      (error (err)
        (message "pjb-find-tag-meat 7")
        (message "%s" err)))))


;; (add-hook 'find-tag-hook (function pjb-find-tag-meat))
;; (setq find-tag-hook nil)

;;;; THE END;;;;
