;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-image-minor-mode.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This minor mode adds font-locking for image links that are
;;;;    replaced by the image in the buffers.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-08-07 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2021 - 2021
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
(require 'pjb-cl)

(defparameter *pjb-image--re*
  '("\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]"
    (url . 1)
    (text . 2)))

(defvar *pjb-image--links* '())

(defun pjb-image-get-image-at-url (url)
  ;; for now, only pathnames are accepted
  (create-image url nil nil :max-width 510))

(defun pjb-image--search-image-links (limit)
  (when (re-search-forward (car *pjb-image--re*) limit t)
    (let ((start (match-beginning 0))
          (end   (match-end       0))
          (url   (match-string (cdr (assoc 'url  (cdr *pjb-image--re*)))))
          (text  (match-string (cdr (assoc 'text (cdr *pjb-image--re*))))))
      (when (display-images-p)
        (push (cons start end) *pjb-image--links*)
        (set-text-properties start end '(invisible t))
        (let ((image (pjb-image-get-image-at-url url)))
          (put-image image start))))))

(defun pjb-image--add-font-lock ()
  (font-lock-add-keywords nil '((pjb-image--search-image-links))))

(defun pjb-image--remove-font-lock ()
  (font-lock-remove-keywords nil '((pjb-image--search-image-links))))

(defvar pjb-image-minor-mode nil)

(defun pjb-image-minor--enable  ()
  (make-variable-buffer-local '*pjb-image--links*)
  (add-to-list 'minor-mode-alist '(pjb-image-minor-mode " Img"))
  (pjb-image--add-font-lock)
  (setf pjb-image-minor-mode t))

(defun pjb-image-minor--disable ()
  (pjb-image--remove-font-lock)
  (dolist (range *pjb-image--links* (setf *pjb-image--links* '()))
    (remove-text-properties (car range) (cdr range) '(invisble t)))
  (remove-images (point-min) (point-max))
  (setf pjb-image-minor-mode nil))

(defun pjb-image-minor-mode (&optional arg)
  (interactive (list (or current-prefix-arg 'toggle)))
  (let ((enable (if (eq arg 'toggle)
                    (not pjb-image-minor-mode)
                    (> (prefix-numeric-value arg) 0))))
    (if enable
        (pjb-image-minor--enable)
        (pjb-image-minor--disable))))

;;;###autoload
(defcustom pjb-image-minor-mode nil
  "Toggle then pjb-image-minor-mode; this mode displays in the buffer
the images that are linked such as [[foo.png][Example Foo Image]]
Setting this variable directly does not take effect;
use either \\[customize] or the function `pjb-image-minor-mode'."
  :set 'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :version "27.0"
  :type    'boolean
  :group   'pjb-image-minor-mode
  :require 'pjb-image-minor-mode)
  
(provide 'pjb-image-minor-mode)
