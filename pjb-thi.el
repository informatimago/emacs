;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-thi.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Toggle header/implementation buffers for C-like languages.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pbourguignon@dxo.com>
;;;;MODIFICATIONS
;;;;    2012-11-28 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2012 - 2012
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


(defun invert-alist (alist)
  "Returns a new a-list mapping values to lists of keys."
  ;; Note: elisp assoc uses equal.
  (let ((new '()))
    (dolist (entry alist)
      (let ((key (car entry)))
	(dolist (value  (if (listp (cdr entry))
			    (cdr entry)
			    (list (cdr entry))))
          (let ((nentry (assoc value new)))
            (if nentry
                (pushnew key (cdr nentry) :test (function equal))
                (setf new (acons value (list key) new)))))))
    (mapcar (lambda (entry) (cons (car entry) (nreverse (cdr entry))))
            new)))

(defparameter *implementation/header-map*
  '(("m"    "h")
    ("c"    "h")
    ("mm"   "hh"  "h")
    ("cc"   "hh"  "h")
    ("cpp"  "hpp" "h")
    ("c++"  "h++" "h"))
  "A map of implementation file types to the corresponding header file types.
The order matters, since invert-alist keeps the same order for the couples (value key).")

(defparameter *header/implementation-map* (invert-alist *implementation/header-map*)
  "A map of header file types to the corresponding implementation file types.")


(defun change-file-type (path type)
  (format "%s.%s" (file-name-sans-extension path) type))


(defun toggle-header/implementation ()
  (interactive)
  (let* ((fname (buffer-file-name (current-buffer)))
	 (ftype (file-name-extension fname))
	 (other-extensions (or (cdr (assoc ftype *implementation/header-map*))
			       (cdr (assoc ftype *header/implementation-map*)))))
    (if other-extensions
	(loop
	   named toggle
	   for newext in other-extensions
	   for newfile = (change-file-type fname newext)
	   for buffer = (find-buffer-visiting newfile)
	   do (cond
		(buffer
		 (switch-to-buffer buffer)
		 (return-from toggle))
		((file-exists-p newfile)
		 (find-file newfile)
		 (return-from toggle)))
	   finally (switch-to-buffer (get-buffer-create
				      (file-name-nondirectory
				       (change-file-type fname (first other-extensions))))))
	(error "File type not known (update `*implementation/header-map*')."))))

(global-set-key (kbd "C-c C-SPC") 'toggle-header/implementation)

(provide 'pjb-thi)
;;;; THE END ;;;;

