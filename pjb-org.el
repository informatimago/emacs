;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;;;**************************************************************************
;;;;FILE:               pjb-org.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    org-mode utilities:
;;;;
;;;;    - pjb-org-split-big-blocks splits out big blocks to separate
;;;;      files that are then included.
;;;;
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-10-05 <PJB> Created.
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

(defun pjb-org-enough-namestring (path base)
  "Compute a relative path to go to the `PATH' from a `BASE' directory.
If `PATH' is an absolute pathname,
then it is reduced to a pathname relative to `BASE'
else it's returned as is."
  (let ((separator "/")
        (here      ".")
        (back      ".."))
    (if (string= (subseq path 0 (min 1 (length path))) separator)
        (let* ((apath  (split-string path separator t))
               (abase  (split-string base separator t))

               (i      (mismatch apath abase :test (function string=))))
          (if i
              (mapconcat (function identity)
                         (let ((new (or (nthcdr i apath) '(""))))
                           (dotimes (n (- (length abase) i) new)
                             (push back new)))
                         separator)
              (concat here separator)))
        path)))


(defmacro generate-org-element-reader (&rest fields)
  `(progn
     ,@(mapcar (lambda (field)
		         `(defun ,(intern (format "org-element-%s" field)) (element)
		            (getf (second element) ,(intern (format ":%s" field)))))
	           fields)))

(generate-org-element-reader begin end contents-begin contents-end
	                         name language switches parameters
			                 post-blank post-affiliated parent value switches number-lines
			                 preserve-indent retain-labels use-labels label-fmt)

(defun org-element-type (element)
  (first element))

(defun org-element-subtype (element)
  (org-element-language element))

(defun pjb-org-make-document-directory (&optional buffer-or-path)
  "Creates a directory named ${buffer-file-name}-inc
If it already exist, then signal a warning.
If it's a file, then signal an error.
Return the path of the new directory."
  (cond
    ((null buffer-or-path)
     (pjb-org-make-document-directory (current-buffer)))
    ((bufferp buffer-or-path)
     (pjb-org-make-document-directory (buffer-file-name buffer-or-path)))
    ((stringp buffer-or-path)
     (let ((path (concat (file-name-directory buffer-or-path)
			             (file-name-base buffer-or-path)
			             "-inc")))
       (cond
	     ((file-directory-p path)
	      path)
	     ((file-exists-p path)
	      (error "file %S already exists" path))
	     (t
	      (make-directory path t)
	      path))))
    (t
     (error "Invalid argument, expected a buffer or a path (string), got a %S: %S"
	        (type-of buffer-or-path) buffer-or-path))))

;; (pjb-org-make-document-directory "/tmp/bar")

(defun pjb-org-random-uuid ()
  "Return a UUID."
  ;; code here by Christopher Wellons, 2011-11-18.
  ;; and editted Hideki Saito further to generate all valid variants for "N" in xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx format.
  (let ((uuidata (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                              (user-uid)
                              (emacs-pid)
                              (system-name)
                              (user-full-name)
                              (current-time)
                              (emacs-uptime)
                              (garbage-collect)
                              (buffer-string)
                              (random)
                              (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
            (substring uuidata 0 8)
            (substring uuidata 8 12)
            (substring uuidata 13 16)
            (format "%x" (+ 8 (random 4)))
            (substring uuidata 17 20)
            (substring uuidata 20 32))))

(defun pjb-org-save-element (element name path)
  (let ((text (buffer-substring (org-element-begin element)
				                (org-element-end element))))
    (with-temp-buffer nil
      (insert  "#### -*- mode:org;coding:utf-8;lexical-binding:t -*-\n\n")
      (insert (format  "## the %s block\n\n" name))
      (insert "\n" text "\n\n")
      (write-file path nil))))

(defun pjb-org-insert-file (path type &optional subtype)
  "Inserts a #+INCLUDE tag."
  (insert "\n")
  (insert (format "#+INCLUDE: %S %s" path type ))
  (when subtype (insert (format " %s" subtype)))
  (insert "\n\n"))

(defun pjb-org-split-big-blocks (&optional maxsize)
  (interactive)
  (let ((maxsize (or maxsize 2048))
	    (path (buffer-file-name)))
    (org-block-map
     (lambda ()
       (let* ((element (org-element-at-point))
	          (size (- (or (org-element-contents-end element)
			               (org-element-end element))
		               (or (org-element-contents-begin element)
			               (org-element-begin element)))))
	     (message "pjb-org-split-big-blocks point = %s ; name = %S ; subtype = %S" (point) (org-element-name element) (org-element-subtype element))
	     (when (< maxsize size)
	       (let* ((dir-path (pjb-org-make-document-directory path))
				  ;; not yet: we need to clean up the name.
		          ;; (ele-name (let ((id   (pjb-org-random-uuid))
                  ;;                 (name (org-element-name element)))
                  ;;             (if name
                  ;;                 (format "%s-%s.org" id name)
                  ;;                 (format "%s.org" id))))
		          (ele-name (pjb-org-random-uuid))
		          (ele-path (concat (file-name-as-directory dir-path)
                                    (format "%s.org" ele-name))))
	         (pjb-org-save-element element ele-name ele-path)
	         (delete-region (org-element-begin element) (org-element-end element))
	         (pjb-org-insert-file (pjb-org-enough-namestring ele-path (file-name-directory path))
                                  (org-element-type element)
                                  (org-element-subtype element)))))))))


(provide 'pjb-org)

;;;; THE END ;;;;

