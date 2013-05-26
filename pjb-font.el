;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-font.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Font stuff.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-11-15 <PJB> Created. Extracted code from ~/.emacs.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2011
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
;;;;**************************************************************************
(require 'cl)
(require 'devices nil t)
(require 'font nil t)
(require 'pjb-list)

(defun font-exists-p (pattern)
  (unless (eq window-system 'x)
    (error "font-exists-p works only on X"))
  (zerop
   (parse-integer
    (shell-command-to-string
     (format "xlsfonts -fn %S 2>&1|awk 'BEGIN{r=0;} /xlsfonts: pattern .* unmatched/{r=1;} END{printf \"%%d\",r;}'" pattern)))))





(defun font-canonical-to-pixel (canon &optional device)
  (let ((pix-width (float (or (cond ((fboundp 'device-pixel-width)
                                     (device-pixel-width device))
                                    ((fboundp 'display-pixel-width)
                                     (display-pixel-width) device)
                                    (t nil)) 1024)))
        (mm-width (float (or (cond ((fboundp 'device-mm-width)
                                    (device-mm-width device))
                                   ((fboundp 'display-mm-width)
                                    (display-mm-width) device)
                                   (t nil)) 293))))
    (/ canon (/ pix-width mm-width) (/ 25.4 72.0))))


(defun get-font-size-in-pixel (font &optional device)
  "
RETURN: The font height in pixel.
"
  (cond ((and (fboundp 'font-size)
              (fboundp 'font-create-object)
              (fboundp 'font-spatial-to-canonical))
         (let ((fs (font-size (font-create-object font))))
           (if (numberp fs) 
               fs
               (font-canonical-to-pixel
                (font-spatial-to-canonical fs device) device))))
        (error "How do I compute the font size in pixel for font %S?" font)))


(defun create-new-fontset (fontset-spec &optional style-variant noerror)
  (handler-case
      (create-fontset-from-fontset-spec fontset-spec style-variant noerror)
    (error ())))


(defun split-font-pattern (pattern)
  "Splits a X font pattern into a plist."
  (let ((parts (split-string pattern "-"))
        (plist nil))
    ;; (unless (and (elt parts 7) (string-match "^[0-9]" (elt parts  7)))
    ;;   (message ".EMACS: font=%S\nparts=%S\n" pattern parts))
    (loop
     for item in (cdr parts)
     for key  in '(:foundry
                   :family :weight :slant :width :style
                   :pixel-size :point-size :resolution-x :resolution-y
                   :spacing :average-width :registry :encoding)
     collect key
     collect item)))


(defun* make-font-pattern (&key foundry family weight slant width
                                style pixel-size point-size
                                resolution-x resolution-y spacing
                                average-width registry encoding
                                (defaults "-*-*-*-*-*-*-*-*-*-*-*-*-*-*"))
  "Builds a X font pattern from the keyword arguments
DEFAULTS:  either a X font pattern (string) or a plist used as default
           when the corresponding keyword is not given.
EXAMPLE:   Changing the size of a font:
          (make-font-pattern
               :defaults \"-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*\"
               :pixel-size 12)"
  (when (stringp defaults)
    (setf defaults (split-font-pattern defaults)))
  (macrolet ((field (name)
               `(or ,name (getf defaults ,(intern (format ":%s" name))) "*")))
    (format "-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s"
            (field foundry) (field family) (field weight) (field slant)
            (field width) (field style) (field pixel-size) (field point-size)
            (field resolution-x) (field resolution-y) (field spacing)
            (field average-width) (field registry) (field encoding))))


(defun* get-font-parts (pattern &key foundry family weight slant width
                                style pixel-size point-size
                                resolution-x resolution-y spacing
                                average-width registry encoding)
  "
RETURN: A list of unique property lists containing the selected keys
        with all unique tuples in the fonts selected by the pattern.
"
  (let ((parts nil)
        (plist)
        (res nil))
    (dolist (font (split-string
                   (shell-command-to-string
                    (format "xlsfonts -fn '%s'|sort -u" pattern)) "\n"))
      (pushnew (split-font-pattern font) res :test (function equalp)))
    res))



(defun get-independant-font-parts (pattern &key foundry family weight
                                             slant width style
                                             pixel-size point-size
                                             resolution-x resolution-y
                                             spacing average-width
                                             registry encoding
                                             registry-encoding)
  "
RETURN:  An a-list with entries for the selected keywords,
         each being the list of unique values for the corresponding
         field in all fonts selected by the pattern.
EXAMPLE: All families in the Adobe foundry:
         (get-independant-font-parts (make-font-pattern :foundry \"adobe\") 
                                     :family t)
"
  (let ((p-foundry nil)      (p-family nil)
        (p-weight nil)       (p-slant nil)
        (p-width nil)        (p-style nil)
        (p-pixel-size nil)   (p-point-size nil)
        (p-resolution-x nil) (p-resolution-y nil)
        (p-spacing nil)      (p-average-width nil)
        (p-registry nil)     (p-encoding nil)
        (p-registry-encoding nil)
        (parts nil)
        (res nil)
        (test (function string-equal)))
    (dolist (font (split-string
                   (shell-command-to-string
                    (format "xlsfonts -fn '%s'|sort -u" pattern)) "\n"))
      (setf parts (split-string font "-"))
      (when foundry
        (pushnew (elt parts  1) p-foundry       :test test))
      (when family
        (pushnew (elt parts  2) p-family        :test test))
      (when weight
        (pushnew (elt parts  3) p-weight        :test test))
      (when slant
        (pushnew (elt parts  4) p-slant         :test test))
      (when width
        (pushnew (elt parts  5) p-width         :test test))
      (when style
        (pushnew (elt parts  6) p-style         :test test))
      (when pixel-size
        (pushnew (elt parts  7) p-pixel-size    :test test))
      (when point-size
        (pushnew (elt parts  8) p-point-size    :test test))
      (when resolution-x
        (pushnew (elt parts  9) p-resolution-x  :test test))
      (when resolution-y
        (pushnew (elt parts 10) p-resolution-y  :test test))
      (when spacing
        (pushnew (elt parts 11) p-spacing       :test test))
      (when average-width
        (pushnew (elt parts 12) p-average-width :test test))
      (when registry
        (pushnew (elt parts 13) p-registry      :test test))
      (when encoding
        (pushnew (elt parts 14) p-encoding      :test test))
      (when registry-encoding
        (pushnew (format "%s-%s" (elt parts 13) (elt parts 14))
                 p-registry-encoding      :test test)))
    (when registry-encoding
      (push (cons :registry-encoding      p-registry-encoding     ) res))
    (when encoding      (push (cons :encoding      p-encoding     ) res))
    (when registry      (push (cons :registry      p-registry     ) res))
    (when average-width (push (cons :average-width p-average-width) res))
    (when spacing       (push (cons :spacing       p-spacing      ) res))
    (when resolution-y  (push (cons :resolution-y  p-resolution-y ) res))
    (when resolution-x  (push (cons :resolution-x  p-resolution-x ) res))
    (when point-size    (push (cons :point-size    p-point-size   ) res))
    (when pixel-size    (push (cons :pixel-size    p-pixel-size   ) res))
    (when style         (push (cons :style         p-style        ) res))
    (when width         (push (cons :width         p-width        ) res))
    (when slant         (push (cons :slant         p-slant        ) res))
    (when weight        (push (cons :weight        p-weight       ) res))
    (when family        (push (cons :family        p-family       ) res))
    (when foundry       (push (cons :foundry       p-foundry      ) res))
    res))




(defmacro make-my-mac-font-sets (size)
  `(progn
     (create-new-fontset
      ,(format "-*-courier-*-*-*-*-%d-*-*-*-*-*-fontset-courier,
          ascii:-*-courier-*-*-*-*-%d-*-*-*-*-*-*,
latin-iso8859-1:-*-courier-*-*-*-*-%d-*-*-*-*-*-*"
               size size size))
     (create-new-fontset
      ,(format "-apple-monaco-%s--%d-*-*-*-*-*-fontset-monaco,
          ascii:-apple-monaco-%s--%d-%d0-75-75-m-%d0-mac-roman,
latin-iso8859-1:-apple-monaco-%s--%d-%d0-75-75-m-%d0-mac-roman"
               "medium-r-normal" size
               "medium-r-normal" size size size
               "medium-r-normal" size size size))))


;; ---------------------------
;;       select-font
;; ---------------------------
   
(defstruct ftree label children)

(defun build-font-tree (fps &optional label)
  "
FPS:     A property list of font pattern fields (in order).
RETURN:  A tree where the child of each node are labelled with
         the corresponding pattern field.
"
  (when fps
    (setf label (or label "root"))
    (if (car fps)
      (let ((classes (equivalence-classes
                      fps (lambda (a b) (equalp (second a) (second b))))))
        (make-ftree :label label
                    :children (mapcar (lambda (class)
                                        (build-font-tree
                                         (mapcar (function cddr) class)
                                         (second (first class))))
                                      classes)))
      (make-ftree :label label))))


(defun ftree-children-named (font-tree name)
  (car (delete-if (lambda (child) (not (string-equal name (ftree-label child))))
                  (ftree-children font-tree))))

  
(defparameter *font-default-fields*
  ;;'(:spacing "m" :registry "iso8859" :encoding "1"))
  '(:spacing "m" :registry "iso8859"))


(defparameter *font-tree*
  (when (eq window-system 'x)
    (build-font-tree
     (delete-if (lambda (fp)
                  (let ((size (parse-integer (plist-get fp :pixel-size))))
                    (or (null size) (< size 8))))
                (get-font-parts
                 (apply (function make-font-pattern) *font-default-fields*)
                 :family t :weight t :slant t :pixel-size t)))))


(defvar *font-current-node* nil)
(defvar *font-family-history* nil)
(defvar *font-weight-history* nil)
(defvar *font-slant-history* nil)
(defvar *font-pixel-size-history* nil)

(defun select-font (family weight slant pixel-size)
  (interactive
   (list
    (completing-read
     "Family: "
     (mapcar (lambda (child) (cons (ftree-label child) child))
             (ftree-children *font-tree*))
     (lambda (item) (setq *font-current-node* (cdr item))) t nil
     '*font-family-history*)
    (completing-read
     "Weight: "
     (mapcar (lambda (child) (cons (ftree-label child) child))
             (ftree-children *font-current-node*))
     (lambda (item) (setq *font-current-node* (cdr item))) t nil
     '*font-weight-history*)
    (completing-read
     "Slant: "
     (mapcar (lambda (child) (cons (ftree-label child) child))
             (ftree-children *font-current-node*))
     (lambda (item) (setq *font-current-node* (cdr item))) t nil
     '*font-slant-history*)
    (completing-read
     "Pixel-Size: "
     (mapcar (lambda (child) (cons (ftree-label child) child))
             (ftree-children *font-current-node*))
     (lambda (item) (setq *font-current-node* (cdr item))) t nil
     '*font-pixel-size-history*)))
  (set-frame-font (make-font-pattern :family family :weight weight
                                                    :slant slant :pixel-size pixel-size
                                                    :spacing "m"))
  (when (fboundp 'single-frame) (single-frame)))
   


(defvar *default-font* "fixed")
(defun select-default-font ()
  (interactive)
  (set-frame-font *default-font*)
  (when (fboundp 'single-frame) (single-frame)))


(cond
 ((eq window-system 'mac)
  (make-my-mac-font-sets 9)
  (make-my-mac-font-sets 10)
  (make-my-mac-font-sets 12)
  (make-my-mac-font-sets 14))

 ((eq window-system 'x)
  (select-default-font)))


(provide 'pjb-font)
