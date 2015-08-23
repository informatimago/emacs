;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               pjb-cl-face.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines font-lock faces for COMMON-LISP symbols.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-01-26 <PJB> Created.
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
(require 'font-lock)
(require 'custom)
(require 'lisp-mode)
(require 'cc-engine)
(require 'pjb-cl)
(require 'pjb-sources)


;; font-lock-builtin-face
;; font-lock-comment-face
;; font-lock-constant-face
;; font-lock-doc-face
;; font-lock-function-name-face
;; font-lock-keyword-face
;; font-lock-string-face
;; font-lock-type-face
;; font-lock-variable-name-face
;; font-lock-warning-face


;;; (mapcar
;;;  (lambda (kind) 
;;;    (insert (format "%S\n"
;;;              (list kind 
;;;                    (intern
;;;                     (concatenate 'string
;;;                       "font-lock-cl-" 
;;;                       (replace-regexp-in-string 
;;;                        " " "-" (string-downcase kind))
;;;                       "-face"))
;;;                     '(spec tty glight gdark clight cdark default)))))
;;;  (delete-duplicates (mapcar (function second) *common-lisp-symbols*)
;;;                     :test (function string=)))


(defmacro ident-list (&rest args)
  `(list ,@(mapcar (lambda (x) (list 'quote x)) args))
  ) ;;ident-list


(defmacro spec (tty gray-light gray-dark color-light color-dark default)
  `(ident-list
    (((type tty) (class color)) ,tty)
    (((class grayscale) (background light)) ,gray-light)
    (((class grayscale) (background dark)) ,gray-dark) 
    (((class color) (background light)) ,color-light) 
    (((class color) (background dark)) ,color-dark) 
    (t ,default))
  ) ;;spec


;; :family :width :height 
;; :weight :slant :underline :overline :strike-through :box
;; :foreground :background :stipple :inverse-video :inherit


;; cyan    blue
;; magenta red
;; yellow  green
;; white   black


(defmacro kf-kind (x) `(first  ,x))
(defmacro kf-lock (x) `(second ,x))
(defmacro kf-face (x) `(third  ,x))
(defmacro kf-spec (x) `(fourth ,x))

(defparameter *kind-to-face-map*
  `(
    ;;     ("Comment"
    ;;      (;;pjb-cl$find-comment
    ;;       ("\\(\\(;.*$\\)\\|\\(#|\\(\n\\|.\\)*?|#\\)\\)"
    ;;       (0 font-lock-cl-comment-face t))
    ;;      font-lock-cl-comment-face
    ;;      ,(spec (:foreground "green")
    ;;             (:foreground "DimGray"   :slant italic)
    ;;             (:foreground "LightGray" :slant italic)
    ;;             (:foreground "coral"     :slant italic)
    ;;             (:foreground "coral"     :slant italic)
    ;;             ()))
    ;;     ("String"
    ;;      (pjb-cl$find-string
    ;;       (0 font-lock-cl-string-face t))
    ;;      font-lock-cl-string-face
    ;;      ,(spec (:foreground "green")
    ;;             (:foreground "DimGray"   :slant italic)
    ;;             (:foreground "LightGray" :slant italic)
    ;;             (:foreground "SeaGreen3" :slant italic)
    ;;             (:foreground "SeaGreen3" :slant italic)
    ;;             ()))

    ("Warning"
     nil
     font-lock-cl-warning-face
     ,(spec (:underline nil :bold t  :foreground "red"       :bold t)
            (:underline nil :bold t  :foreground "DimGray"   :bold t)
            (:underline nil :bold t  :foreground "LightGray" :bold t)
            (:underline nil :bold t  :foreground "Red"       :bold t)
            (:underline nil :bold t  :foreground "Red"       :bold t)
            (:underline nil :bold t )))
    ("Variable"
     nil
     font-lock-cl-variable-face
     ,(spec (:underline nil :bold t  :foreground "yellow")
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "goldenrod")
            (:underline nil :bold t  :foreground "goldenrod")
            (:underline nil :bold t )))
    ("Constant Variable"
     nil
     font-lock-cl-constant-variable-face
     ,(spec (:underline nil :bold t  :foreground "yellow")
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "gold" :bold t)
            (:underline nil :bold t  :foreground "gold" :bold t)
            (:underline nil :bold t )))
    ("Symbol"
     nil
     font-lock-cl-symbol-face
     ,(spec (:underline nil :bold t  :foreground "green")
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "ForestGreen")
            (:underline nil :bold t  :foreground "LimeGreen")
            (:underline nil :bold t )))
    ;;     ("Pipe-Symbol"
    ;;      (pjb-cl$find-symbol
    ;;       (0 font-lock-cl-symbol-face t))
    ;;      font-lock-cl-symbol-face
    ;;      ,(spec (:underline nil :bold t  :foreground "green")
    ;;             (:underline nil :bold t  :foreground "DimGray")
    ;;             (:underline nil :bold t  :foreground "LightGray")
    ;;             (:underline nil :bold t  :foreground "ForestGreen")
    ;;             (:underline nil :bold t  :foreground "LimeGreen")
    ;;             (:underline nil :bold t )))
    ("System Class"
     nil
     font-lock-cl-system-class-face
     ,(spec (:underline nil :bold t  :foreground "green")
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "LightSeaGreen" :bold t)
            (:underline nil :bold t  :foreground "LightSeaGreen" :bold t)
            (:underline nil :bold t )))
    ("Class"
     nil
     font-lock-cl-class-face
     ,(spec (:underline nil :bold t  :foreground "green")
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "LightSeaGreen")
            (:underline nil :bold t  :foreground "LightSeaGreen")
            (:underline nil :bold t )))
    ("Condition Type"
     nil
     font-lock-cl-condition-type-face
     ,(spec (:underline nil :bold t  :foreground "green")
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "DarkKhaki")
            (:underline nil :bold t  :foreground "GreenYellow")
            (:underline nil :bold t )))
    ("Type"
     nil
     font-lock-cl-type-face
     ,(spec (:underline nil :bold t  :foreground "green")
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "MediumSeaGreen")
            (:underline nil :bold t  :foreground "MediumSeaGreen")
            (:underline nil :bold t )))
    ("Type Specifier"
     nil
     font-lock-cl-type-specifier-face
     ,(spec (:underline nil :bold t  :foreground "green")
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "MediumSeaGreen")
            (:underline nil :bold t  :foreground "MediumSeaGreen")
            (:underline nil :bold t )))
    ("Declaration"
     nil
     font-lock-cl-declaration-face
     ,(spec (:underline nil :bold t  :foreground "red")
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "DeepPink" :bold t)
            (:underline nil :bold t  :foreground "DeepPink" :bold t)
            (:underline nil :bold t )))
    ("Restart"
     nil
     font-lock-cl-restart-face
     ,(spec (:underline nil :bold t  :foreground "magenta")
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "purple")
            (:underline nil :bold t  :foreground "purple")
            (:underline nil :bold t )))
    ("Special Operator"
     nil
     font-lock-cl-special-operator-face
     ,(spec (:underline nil :bold t  :foreground "blue" :bold t)
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "BlueViolet")
            (:underline nil :bold t  :foreground "BlueViolet")
            (:underline nil :bold t )))
    ("Special Form"
     nil
     font-lock-cl-special-form-face
     ,(spec (:underline nil :bold t  :foreground "blue" :bold t)
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "BlueViolet")
            (:underline nil :bold t  :foreground "BlueViolet")
            (:underline nil :bold t )))
    ("Local Macro"
     nil
     font-lock-cl-local-macro-face
     ,(spec (:underline nil :bold t  :foreground "blue" :bold t)
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "orchid")
            (:underline nil :bold t  :foreground "plum")
            (:underline nil :bold t )))
    ("Macro"
     nil
     font-lock-cl-macro-face
     ,(spec (:underline nil :bold t  :foreground "blue" :bold t)
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "maroon")
            (:underline nil :bold t  :foreground "PaleVioletRed")
            (:underline nil :bold t )))
    ("Accessor"
     nil
     font-lock-cl-accessor-face
     ,(spec (:underline nil :bold t  :foreground "blue")
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "CadetBlue")
            (:underline nil :bold t  :foreground "DarkTurquoise")
            (:underline nil :bold t )))
    ("Local Function"
     nil
     font-lock-cl-local-function-face
     ,(spec (:underline nil :bold t  :foreground "blue")
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "SlateBlue")
            (:underline nil :bold t  :foreground "LightBlue")
            (:underline nil :bold t )))
    ("Standard Generic Function"
     nil
     font-lock-cl-standard-generic-function-face
     ,(spec (:underline nil :bold t  :foreground "blue")
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "MediumBlue"     :bold t)
            (:underline nil :bold t  :foreground "CornflowerBlue" :bold t)
            (:underline nil :bold t )))
    ("Function"
     nil
     font-lock-cl-function-face
     ,(spec (:underline nil :bold t  :foreground "blue")
            (:underline nil :bold t  :foreground "DimGray")
            (:underline nil :bold t  :foreground "LightGray")
            (:underline nil :bold t  :foreground "MediumBlue")
            (:underline nil :bold t  :foreground "CornflowerBlue")
            (:underline nil :bold t )))
    ("Keyword" ;; from the KEYWORD package
     ("\\(\\<:\\sw\\sw+\\>\\)" 
      (0 font-lock-cl-keyword-face t))
     font-lock-cl-keyword-face
     ,(spec (:foreground "magenta")
            (:foreground "DimGray"   :bold t)
            (:foreground "LightGray" :bold t)
            (:foreground "Magenta")
            (:foreground "Magenta")
            ()))
    ) ;;*kind-to-face-map*
  )   ;;*kind-to-face-map*




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (defvar pjb-cl$*state*)
;;; (make-local-variable 'pjb-cl$*state*)
;;; (defmacro clfss-modified (x) `(first  ,x))
;;; (defmacro clfss-strings  (x) `(second ,x))
;;; (defmacro clfss-symbols  (x) `(third  ,x))
;;; (defmacro clfss-comments (x) `(fourth ,x))


;;; (defun pjb-cl$rla (list)
;;;   (make-array (list (length list)) :initial-contents (nreverse list))
;;;   );;pjb-cl$rla


;;; (defun pjb-cl$update-cache ()
;;;   (when (or (not (boundp 'pjb-cl$*state*)) (null pjb-cl$*state*))
;;;     (setf pjb-cl$*state* (list 0 nil nil nil)))
;;;   (when (/= (clfss-modified pjb-cl$*state*) (buffer-modified-tick))
;;;     (save-excursion
;;;       (goto-char (point-min))
;;;       (do ((state    :out)
;;;            (level    0)
;;;            (strings  '())
;;;            (symbols  '())
;;;            (comments '())
;;;            (start))
;;;           ((>= (point) (point-max))
;;;            (setf (clfss-modified pjb-cl$*state*) (buffer-modified-tick)
;;;                  (clfss-strings  pjb-cl$*state*) (pjb-cl$rla strings)
;;;                  (clfss-symbols  pjb-cl$*state*) (pjb-cl$rla symbols)
;;;                  (clfss-comments pjb-cl$*state*) (pjb-cl$rla comments)))
;;;         (case state
;;;           ((:out)
;;;            (cond
;;;             ((looking-at "\\\\."))
;;;             ((looking-at ";.*$")
;;;              (push (cons (match-beginning 0) (match-end 0)) comments))
;;;             ((looking-at "\\(\"\\([^\"]\\|\\\\\"\\)*?\"\\)")
;;;              (push (cons (match-beginning 0) (match-end 0)) strings))
;;;             ((looking-at "\\(|\\([^|]\\|\\\\.\\)*?|\\)")
;;;              (push (cons (match-beginning 0) (match-end 0)) symbols))
;;;             ((looking-at "#|")
;;;              (setf state :sharp-comment level (1+ level)
;;;                    start (match-beginning 0)))
;;;             ((looking-at "\\([^;#|\"\\\\]\\|#[^;#|\"\\\\]\\)*"))
;;;             ((looking-at "."))))
;;;           ((:sharp-comment)
;;;            (cond
;;;             ((looking-at "#|")
;;;              (setf state :sharp-comment  level (1+ level))
;;;              (goto-char (match-end 0)))
;;;             ((looking-at "|#")
;;;              (decf level)
;;;              (when (= 0 level) 
;;;                (push (cons start (match-end 0)) comments)
;;;                (setf state :out))
;;;              (goto-char (match-end 0)))
;;;             ((looking-at "\\([^#|]*\\|#[^#|]\\||[^#|]\\)*"))
;;;             ((looking-at ".")))))
;;;         (goto-char (match-end 0)))))
;;;   );;pjb-cl$update-cache
   


;; dichotomy (vector value compare &optional start end key)
;; PRE:	entry is the element to be searched in the table.
;;         (<= start end)
;; RETURN: (values found index order)
;; POST:	(<= start index end)
;;         +-------------------+----------+-------+----------+----------------+
;;         | Case              |  found   | index |  order   |     Error      |
;;         +-------------------+----------+-------+----------+----------------+
;;         | x < a[min]        |   FALSE  |  min  |  less    |      0         |
;;         | a[i] < x < a[i+1] |   FALSE  |   i   |  greater |      0         |
;;         | x = a[i]          |   TRUE   |   i   |  equal   |      0         |
;;         | a[max] < x        |   FALSE  |  max  |  greater |      0         |
;;         +-------------------+----------+-------+----------+----------------+



;;; (defun pjb-cl$find-in-cache (min max cache)
;;;   (multiple-value-bind (found index order)
;;;       (dichotomy cache min (lambda (a b) (cond ((< a b) -1) 
;;;                                           ((= a b) 0)
;;;                                           (t 1))) 
;;;                  0 (length cache) (function car))
;;;     (let ((se))
;;;       (cond
;;;        ((< order 0)   (setf se (aref cache 0)))
;;;        ((= index max) (setf se nil))
;;;        (t (if (<= (cdr (aref cache index)) min)
;;;             (setf se nil)
;;;             (setf se (aref cache index)))))
;;;       (if se
;;;         (let ((range (- (cdr se) (car se))))
;;;           (goto-char (car se))
;;;           (re-search-forward (format ".{%d,%d}" (- (cdr se) (car se)))))
;;;         (re-search-forward  (format ".{%d}" (1+ (buffer-size)))))))
;;;   );;pjb-cl$find-in-cache


(defstruct fo-cache
  limit kind found start end)

(defvar *fo-cache* (make-fo-cache))
(defvar *fo-cache-counter-match* 0)
(defvar *fo-cache-counter-miss*  0)

(defun fo-cache (limit kind)
  (if (and (equal limit (fo-cache-limit *fo-cache*))
           (equal kind  (fo-cache-kind  *fo-cache*)))
      (progn (incf *fo-cache-counter-match*)
             (if (fo-cache-found *fo-cache*)
                 (progn
                   (goto-char (fo-cache-start *fo-cache*))
                   (looking-at (format ".{%d}" (- (fo-cache-end   *fo-cache*)
                                                  (fo-cache-start *fo-cache*))))
                   (values t t))
                 (values t nil)))
      (progn
        (incf *fo-cache-counter-miss*)
        (values nil nil)))) ;;fo-cache


(defun fo-put-in-cache (limit kind found start end)
  (setf (fo-cache-limit *fo-cache*) limit
        (fo-cache-kind  *fo-cache*) kind
        (fo-cache-found *fo-cache*) found
        (fo-cache-start *fo-cache*) start
        (fo-cache-end   *fo-cache*) end)) ;;fo-put-in-cache


(defun pjb-cl$find-object (limit kind)
  (multiple-value-bind (in-cache result) (fo-cache limit kind)
    (if in-cache
        result
        (progn
          (garbage-collect)
          (message "pjb-cl$find-object %S %S" limit kind)
          (do ((state    :out)
               (from (point))
               (level    0)
               (start)
               (found nil)
               (done  nil))
              (done 
               (progn
                 (message "pjb-cl$find-object done found=%S" found)
                 (fo-put-in-cache limit kind found 
                                  (if found (match-beginning 0) from)
                                  (if found (match-end 0)       from))
                 (unless found 
                   ;; we must reset the matches
                   (goto-char from)
                   (looking-at (regexp-quote
                                (buffer-substring-no-properties from (1+ from)))))
                 found))
            (case state
              ((:out)
               (cond
                 ((looking-at "\\\\."))
                 ((looking-at ";.*$")
                  (when (eq kind 'comment) (setf done t found t)))
                 ((looking-at "\\(\"\\([^\\\\\"]\\|\\\\\.\\)*?\"\\)")
                  (when (eq kind 'string) (setf done t found t)))
                 ((looking-at "\\(|\\([^|]\\|\\\\.\\)*?|\\)")
                  (when (eq kind 'symbol) (setf done t found t)))
                 ((looking-at "#|")
                  (setf state :sharp-comment level (1+ level)
                        start (match-beginning 0)))
                 ((looking-at "\\([^;#|\"\\\\]\\|#[^;#|\"\\\\]\\)*"))
                 ((looking-at ".")))
               (goto-char (match-end 0))
               (setf done (or done (<= limit (point)))))
              ((:sharp-comment)
               (cond
                 ((looking-at "#|")
                  (setf state :sharp-comment  level (1+ level))
                  (goto-char (match-end 0)))
                 ((looking-at "|#")
                  (decf level)
                  (when (= 0 level) 
                    (goto-char start)
                    (looking-at (format ".{%d}" (- (match-end 0) start)))
                    (setf done t found t))
                  (goto-char (match-end 0)))
                 ((looking-at "\\([^#|]*\\|#[^#|]\\||[^#|]\\)*"))
                 ((looking-at ".")))
               (goto-char (match-end 0))
               (setf done (or done (<= (point-max) (point)))))))
          )))) ;;pjb-cl$find-object


(defun pjb-cl$find-string (limit) 
  (pjb-cl$find-object limit 'string)
  ;; (pjb-cl$update-cache)
  ;; (pjb-cl$find-in-cache (point) limit (clfss-strings pjb-cl$*state*))
  ) ;;pjb-cl$find-string


(defun pjb-cl$find-symbol (limit) 
  (pjb-cl$find-object limit 'symbol)
  ;; (pjb-cl$update-cache)
  ;; (pjb-cl$find-in-cache (point) limit (clfss-symbols pjb-cl$*state*))
  ) ;;pjb-cl$find-symbol


(defun pjb-cl$find-comment (limit) 
  (pjb-cl$find-object limit 'comment)
  ;; (pjb-cl$update-cache)
  ;; (pjb-cl$find-in-cache (point) limit (clfss-comments pjb-cl$*state*))
  ) ;;pjb-cl$find-comment




(custom-declare-group 
 'common-lisp-faces
 (mapcar (lambda (kf) (list (kf-face kf) 'custom-face)) *kind-to-face-map*)
 "COMMON-LISP Faces"
 :group 'faces)


(map nil
     (lambda (kf) 
       (let ((kind (kf-kind kf))
             (face (kf-face kf))
             (spec (kf-spec kf)) )
         (set face face)
         (custom-declare-face
          face spec
          (format "Face for COMMON-LISP %s." kind)
          :group 'common-lisp-faces)))
     *kind-to-face-map*)


(defparameter *common-lisp-symbols*
  '(
    (*                                       "Function")
    (*                                       "Variable")
    (**                                      "Variable")
    (***                                     "Variable")
    (*BREAK-ON-SIGNALS*                      "Variable")
    (*COMPILE-FILE-PATHNAME*                 "Variable")
    (*COMPILE-FILE-TRUENAME*                 "Variable")
    (*COMPILE-PRINT*                         "Variable")
    (*COMPILE-VERBOSE*                       "Variable")
    (*DEBUG-IO*                              "Variable")
    (*DEBUGGER-HOOK*                         "Variable")
    (*DEFAULT-PATHNAME-DEFAULTS*             "Variable")
    (*ERROR-OUTPUT*                          "Variable")
    (*FEATURES*                              "Variable")
    (*GENSYM-COUNTER*                        "Variable")
    (*LOAD-PATHNAME*                         "Variable")
    (*LOAD-PRINT*                            "Variable")
    (*LOAD-TRUENAME*                         "Variable")
    (*LOAD-VERBOSE*                          "Variable")
    (*MACROEXPAND-HOOK*                      "Variable")
    (*MODULES*                               "Variable")
    (*PACKAGE*                               "Variable")
    (*PRINT-ARRAY*                           "Variable")
    (*PRINT-BASE*                            "Variable")
    (*PRINT-CASE*                            "Variable")
    (*PRINT-CIRCLE*                          "Variable")
    (*PRINT-ESCAPE*                          "Variable")
    (*PRINT-GENSYM*                          "Variable")
    (*PRINT-LENGTH*                          "Variable")
    (*PRINT-LEVEL*                           "Variable")
    (*PRINT-LINES*                           "Variable")
    (*PRINT-MISER-WIDTH*                     "Variable")
    (*PRINT-PPRINT-DISPATCH*                 "Variable")
    (*PRINT-PRETTY*                          "Variable")
    (*PRINT-RADIX*                           "Variable")
    (*PRINT-READABLY*                        "Variable")
    (*PRINT-RIGHT-MARGIN*                    "Variable")
    (*QUERY-IO*                              "Variable")
    (*RANDOM-STATE*                          "Variable")
    (*READ-BASE*                             "Variable")
    (*READ-DEFAULT-FLOAT-FORMAT*             "Variable")
    (*READ-EVAL*                             "Variable")
    (*READ-SUPPRESS*                         "Variable")
    (*READTABLE*                             "Variable")
    (*STANDARD-INPUT*                        "Variable")
    (*STANDARD-OUTPUT*                       "Variable")
    (*TERMINAL-IO*                           "Variable")
    (*TRACE-OUTPUT*                          "Variable")
    (+                                       "Function")
    (+                                       "Variable")
    (++                                      "Variable")
    (+++                                     "Variable")
    (-                                       "Function")
    (-                                       "Variable")
    (/                                       "Function")
    (/                                       "Variable")
    (//                                      "Variable")
    (///                                     "Variable")
    (/=                                      "Function")
    (1+                                      "Function")
    (1-                                      "Function")
    (<                                       "Function")
    (<=                                      "Function")
    (=                                       "Function")
    (>                                       "Function")
    (>=                                      "Function")
    (ABORT                                   "Function")
    (ABORT                                   "Restart")
    (ABS                                     "Function")
    (ACONS                                   "Function")
    (ACOS                                    "Function")
    (ACOSH                                   "Function")
    (ADD-METHOD                              "Standard Generic Function")
    (ADJOIN                                  "Function")
    (ADJUST-ARRAY                            "Function")
    (ADJUSTABLE-ARRAY-P                      "Function")
    (ALLOCATE-INSTANCE                       "Standard Generic Function")
    (ALPHA-CHAR-P                            "Function")
    (ALPHANUMERICP                           "Function")
    (AND                                     "Macro")
    (AND                                     "Type Specifier")
    (APPEND                                  "Function")
    (APPLY                                   "Function")
    (APROPOS                                 "Function")
    (APROPOS-LIST                            "Function")
    (AREF                                    "Accessor")
    (ARITHMETIC-ERROR                        "Condition Type")
    (ARITHMETIC-ERROR-OPERANDS               "Function")
    (ARITHMETIC-ERROR-OPERATION              "Function")
    (ARRAY                                   "System Class")
    (ARRAY-DIMENSION                         "Function")
    (ARRAY-DIMENSION-LIMIT                   "Constant Variable")
    (ARRAY-DIMENSIONS                        "Function")
    (ARRAY-DISPLACEMENT                      "Function")
    (ARRAY-ELEMENT-TYPE                      "Function")
    (ARRAY-HAS-FILL-POINTER-P                "Function")
    (ARRAY-IN-BOUNDS-P                       "Function")
    (ARRAY-RANK                              "Function")
    (ARRAY-RANK-LIMIT                        "Constant Variable")
    (ARRAY-ROW-MAJOR-INDEX                   "Function")
    (ARRAY-TOTAL-SIZE                        "Function")
    (ARRAY-TOTAL-SIZE-LIMIT                  "Constant Variable")
    (ARRAYP                                  "Function")
    (ASH                                     "Function")
    (ASIN                                    "Function")
    (ASINH                                   "Function")
    (ASSERT                                  "Macro")
    (ASSOC                                   "Function")
    (ASSOC-IF                                "Function")
    (ASSOC-IF-NOT                            "Function")
    (ATAN                                    "Function")
    (ATANH                                   "Function")
    (ATOM                                    "Function")
    (ATOM                                    "Type")
    (BASE-CHAR                               "Type")
    (BASE-STRING                             "Type")
    (BIGNUM                                  "Type")
    (BIT                                     "Accessor")
    (BIT                                     "Type")
    (BIT-AND                                 "Function")
    (BIT-ANDC1                               "Function")
    (BIT-ANDC2                               "Function")
    (BIT-EQV                                 "Function")
    (BIT-IOR                                 "Function")
    (BIT-NAND                                "Function")
    (BIT-NOR                                 "Function")
    (BIT-NOT                                 "Function")
    (BIT-ORC1                                "Function")
    (BIT-ORC2                                "Function")
    (BIT-VECTOR                              "System Class")
    (BIT-VECTOR-P                            "Function")
    (BIT-XOR                                 "Function")
    (BLOCK                                   "Special Operator")
    (BOOLE                                   "Function")
    (BOOLE-1                                 "Constant Variable")
    (BOOLE-2                                 "Constant Variable")
    (BOOLE-AND                               "Constant Variable")
    (BOOLE-ANDC1                             "Constant Variable")
    (BOOLE-ANDC2                             "Constant Variable")
    (BOOLE-C1                                "Constant Variable")
    (BOOLE-C2                                "Constant Variable")
    (BOOLE-CLR                               "Constant Variable")
    (BOOLE-EQV                               "Constant Variable")
    (BOOLE-IOR                               "Constant Variable")
    (BOOLE-NAND                              "Constant Variable")
    (BOOLE-NOR                               "Constant Variable")
    (BOOLE-ORC1                              "Constant Variable")
    (BOOLE-ORC2                              "Constant Variable")
    (BOOLE-SET                               "Constant Variable")
    (BOOLE-XOR                               "Constant Variable")
    (BOOLEAN                                 "Type")
    (BOTH-CASE-P                             "Function")
    (BOUNDP                                  "Function")
    (BREAK                                   "Function")
    (BROADCAST-STREAM                        "System Class")
    (BROADCAST-STREAM-STREAMS                "Function")
    (BUILT-IN-CLASS                          "System Class")
    (BUTLAST                                 "Function")
    (BYTE                                    "Function")
    (BYTE-POSITION                           "Function")
    (BYTE-SIZE                               "Function")
    (CAAAAR                                  "Accessor")
    (CAAADR                                  "Accessor")
    (CAAAR                                   "Accessor")
    (CAADAR                                  "Accessor")
    (CAADDR                                  "Accessor")
    (CAADR                                   "Accessor")
    (CAAR                                    "Accessor")
    (CADAAR                                  "Accessor")
    (CADADR                                  "Accessor")
    (CADAR                                   "Accessor")
    (CADDAR                                  "Accessor")
    (CADDDR                                  "Accessor")
    (CADDR                                   "Accessor")
    (CADR                                    "Accessor")
    (CALL-ARGUMENTS-LIMIT                    "Constant Variable")
    (CALL-METHOD                             "Local Macro")
    (CALL-NEXT-METHOD                        "Local Function")
    (CAR                                     "Accessor")
    (CASE                                    "Macro")
    (CATCH                                   "Special Operator")
    (CCASE                                   "Macro")
    (CDAAAR                                  "Accessor")
    (CDAADR                                  "Accessor")
    (CDAAR                                   "Accessor")
    (CDADAR                                  "Accessor")
    (CDADDR                                  "Accessor")
    (CDADR                                   "Accessor")
    (CDAR                                    "Accessor")
    (CDDAAR                                  "Accessor")
    (CDDADR                                  "Accessor")
    (CDDAR                                   "Accessor")
    (CDDDAR                                  "Accessor")
    (CDDDDR                                  "Accessor")
    (CDDDR                                   "Accessor")
    (CDDR                                    "Accessor")
    (CDR                                     "Accessor")
    (CEILING                                 "Function")
    (CELL-ERROR                              "Condition Type")
    (CELL-ERROR-NAME                         "Function")
    (CERROR                                  "Function")
    (CHANGE-CLASS                            "Standard Generic Function")
    (CHAR                                    "Accessor")
    (CHAR-CODE                               "Function")
    (CHAR-CODE-LIMIT                         "Constant Variable")
    (CHAR-DOWNCASE                           "Function")
    (CHAR-EQUAL                              "Function")
    (CHAR-GREATERP                           "Function")
    (CHAR-INT                                "Function")
    (CHAR-LESSP                              "Function")
    (CHAR-NAME                               "Function")
    (CHAR-NOT-EQUAL                          "Function")
    (CHAR-NOT-GREATERP                       "Function")
    (CHAR-NOT-LESSP                          "Function")
    (CHAR-UPCASE                             "Function")
    (CHAR/=                                  "Function")
    (CHAR<                                   "Function")
    (CHAR<=                                  "Function")
    (CHAR=                                   "Function")
    (CHAR>                                   "Function")
    (CHAR>=                                  "Function")
    (CHARACTER                               "Function")
    (CHARACTER                               "System Class")
    (CHARACTERP                              "Function")
    (CHECK-TYPE                              "Macro")
    (CIS                                     "Function")
    (CLASS                                   "System Class")
    (CLASS-NAME                              "Standard Generic Function")
    (CLASS-OF                                "Function")
    (CLEAR-INPUT                             "Function")
    (CLEAR-OUTPUT                            "Function")
    (CLOSE                                   "Function")
    (CLRHASH                                 "Function")
    (CODE-CHAR                               "Function")
    (COERCE                                  "Function")
    (COMPILE                                 "Function")
    (COMPILE-FILE                            "Function")
    (COMPILE-FILE-PATHNAME                   "Function")
    (COMPILED-FUNCTION                       "Type")
    (COMPILED-FUNCTION-P                     "Function")
    (COMPILER-MACRO-FUNCTION                 "Accessor")
    (COMPLEMENT                              "Function")
    (COMPLEX                                 "Function")
    (COMPLEX                                 "System Class")
    (COMPLEXP                                "Function")
    (COMPUTE-APPLICABLE-METHODS              "Standard Generic Function")
    (COMPUTE-RESTARTS                        "Function")
    (CONCATENATE                             "Function")
    (CONCATENATED-STREAM                     "System Class")
    (CONCATENATED-STREAM-STREAMS             "Function")
    (COND                                    "Macro")
    (CONDITION                               "Condition Type")
    (CONJUGATE                               "Function")
    (CONS                                    "Function")
    (CONS                                    "System Class")
    (CONSP                                   "Function")
    (CONSTANTLY                              "Function")
    (CONSTANTP                               "Function")
    (CONTINUE                                "Function")
    (CONTINUE                                "Restart")
    (CONTROL-ERROR                           "Condition Type")
    (COPY-ALIST                              "Function")
    (COPY-LIST                               "Function")
    (COPY-PPRINT-DISPATCH                    "Function")
    (COPY-READTABLE                          "Function")
    (COPY-SEQ                                "Function")
    (COPY-STRUCTURE                          "Function")
    (COPY-SYMBOL                             "Function")
    (COPY-TREE                               "Function")
    (COS                                     "Function")
    (COSH                                    "Function")
    (COUNT                                   "Function")
    (COUNT-IF                                "Function")
    (COUNT-IF-NOT                            "Function")
    (CTYPECASE                               "Macro")
    (DECF                                    "Macro")
    (DECLAIM                                 "Macro")
    (DECLARATION                             "Declaration")
    (DECLARE                                 "Symbol")
    (DECODE-FLOAT                            "Function")
    (DECODE-UNIVERSAL-TIME                   "Function")
    (DEFCLASS                                "Macro")
    (DEFCONSTANT                             "Macro")
    (DEFGENERIC                              "Macro")
    (DEFINE-COMPILER-MACRO                   "Macro")
    (DEFINE-CONDITION                        "Macro")
    (DEFINE-METHOD-COMBINATION               "Macro")
    (DEFINE-MODIFY-MACRO                     "Macro")
    (DEFINE-SETF-EXPANDER                    "Macro")
    (DEFINE-SYMBOL-MACRO                     "Macro")
    (DEFMACRO                                "Macro")
    (DEFMETHOD                               "Macro")
    (DEFPACKAGE                              "Macro")
    (DEFPARAMETER                            "Macro")
    (DEFSETF                                 "Macro")
    (DEFSTRUCT                               "Macro")
    (DEFTYPE                                 "Macro")
    (DEFUN                                   "Macro")
    (DEFVAR                                  "Macro")
    (DELETE                                  "Function")
    (DELETE-DUPLICATES                       "Function")
    (DELETE-FILE                             "Function")
    (DELETE-IF                               "Function")
    (DELETE-IF-NOT                           "Function")
    (DELETE-PACKAGE                          "Function")
    (DENOMINATOR                             "Function")
    (DEPOSIT-FIELD                           "Function")
    (DESCRIBE                                "Function")
    (DESCRIBE-OBJECT                         "Standard Generic Function")
    (DESTRUCTURING-BIND                      "Macro")
    (DIGIT-CHAR                              "Function")
    (DIGIT-CHAR-P                            "Function")
    (DIRECTORY                               "Function")
    (DIRECTORY-NAMESTRING                    "Function")
    (DISASSEMBLE                             "Function")
    (DIVISION-BY-ZERO                        "Condition Type")
    (DO                                      "Macro")
    (DO*                                     "Macro")
    (DO-ALL-SYMBOLS                          "Macro")
    (DO-EXTERNAL-SYMBOLS                     "Macro")
    (DO-SYMBOLS                              "Macro")
    (DOCUMENTATION                           "Standard Generic Function")
    (DOLIST                                  "Macro")
    (DOTIMES                                 "Macro")
    (DOUBLE-FLOAT                            "Type")
    (DOUBLE-FLOAT-EPSILON                    "Constant Variable")
    (DOUBLE-FLOAT-NEGATIVE-EPSILON           "Constant Variable")
    (DPB                                     "Function")
    (DRIBBLE                                 "Function")
    (DYNAMIC-EXTENT                          "Declaration")
    (ECASE                                   "Macro")
    (ECHO-STREAM                             "System Class")
    (ECHO-STREAM-INPUT-STREAM                "Function")
    (ECHO-STREAM-OUTPUT-STREAM               "Function")
    (ED                                      "Function")
    (EIGHTH                                  "Accessor")
    (ELT                                     "Accessor")
    (END-OF-FILE                             "Condition Type")
    (ENDP                                    "Function")
    (ENOUGH-NAMESTRING                       "Function")
    (ENSURE-DIRECTORIES-EXIST                "Function")
    (ENSURE-GENERIC-FUNCTION                 "Function")
    (EQ                                      "Function")
    (EQL                                     "Function")
    (EQL                                     "Type Specifier")
    (EQUAL                                   "Function")
    (EQUALP                                  "Function")
    (ERROR                                   "Condition Type")
    (ERROR                                   "Function")
    (ETYPECASE                               "Macro")
    (EVAL                                    "Function")
    (EVAL-WHEN                               "Special Operator")
    (EVENP                                   "Function")
    (EVERY                                   "Function")
    (EXP                                     "Function")
    (EXPORT                                  "Function")
    (EXPT                                    "Function")
    (EXTENDED-CHAR                           "Type")
    (FBOUNDP                                 "Function")
    (FCEILING                                "Function")
    (FDEFINITION                             "Accessor")
    (FFLOOR                                  "Function")
    (FIFTH                                   "Accessor")
    (FILE-AUTHOR                             "Function")
    (FILE-ERROR                              "Condition Type")
    (FILE-ERROR-PATHNAME                     "Function")
    (FILE-LENGTH                             "Function")
    (FILE-NAMESTRING                         "Function")
    (FILE-POSITION                           "Function")
    (FILE-STREAM                             "System Class")
    (FILE-STRING-LENGTH                      "Function")
    (FILE-WRITE-DATE                         "Function")
    (FILL                                    "Function")
    (FILL-POINTER                            "Accessor")
    (FIND                                    "Function")
    (FIND-ALL-SYMBOLS                        "Function")
    (FIND-CLASS                              "Accessor")
    (FIND-IF                                 "Function")
    (FIND-IF-NOT                             "Function")
    (FIND-METHOD                             "Standard Generic Function")
    (FIND-PACKAGE                            "Function")
    (FIND-RESTART                            "Function")
    (FIND-SYMBOL                             "Function")
    (FINISH-OUTPUT                           "Function")
    (FIRST                                   "Accessor")
    (FIXNUM                                  "Type")
    (FLET                                    "Special Operator")
    (FLOAT                                   "Function")
    (FLOAT                                   "System Class")
    (FLOAT-DIGITS                            "Function")
    (FLOAT-PRECISION                         "Function")
    (FLOAT-RADIX                             "Function")
    (FLOAT-SIGN                              "Function")
    (FLOATING-POINT-INEXACT                  "Condition Type")
    (FLOATING-POINT-INVALID-OPERATION        "Condition Type")
    (FLOATING-POINT-OVERFLOW                 "Condition Type")
    (FLOATING-POINT-UNDERFLOW                "Condition Type")
    (FLOATP                                  "Function")
    (FLOOR                                   "Function")
    (FMAKUNBOUND                             "Function")
    (FORCE-OUTPUT                            "Function")
    (FORMAT                                  "Function")
    (FORMATTER                               "Macro")
    (FOURTH                                  "Accessor")
    (FRESH-LINE                              "Function")
    (FROUND                                  "Function")
    (FTRUNCATE                               "Function")
    (FTYPE                                   "Declaration")
    (FUNCALL                                 "Function")
    (FUNCTION                                "Special Operator")
    (FUNCTION                                "System Class")
    (FUNCTION-KEYWORDS                       "Standard Generic Function")
    (FUNCTION-LAMBDA-EXPRESSION              "Function")
    (FUNCTIONP                               "Function")
    (GCD                                     "Function")
    (GENERIC-FUNCTION                        "System Class")
    (GENSYM                                  "Function")
    (GENTEMP                                 "Function")
    (GET                                     "Accessor")
    (GET-DECODED-TIME                        "Function")
    (GET-DISPATCH-MACRO-CHARACTER            "Function")
    (GET-INTERNAL-REAL-TIME                  "Function")
    (GET-INTERNAL-RUN-TIME                   "Function")
    (GET-MACRO-CHARACTER                     "Function")
    (GET-OUTPUT-STREAM-STRING                "Function")
    (GET-PROPERTIES                          "Function")
    (GET-SETF-EXPANSION                      "Function")
    (GET-UNIVERSAL-TIME                      "Function")
    (GETF                                    "Accessor")
    (GETHASH                                 "Accessor")
    (GO                                      "Special Operator")
    (GRAPHIC-CHAR-P                          "Function")
    (HANDLER-BIND                            "Macro")
    (HANDLER-CASE                            "Macro")
    (HASH-TABLE                              "System Class")
    (HASH-TABLE-COUNT                        "Function")
    (HASH-TABLE-P                            "Function")
    (HASH-TABLE-REHASH-SIZE                  "Function")
    (HASH-TABLE-REHASH-THRESHOLD             "Function")
    (HASH-TABLE-SIZE                         "Function")
    (HASH-TABLE-TEST                         "Function")
    (HOST-NAMESTRING                         "Function")
    (IDENTITY                                "Function")
    (IF                                      "Special Operator")
    (IGNORABLE                               "Declaration")
    (IGNORE                                  "Declaration")
    (IGNORE-ERRORS                           "Macro")
    (IMAGPART                                "Function")
    (IMPORT                                  "Function")
    (IN-PACKAGE                              "Macro")
    (INCF                                    "Macro")
    (INITIALIZE-INSTANCE                     "Standard Generic Function")
    (INLINE                                  "Declaration")
    (INPUT-STREAM-P                          "Function")
    (INSPECT                                 "Function")
    (INTEGER                                 "System Class")
    (INTEGER-DECODE-FLOAT                    "Function")
    (INTEGER-LENGTH                          "Function")
    (INTEGERP                                "Function")
    (INTERACTIVE-STREAM-P                    "Function")
    (INTERN                                  "Function")
    (INTERNAL-TIME-UNITS-PER-SECOND          "Constant Variable")
    (INTERSECTION                            "Function")
    (INVALID-METHOD-ERROR                    "Function")
    (INVOKE-DEBUGGER                         "Function")
    (INVOKE-RESTART                          "Function")
    (INVOKE-RESTART-INTERACTIVELY            "Function")
    (ISQRT                                   "Function")
    (KEYWORD                                 "Type")
    (KEYWORDP                                "Function")
    (LABELS                                  "Special Operator")
    (LAMBDA                                  "Macro")
    (LAMBDA                                  "Symbol")
    (LAMBDA-LIST-KEYWORDS                    "Constant Variable")
    (LAMBDA-PARAMETERS-LIMIT                 "Constant Variable")
    (LAST                                    "Function")
    (LCM                                     "Function")
    (LDB                                     "Accessor")
    (LDB-TEST                                "Function")
    (LDIFF                                   "Function")
    (LEAST-NEGATIVE-DOUBLE-FLOAT             "Constant Variable")
    (LEAST-NEGATIVE-LONG-FLOAT               "Constant Variable")
    (LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT  "Constant Variable")
    (LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT    "Constant Variable")
    (LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT   "Constant Variable")
    (LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT  "Constant Variable")
    (LEAST-NEGATIVE-SHORT-FLOAT              "Constant Variable")
    (LEAST-NEGATIVE-SINGLE-FLOAT             "Constant Variable")
    (LEAST-POSITIVE-DOUBLE-FLOAT             "Constant Variable")
    (LEAST-POSITIVE-LONG-FLOAT               "Constant Variable")
    (LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT  "Constant Variable")
    (LEAST-POSITIVE-NORMALIZED-LONG-FLOAT    "Constant Variable")
    (LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT   "Constant Variable")
    (LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT  "Constant Variable")
    (LEAST-POSITIVE-SHORT-FLOAT              "Constant Variable")
    (LEAST-POSITIVE-SINGLE-FLOAT             "Constant Variable")
    (LENGTH                                  "Function")
    (LET                                     "Special Operator")
    (LET*                                    "Special Operator")
    (LISP-IMPLEMENTATION-TYPE                "Function")
    (LISP-IMPLEMENTATION-VERSION             "Function")
    (LIST                                    "Function")
    (LIST                                    "System Class")
    (LIST*                                   "Function")
    (LIST-ALL-PACKAGES                       "Function")
    (LIST-LENGTH                             "Function")
    (LISTEN                                  "Function")
    (LISTP                                   "Function")
    (LOAD                                    "Function")
    (LOAD-LOGICAL-PATHNAME-TRANSLATIONS      "Function")
    (LOAD-TIME-VALUE                         "Special Operator")
    (LOCALLY                                 "Special Operator")
    (LOG                                     "Function")
    (LOGAND                                  "Function")
    (LOGANDC1                                "Function")
    (LOGANDC2                                "Function")
    (LOGBITP                                 "Function")
    (LOGCOUNT                                "Function")
    (LOGEQV                                  "Function")
    (LOGICAL-PATHNAME                        "Function")
    (LOGICAL-PATHNAME                        "System Class")
    (LOGICAL-PATHNAME-TRANSLATIONS           "Accessor")
    (LOGIOR                                  "Function")
    (LOGNAND                                 "Function")
    (LOGNOR                                  "Function")
    (LOGNOT                                  "Function")
    (LOGORC1                                 "Function")
    (LOGORC2                                 "Function")
    (LOGTEST                                 "Function")
    (LOGXOR                                  "Function")
    (LONG-FLOAT                              "Type")
    (LONG-FLOAT-EPSILON                      "Constant Variable")
    (LONG-FLOAT-NEGATIVE-EPSILON             "Constant Variable")
    (LONG-SITE-NAME                          "Function")
    (LOOP                                    "Macro")
    (LOOP-FINISH                             "Local Macro")
    (LOWER-CASE-P                            "Function")
    (MACHINE-INSTANCE                        "Function")
    (MACHINE-TYPE                            "Function")
    (MACHINE-VERSION                         "Function")
    (MACRO-FUNCTION                          "Accessor")
    (MACROEXPAND                             "Function")
    (MACROEXPAND-1                           "Function")
    (MACROLET                                "Special Operator")
    (MAKE-ARRAY                              "Function")
    (MAKE-BROADCAST-STREAM                   "Function")
    (MAKE-CONCATENATED-STREAM                "Function")
    (MAKE-CONDITION                          "Function")
    (MAKE-DISPATCH-MACRO-CHARACTER           "Function")
    (MAKE-ECHO-STREAM                        "Function")
    (MAKE-HASH-TABLE                         "Function")
    (MAKE-INSTANCE                           "Standard Generic Function")
    (MAKE-INSTANCES-OBSOLETE                 "Standard Generic Function")
    (MAKE-LIST                               "Function")
    (MAKE-LOAD-FORM                          "Standard Generic Function")
    (MAKE-LOAD-FORM-SAVING-SLOTS             "Function")
    (MAKE-METHOD                             "Local Macro")
    (MAKE-PACKAGE                            "Function")
    (MAKE-PATHNAME                           "Function")
    (MAKE-RANDOM-STATE                       "Function")
    (MAKE-SEQUENCE                           "Function")
    (MAKE-STRING                             "Function")
    (MAKE-STRING-INPUT-STREAM                "Function")
    (MAKE-STRING-OUTPUT-STREAM               "Function")
    (MAKE-SYMBOL                             "Function")
    (MAKE-SYNONYM-STREAM                     "Function")
    (MAKE-TWO-WAY-STREAM                     "Function")
    (MAKUNBOUND                              "Function")
    (MAP                                     "Function")
    (MAP-INTO                                "Function")
    (MAPC                                    "Function")
    (MAPCAN                                  "Function")
    (MAPCAR                                  "Function")
    (MAPCON                                  "Function")
    (MAPHASH                                 "Function")
    (MAPL                                    "Function")
    (MAPLIST                                 "Function")
    (MASK-FIELD                              "Accessor")
    (MAX                                     "Function")
    (MEMBER                                  "Function")
    (MEMBER                                  "Type Specifier")
    (MEMBER-IF                               "Function")
    (MEMBER-IF-NOT                           "Function")
    (MERGE                                   "Function")
    (MERGE-PATHNAMES                         "Function")
    (METHOD                                  "System Class")
    (METHOD-COMBINATION                      "System Class")
    (METHOD-COMBINATION-ERROR                "Function")
    (METHOD-QUALIFIERS                       "Standard Generic Function")
    (MIN                                     "Function")
    (MINUSP                                  "Function")
    (MISMATCH                                "Function")
    (MOD                                     "Function")
    (MOD                                     "Type Specifier")
    (MOST-NEGATIVE-DOUBLE-FLOAT              "Constant Variable")
    (MOST-NEGATIVE-FIXNUM                    "Constant Variable")
    (MOST-NEGATIVE-LONG-FLOAT                "Constant Variable")
    (MOST-NEGATIVE-SHORT-FLOAT               "Constant Variable")
    (MOST-NEGATIVE-SINGLE-FLOAT              "Constant Variable")
    (MOST-POSITIVE-DOUBLE-FLOAT              "Constant Variable")
    (MOST-POSITIVE-FIXNUM                    "Constant Variable")
    (MOST-POSITIVE-LONG-FLOAT                "Constant Variable")
    (MOST-POSITIVE-SHORT-FLOAT               "Constant Variable")
    (MOST-POSITIVE-SINGLE-FLOAT              "Constant Variable")
    (MUFFLE-WARNING                          "Function")
    (MUFFLE-WARNING                          "Restart")
    (MULTIPLE-VALUE-BIND                     "Macro")
    (MULTIPLE-VALUE-CALL                     "Special Operator")
    (MULTIPLE-VALUE-LIST                     "Macro")
    (MULTIPLE-VALUE-PROG1                    "Special Operator")
    (MULTIPLE-VALUE-SETQ                     "Macro")
    (MULTIPLE-VALUES-LIMIT                   "Constant Variable")
    (NAME-CHAR                               "Function")
    (NAMESTRING                              "Function")
    (NBUTLAST                                "Function")
    (NCONC                                   "Function")
    (NEXT-METHOD-P                           "Local Function")
    (NIL                                     "Constant Variable")
    (NIL                                     "Type")
    (NINTERSECTION                           "Function")
    (NINTH                                   "Accessor")
    (NO-APPLICABLE-METHOD                    "Standard Generic Function")
    (NO-NEXT-METHOD                          "Standard Generic Function")
    (NOT                                     "Function")
    (NOT                                     "Type Specifier")
    (NOTANY                                  "Function")
    (NOTEVERY                                "Function")
    (NOTINLINE                               "Declaration")
    (NRECONC                                 "Function")
    (NREVERSE                                "Function")
    (NSET-DIFFERENCE                         "Function")
    (NSET-EXCLUSIVE-OR                       "Function")
    (NSTRING-CAPITALIZE                      "Function")
    (NSTRING-DOWNCASE                        "Function")
    (NSTRING-UPCASE                          "Function")
    (NSUBLIS                                 "Function")
    (NSUBST                                  "Function")
    (NSUBST-IF                               "Function")
    (NSUBST-IF-NOT                           "Function")
    (NSUBSTITUTE                             "Function")
    (NSUBSTITUTE-IF                          "Function")
    (NSUBSTITUTE-IF-NOT                      "Function")
    (NTH                                     "Accessor")
    (NTH-VALUE                               "Macro")
    (NTHCDR                                  "Function")
    (NULL                                    "Function")
    (NULL                                    "System Class")
    (NUMBER                                  "System Class")
    (NUMBERP                                 "Function")
    (NUMERATOR                               "Function")
    (NUNION                                  "Function")
    (ODDP                                    "Function")
    (OPEN                                    "Function")
    (OPEN-STREAM-P                           "Function")
    (OPTIMIZE                                "Declaration")
    (OR                                      "Macro")
    (OR                                      "Type Specifier")
    (OUTPUT-STREAM-P                         "Function")
    (PACKAGE                                 "System Class")
    (PACKAGE-ERROR                           "Condition Type")
    (PACKAGE-ERROR-PACKAGE                   "Function")
    (PACKAGE-NAME                            "Function")
    (PACKAGE-NICKNAMES                       "Function")
    (PACKAGE-SHADOWING-SYMBOLS               "Function")
    (PACKAGE-USE-LIST                        "Function")
    (PACKAGE-USED-BY-LIST                    "Function")
    (PACKAGEP                                "Function")
    (PAIRLIS                                 "Function")
    (PARSE-ERROR                             "Condition Type")
    (PARSE-INTEGER                           "Function")
    (PARSE-NAMESTRING                        "Function")
    (PATHNAME                                "Function")
    (PATHNAME                                "System Class")
    (PATHNAME-DEVICE                         "Function")
    (PATHNAME-DIRECTORY                      "Function")
    (PATHNAME-HOST                           "Function")
    (PATHNAME-MATCH-P                        "Function")
    (PATHNAME-NAME                           "Function")
    (PATHNAME-TYPE                           "Function")
    (PATHNAME-VERSION                        "Function")
    (PATHNAMEP                               "Function")
    (PEEK-CHAR                               "Function")
    (PHASE                                   "Function")
    (PI                                      "Constant Variable")
    (PLUSP                                   "Function")
    (POP                                     "Macro")
    (POSITION                                "Function")
    (POSITION-IF                             "Function")
    (POSITION-IF-NOT                         "Function")
    (PPRINT                                  "Function")
    (PPRINT-DISPATCH                         "Function")
    (PPRINT-EXIT-IF-LIST-EXHAUSTED           "Local Macro")
    (PPRINT-FILL                             "Function")
    (PPRINT-INDENT                           "Function")
    (PPRINT-LINEAR                           "Function")
    (PPRINT-LOGICAL-BLOCK                    "Macro")
    (PPRINT-NEWLINE                          "Function")
    (PPRINT-POP                              "Local Macro")
    (PPRINT-TAB                              "Function")
    (PPRINT-TABULAR                          "Function")
    (PRIN1                                   "Function")
    (PRIN1-TO-STRING                         "Function")
    (PRINC                                   "Function")
    (PRINC-TO-STRING                         "Function")
    (PRINT                                   "Function")
    (PRINT-NOT-READABLE                      "Condition Type")
    (PRINT-NOT-READABLE-OBJECT               "Function")
    (PRINT-OBJECT                            "Standard Generic Function")
    (PRINT-UNREADABLE-OBJECT                 "Macro")
    (PROBE-FILE                              "Function")
    (PROCLAIM                                "Function")
    (PROG                                    "Macro")
    (PROG*                                   "Macro")
    (PROG1                                   "Macro")
    (PROG2                                   "Macro")
    (PROGN                                   "Special Operator")
    (PROGRAM-ERROR                           "Condition Type")
    (PROGV                                   "Special Operator")
    (PROVIDE                                 "Function")
    (PSETF                                   "Macro")
    (PSETQ                                   "Macro")
    (PUSH                                    "Macro")
    (PUSHNEW                                 "Macro")
    (QUOTE                                   "Special Operator")
    (RANDOM                                  "Function")
    (RANDOM-STATE                            "System Class")
    (RANDOM-STATE-P                          "Function")
    (RASSOC                                  "Function")
    (RASSOC-IF                               "Function")
    (RASSOC-IF-NOT                           "Function")
    (RATIO                                   "System Class")
    (RATIONAL                                "Function")
    (RATIONAL                                "System Class")
    (RATIONALIZE                             "Function")
    (RATIONALP                               "Function")
    (READ                                    "Function")
    (READ-BYTE                               "Function")
    (READ-CHAR                               "Function")
    (READ-CHAR-NO-HANG                       "Function")
    (READ-DELIMITED-LIST                     "Function")
    (READ-FROM-STRING                        "Function")
    (READ-LINE                               "Function")
    (READ-PRESERVING-WHITESPACE              "Function")
    (READ-SEQUENCE                           "Function")
    (READER-ERROR                            "Condition Type")
    (READTABLE                               "System Class")
    (READTABLE-CASE                          "Accessor")
    (READTABLEP                              "Function")
    (REAL                                    "System Class")
    (REALP                                   "Function")
    (REALPART                                "Function")
    (REDUCE                                  "Function")
    (REINITIALIZE-INSTANCE                   "Standard Generic Function")
    (REM                                     "Function")
    (REMF                                    "Macro")
    (REMHASH                                 "Function")
    (REMOVE                                  "Function")
    (REMOVE-DUPLICATES                       "Function")
    (REMOVE-IF                               "Function")
    (REMOVE-IF-NOT                           "Function")
    (REMOVE-METHOD                           "Standard Generic Function")
    (REMPROP                                 "Function")
    (RENAME-FILE                             "Function")
    (RENAME-PACKAGE                          "Function")
    (REPLACE                                 "Function")
    (REQUIRE                                 "Function")
    (REST                                    "Accessor")
    (RESTART                                 "System Class")
    (RESTART-BIND                            "Macro")
    (RESTART-CASE                            "Macro")
    (RESTART-NAME                            "Function")
    (RETURN                                  "Macro")
    (RETURN-FROM                             "Special Operator")
    (REVAPPEND                               "Function")
    (REVERSE                                 "Function")
    (ROOM                                    "Function")
    (ROTATEF                                 "Macro")
    (ROUND                                   "Function")
    (ROW-MAJOR-AREF                          "Accessor")
    (RPLACA                                  "Function")
    (RPLACD                                  "Function")
    (SATISFIES                               "Type Specifier")
    (SBIT                                    "Accessor")
    (SCALE-FLOAT                             "Function")
    (SCHAR                                   "Accessor")
    (SEARCH                                  "Function")
    (SECOND                                  "Accessor")
    (SEQUENCE                                "System Class")
    (SERIOUS-CONDITION                       "Condition Type")
    (SET                                     "Function")
    (SET-DIFFERENCE                          "Function")
    (SET-DISPATCH-MACRO-CHARACTER            "Function")
    (SET-EXCLUSIVE-OR                        "Function")
    (SET-MACRO-CHARACTER                     "Function")
    (SET-PPRINT-DISPATCH                     "Function")
    (SET-SYNTAX-FROM-CHAR                    "Function")
    (SETF                                    "Macro")
    (SETQ                                    "Special Form")
    (SEVENTH                                 "Accessor")
    (SHADOW                                  "Function")
    (SHADOWING-IMPORT                        "Function")
    (SHARED-INITIALIZE                       "Standard Generic Function")
    (SHIFTF                                  "Macro")
    (SHORT-FLOAT                             "Type")
    (SHORT-FLOAT-EPSILON                     "Constant Variable")
    (SHORT-FLOAT-NEGATIVE-EPSILON            "Constant Variable")
    (SHORT-SITE-NAME                         "Function")
    (SIGNAL                                  "Function")
    (SIGNED-BYTE                             "Type")
    (SIGNUM                                  "Function")
    (SIMPLE-ARRAY                            "Type")
    (SIMPLE-BASE-STRING                      "Type")
    (SIMPLE-BIT-VECTOR                       "Type")
    (SIMPLE-BIT-VECTOR-P                     "Function")
    (SIMPLE-CONDITION                        "Condition Type")
    (SIMPLE-CONDITION-FORMAT-ARGUMENTS       "Function")
    (SIMPLE-CONDITION-FORMAT-CONTROL         "Function")
    (SIMPLE-ERROR                            "Condition Type")
    (SIMPLE-STRING                           "Type")
    (SIMPLE-STRING-P                         "Function")
    (SIMPLE-TYPE-ERROR                       "Condition Type")
    (SIMPLE-VECTOR                           "Type")
    (SIMPLE-VECTOR-P                         "Function")
    (SIMPLE-WARNING                          "Condition Type")
    (SIN                                     "Function")
    (SINGLE-FLOAT                            "Type")
    (SINGLE-FLOAT-EPSILON                    "Constant Variable")
    (SINGLE-FLOAT-NEGATIVE-EPSILON           "Constant Variable")
    (SINH                                    "Function")
    (SIXTH                                   "Accessor")
    (SLEEP                                   "Function")
    (SLOT-BOUNDP                             "Function")
    (SLOT-EXISTS-P                           "Function")
    (SLOT-MAKUNBOUND                         "Function")
    (SLOT-MISSING                            "Standard Generic Function")
    (SLOT-UNBOUND                            "Standard Generic Function")
    (SLOT-VALUE                              "Function")
    (SOFTWARE-TYPE                           "Function")
    (SOFTWARE-VERSION                        "Function")
    (SOME                                    "Function")
    (SORT                                    "Function")
    (SPECIAL                                 "Declaration")
    (SPECIAL-OPERATOR-P                      "Function")
    (SQRT                                    "Function")
    (STABLE-SORT                             "Function")
    (STANDARD-CHAR                           "Type")
    (STANDARD-CHAR-P                         "Function")
    (STANDARD-CLASS                          "System Class")
    (STANDARD-GENERIC-FUNCTION               "System Class")
    (STANDARD-METHOD                         "System Class")
    (STANDARD-OBJECT                         "Class")
    (STEP                                    "Macro")
    (STORAGE-CONDITION                       "Condition Type")
    (STORE-VALUE                             "Function")
    (STORE-VALUE                             "Restart")
    (STREAM                                  "System Class")
    (STREAM-ELEMENT-TYPE                     "Function")
    (STREAM-ERROR                            "Condition Type")
    (STREAM-ERROR-STREAM                     "Function")
    (STREAM-EXTERNAL-FORMAT                  "Function")
    (STREAMP                                 "Function")
    (STRING                                  "Function")
    (STRING                                  "System Class")
    (STRING-CAPITALIZE                       "Function")
    (STRING-DOWNCASE                         "Function")
    (STRING-EQUAL                            "Function")
    (STRING-GREATERP                         "Function")
    (STRING-LEFT-TRIM                        "Function")
    (STRING-LESSP                            "Function")
    (STRING-NOT-EQUAL                        "Function")
    (STRING-NOT-GREATERP                     "Function")
    (STRING-NOT-LESSP                        "Function")
    (STRING-RIGHT-TRIM                       "Function")
    (STRING-STREAM                           "System Class")
    (STRING-TRIM                             "Function")
    (STRING-UPCASE                           "Function")
    (STRING/=                                "Function")
    (STRING<                                 "Function")
    (STRING<=                                "Function")
    (STRING=                                 "Function")
    (STRING>                                 "Function")
    (STRING>=                                "Function")
    (STRINGP                                 "Function")
    (STRUCTURE-CLASS                         "System Class")
    (STRUCTURE-OBJECT                        "Class")
    (STYLE-WARNING                           "Condition Type")
    (SUBLIS                                  "Function")
    (SUBSEQ                                  "Accessor")
    (SUBSETP                                 "Function")
    (SUBST                                   "Function")
    (SUBST-IF                                "Function")
    (SUBST-IF-NOT                            "Function")
    (SUBSTITUTE                              "Function")
    (SUBSTITUTE-IF                           "Function")
    (SUBSTITUTE-IF-NOT                       "Function")
    (SUBTYPEP                                "Function")
    (SVREF                                   "Accessor")
    (SXHASH                                  "Function")
    (SYMBOL                                  "System Class")
    (SYMBOL-FUNCTION                         "Accessor")
    (SYMBOL-MACROLET                         "Special Operator")
    (SYMBOL-NAME                             "Function")
    (SYMBOL-PACKAGE                          "Function")
    (SYMBOL-PLIST                            "Accessor")
    (SYMBOL-VALUE                            "Accessor")
    (SYMBOLP                                 "Function")
    (SYNONYM-STREAM                          "System Class")
    (SYNONYM-STREAM-SYMBOL                   "Function")
    (T                                       "Constant Variable")
    (T                                       "System Class")
    (TAGBODY                                 "Special Operator")
    (TAILP                                   "Function")
    (TAN                                     "Function")
    (TANH                                    "Function")
    (TENTH                                   "Accessor")
    (TERPRI                                  "Function")
    (THE                                     "Special Operator")
    (THIRD                                   "Accessor")
    (THROW                                   "Special Operator")
    (TIME                                    "Macro")
    (TRACE                                   "Macro")
    (TRANSLATE-LOGICAL-PATHNAME              "Function")
    (TRANSLATE-PATHNAME                      "Function")
    (TREE-EQUAL                              "Function")
    (TRUENAME                                "Function")
    (TRUNCATE                                "Function")
    (TWO-WAY-STREAM                          "System Class")
    (TWO-WAY-STREAM-INPUT-STREAM             "Function")
    (TWO-WAY-STREAM-OUTPUT-STREAM            "Function")
    (TYPE                                    "Declaration")
    (TYPE-ERROR                              "Condition Type")
    (TYPE-ERROR-DATUM                        "Function")
    (TYPE-ERROR-EXPECTED-TYPE                "Function")
    (TYPE-OF                                 "Function")
    (TYPECASE                                "Macro")
    (TYPEP                                   "Function")
    (UNBOUND-SLOT                            "Condition Type")
    (UNBOUND-SLOT-INSTANCE                   "Function")
    (UNBOUND-VARIABLE                        "Condition Type")
    (UNDEFINED-FUNCTION                      "Condition Type")
    (UNEXPORT                                "Function")
    (UNINTERN                                "Function")
    (UNION                                   "Function")
    (UNLESS                                  "Macro")
    (UNREAD-CHAR                             "Function")
    (UNSIGNED-BYTE                           "Type")
    (UNTRACE                                 "Macro")
    (UNUSE-PACKAGE                           "Function")
    (UNWIND-PROTECT                          "Special Operator")
    (UPDATE-INSTANCE-FOR-DIFFERENT-CLASS     "Standard Generic Function")
    (UPDATE-INSTANCE-FOR-REDEFINED-CLASS     "Standard Generic Function")
    (UPGRADED-ARRAY-ELEMENT-TYPE             "Function")
    (UPGRADED-COMPLEX-PART-TYPE              "Function")
    (UPPER-CASE-P                            "Function")
    (USE-PACKAGE                             "Function")
    (USE-VALUE                               "Function")
    (USE-VALUE                               "Restart")
    (USER-HOMEDIR-PATHNAME                   "Function")
    (VALUES                                  "Accessor")
    (VALUES                                  "Type Specifier")
    (VALUES-LIST                             "Function")
    (VECTOR                                  "Function")
    (VECTOR                                  "System Class")
    (VECTOR-POP                              "Function")
    (VECTOR-PUSH                             "Function")
    (VECTOR-PUSH-EXTEND                      "Function")
    (VECTORP                                 "Function")
    (WARN                                    "Function")
    (WARNING                                 "Condition Type")
    (WHEN                                    "Macro")
    (WILD-PATHNAME-P                         "Function")
    (WITH-ACCESSORS                          "Macro")
    (WITH-COMPILATION-UNIT                   "Macro")
    (WITH-CONDITION-RESTARTS                 "Macro")
    (WITH-HASH-TABLE-ITERATOR                "Macro")
    (WITH-INPUT-FROM-STRING                  "Macro")
    (WITH-OPEN-STREAM                        "Macro")
    (WITH-OUTPUT-TO-STRING                   "Macro")
    (WITH-PACKAGE-ITERATOR                   "Macro")
    (WITH-SIMPLE-RESTART                     "Macro")
    (WITH-SLOTS                              "Macro")
    (WITH-STANDARD-IO-SYNTAX                 "Macro")
    (WRITE                                   "Function")
    (WRITE-BYTE                              "Function")
    (WRITE-CHAR                              "Function")
    (WRITE-LINE                              "Function")
    (WRITE-SEQUENCE                          "Function")
    (WRITE-STRING                            "Function")
    (WRITE-TO-STRING                         "Function")
    (Y-OR-N-P                                "Function")
    (YES-OR-NO-P                             "Function")
    (ZEROP                                   "Function")
    (WITH-OPEN-FILE                          "Macro")
    (VARIABLE                                "Symbol")
    (STRUCTURE                               "Symbol")
    (STANDARD                                "Symbol")
    (SPEED                                   "Symbol")
    (SPACE                                   "Symbol")
    (SAFETY                                  "Symbol")
    (OTHERWISE                               "Symbol")
    (ENCODE-UNIVERSAL-TIME                   "Function")
    (COMPILER-MACRO                          "Symbol")
    (DEBUG                                   "Symbol")
    (COMPILATION-SPEED                       "Symbol")
    (&WHOLE                                  "Symbol")
    (&REST                                   "Symbol")
    (&OPTIONAL                               "Symbol")
    (&KEY                                    "Symbol")
    (&ENVIRONMENT                            "Symbol")
    (&BODY                                   "Symbol")
    (&AUX                                    "Symbol")
    (&ALLOW-OTHER-KEYS                       "Symbol")
    )) ;;*common-lisp-symbols*


(defconstant *common-lisp-exports*
  '(
    &ALLOW-OTHER-KEYS *PRINT-MISER-WIDTH* &AUX *PRINT-PPRINT-DISPATCH*
    &BODY *PRINT-PRETTY* &ENVIRONMENT *PRINT-RADIX* &KEY *PRINT-READABLY*
    &OPTIONAL *PRINT-RIGHT-MARGIN* &REST *QUERY-IO* &WHOLE *RANDOM-STATE*
    * *READ-BASE* ** *READ-DEFAULT-FLOAT-FORMAT* *** *READ-EVAL*
    *BREAK-ON-SIGNALS* *READ-SUPPRESS* *COMPILE-FILE-PATHNAME* *READTABLE*
    *COMPILE-FILE-TRUENAME* *STANDARD-INPUT* *COMPILE-PRINT*
    *STANDARD-OUTPUT* *COMPILE-VERBOSE* *TERMINAL-IO* *DEBUG-IO*
    *TRACE-OUTPUT* *DEBUGGER-HOOK* + *DEFAULT-PATHNAME-DEFAULTS* ++
    *ERROR-OUTPUT* +++ *FEATURES* - *GENSYM-COUNTER* / *LOAD-PATHNAME* //
    *LOAD-PRINT* /// *LOAD-TRUENAME* /= *LOAD-VERBOSE* 1+
    *MACROEXPAND-HOOK* 1- *MODULES* < *PACKAGE* <= *PRINT-ARRAY* =
    *PRINT-BASE* > *PRINT-CASE* >= *PRINT-CIRCLE* ABORT *PRINT-ESCAPE* ABS
    *PRINT-GENSYM* ACONS *PRINT-LENGTH* ACOS *PRINT-LEVEL* ACOSH
    *PRINT-LINES* ADD-METHOD ADJOIN ATOM BOUNDP ADJUST-ARRAY BASE-CHAR
    BREAK ADJUSTABLE-ARRAY-P BASE-STRING BROADCAST-STREAM
    ALLOCATE-INSTANCE BIGNUM BROADCAST-STREAM-STREAMS ALPHA-CHAR-P BIT
    BUILT-IN-CLASS ALPHANUMERICP BIT-AND BUTLAST AND BIT-ANDC1 BYTE APPEND
    BIT-ANDC2 BYTE-POSITION APPLY BIT-EQV BYTE-SIZE APROPOS BIT-IOR CAAAAR
    APROPOS-LIST BIT-NAND CAAADR AREF BIT-NOR CAAAR ARITHMETIC-ERROR
    BIT-NOT CAADAR ARITHMETIC-ERROR-OPERANDS BIT-ORC1 CAADDR
    ARITHMETIC-ERROR-OPERATION BIT-ORC2 CAADR ARRAY BIT-VECTOR CAAR
    ARRAY-DIMENSION BIT-VECTOR-P CADAAR ARRAY-DIMENSION-LIMIT BIT-XOR
    CADADR ARRAY-DIMENSIONS BLOCK CADAR ARRAY-DISPLACEMENT BOOLE CADDAR
    ARRAY-ELEMENT-TYPE BOOLE-1 CADDDR ARRAY-HAS-FILL-POINTER-P BOOLE-2
    CADDR ARRAY-IN-BOUNDS-P BOOLE-AND CADR ARRAY-RANK BOOLE-ANDC1
    CALL-ARGUMENTS-LIMIT ARRAY-RANK-LIMIT BOOLE-ANDC2 CALL-METHOD
    ARRAY-ROW-MAJOR-INDEX BOOLE-C1 CALL-NEXT-METHOD ARRAY-TOTAL-SIZE
    BOOLE-C2 CAR ARRAY-TOTAL-SIZE-LIMIT BOOLE-CLR CASE ARRAYP BOOLE-EQV
    CATCH ASH BOOLE-IOR CCASE ASIN BOOLE-NAND CDAAAR ASINH BOOLE-NOR
    CDAADR ASSERT BOOLE-ORC1 CDAAR ASSOC BOOLE-ORC2 CDADAR ASSOC-IF
    BOOLE-SET CDADDR ASSOC-IF-NOT BOOLE-XOR CDADR ATAN BOOLEAN CDAR ATANH
    BOTH-CASE-P CDDAAR CDDADR CLEAR-INPUT COPY-TREE CDDAR CLEAR-OUTPUT COS
    CDDDAR CLOSE COSH CDDDDR CLRHASH COUNT CDDDR CODE-CHAR COUNT-IF CDDR
    COERCE COUNT-IF-NOT CDR COMPILATION-SPEED CTYPECASE CEILING COMPILE
    DEBUG CELL-ERROR COMPILE-FILE DECF CELL-ERROR-NAME
    COMPILE-FILE-PATHNAME DECLAIM CERROR COMPILED-FUNCTION DECLARATION
    CHANGE-CLASS COMPILED-FUNCTION-P DECLARE CHAR COMPILER-MACRO
    DECODE-FLOAT CHAR-CODE COMPILER-MACRO-FUNCTION DECODE-UNIVERSAL-TIME
    CHAR-CODE-LIMIT COMPLEMENT DEFCLASS CHAR-DOWNCASE COMPLEX DEFCONSTANT
    CHAR-EQUAL COMPLEXP DEFGENERIC CHAR-GREATERP
    COMPUTE-APPLICABLE-METHODS DEFINE-COMPILER-MACRO CHAR-INT
    COMPUTE-RESTARTS DEFINE-CONDITION CHAR-LESSP CONCATENATE
    DEFINE-METHOD-COMBINATION CHAR-NAME CONCATENATED-STREAM
    DEFINE-MODIFY-MACRO CHAR-NOT-EQUAL CONCATENATED-STREAM-STREAMS
    DEFINE-SETF-EXPANDER CHAR-NOT-GREATERP COND DEFINE-SYMBOL-MACRO
    CHAR-NOT-LESSP CONDITION DEFMACRO CHAR-UPCASE CONJUGATE DEFMETHOD
    CHAR/= CONS DEFPACKAGE CHAR< CONSP DEFPARAMETER CHAR<= CONSTANTLY
    DEFSETF CHAR= CONSTANTP DEFSTRUCT CHAR> CONTINUE DEFTYPE CHAR>=
    CONTROL-ERROR DEFUN CHARACTER COPY-ALIST DEFVAR CHARACTERP COPY-LIST
    DELETE CHECK-TYPE COPY-PPRINT-DISPATCH DELETE-DUPLICATES CIS
    COPY-READTABLE DELETE-FILE CLASS COPY-SEQ DELETE-IF CLASS-NAME
    COPY-STRUCTURE DELETE-IF-NOT CLASS-OF COPY-SYMBOL DELETE-PACKAGE
    DENOMINATOR EQ DEPOSIT-FIELD EQL DESCRIBE EQUAL DESCRIBE-OBJECT EQUALP
    DESTRUCTURING-BIND ERROR DIGIT-CHAR ETYPECASE DIGIT-CHAR-P EVAL
    DIRECTORY EVAL-WHEN DIRECTORY-NAMESTRING EVENP DISASSEMBLE EVERY
    DIVISION-BY-ZERO EXP DO EXPORT DO* EXPT DO-ALL-SYMBOLS EXTENDED-CHAR
    DO-EXTERNAL-SYMBOLS FBOUNDP DO-SYMBOLS FCEILING DOCUMENTATION
    FDEFINITION DOLIST FFLOOR DOTIMES FIFTH DOUBLE-FLOAT FILE-AUTHOR
    DOUBLE-FLOAT-EPSILON FILE-ERROR DOUBLE-FLOAT-NEGATIVE-EPSILON
    FILE-ERROR-PATHNAME DPB FILE-LENGTH DRIBBLE FILE-NAMESTRING
    DYNAMIC-EXTENT FILE-POSITION ECASE FILE-STREAM ECHO-STREAM
    FILE-STRING-LENGTH ECHO-STREAM-INPUT-STREAM FILE-WRITE-DATE
    ECHO-STREAM-OUTPUT-STREAM FILL ED FILL-POINTER EIGHTH FIND ELT
    FIND-ALL-SYMBOLS ENCODE-UNIVERSAL-TIME FIND-CLASS END-OF-FILE FIND-IF
    ENDP FIND-IF-NOT ENOUGH-NAMESTRING FIND-METHOD
    ENSURE-DIRECTORIES-EXIST FIND-PACKAGE ENSURE-GENERIC-FUNCTION
    FIND-RESTART FIND-SYMBOL GET-INTERNAL-RUN-TIME FINISH-OUTPUT
    GET-MACRO-CHARACTER FIRST GET-OUTPUT-STREAM-STRING FIXNUM
    GET-PROPERTIES FLET GET-SETF-EXPANSION FLOAT GET-UNIVERSAL-TIME
    FLOAT-DIGITS GETF FLOAT-PRECISION GETHASH FLOAT-RADIX GO FLOAT-SIGN
    GRAPHIC-CHAR-P FLOATING-POINT-INEXACT HANDLER-BIND
    FLOATING-POINT-INVALID-OPERATION HANDLER-CASE FLOATING-POINT-OVERFLOW
    HASH-TABLE FLOATING-POINT-UNDERFLOW HASH-TABLE-COUNT FLOATP
    HASH-TABLE-P FLOOR HASH-TABLE-REHASH-SIZE FMAKUNBOUND
    HASH-TABLE-REHASH-THRESHOLD FORCE-OUTPUT HASH-TABLE-SIZE FORMAT
    HASH-TABLE-TEST FORMATTER HOST-NAMESTRING FOURTH IDENTITY FRESH-LINE
    IF FROUND IGNORABLE FTRUNCATE IGNORE FTYPE IGNORE-ERRORS FUNCALL
    IMAGPART FUNCTION IMPORT FUNCTION-KEYWORDS IN-PACKAGE
    FUNCTION-LAMBDA-EXPRESSION INCF FUNCTIONP INITIALIZE-INSTANCE GCD
    INLINE GENERIC-FUNCTION INPUT-STREAM-P GENSYM INSPECT GENTEMP INTEGER
    GET INTEGER-DECODE-FLOAT GET-DECODED-TIME INTEGER-LENGTH
    GET-DISPATCH-MACRO-CHARACTER INTEGERP GET-INTERNAL-REAL-TIME
    INTERACTIVE-STREAM-P INTERN LISP-IMPLEMENTATION-TYPE
    INTERNAL-TIME-UNITS-PER-SECOND LISP-IMPLEMENTATION-VERSION
    INTERSECTION LIST INVALID-METHOD-ERROR LIST* INVOKE-DEBUGGER
    LIST-ALL-PACKAGES INVOKE-RESTART LIST-LENGTH
    INVOKE-RESTART-INTERACTIVELY LISTEN ISQRT LISTP KEYWORD LOAD KEYWORDP
    LOAD-LOGICAL-PATHNAME-TRANSLATIONS LABELS LOAD-TIME-VALUE LAMBDA
    LOCALLY LAMBDA-LIST-KEYWORDS LOG LAMBDA-PARAMETERS-LIMIT LOGAND LAST
    LOGANDC1 LCM LOGANDC2 LDB LOGBITP LDB-TEST LOGCOUNT LDIFF LOGEQV
    LEAST-NEGATIVE-DOUBLE-FLOAT LOGICAL-PATHNAME LEAST-NEGATIVE-LONG-FLOAT
    LOGICAL-PATHNAME-TRANSLATIONS LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT
    LOGIOR LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT LOGNAND
    LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT LOGNOR
    LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT LOGNOT
    LEAST-NEGATIVE-SHORT-FLOAT LOGORC1 LEAST-NEGATIVE-SINGLE-FLOAT LOGORC2
    LEAST-POSITIVE-DOUBLE-FLOAT LOGTEST LEAST-POSITIVE-LONG-FLOAT LOGXOR
    LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT LONG-FLOAT
    LEAST-POSITIVE-NORMALIZED-LONG-FLOAT LONG-FLOAT-EPSILON
    LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT LONG-FLOAT-NEGATIVE-EPSILON
    LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT LONG-SITE-NAME
    LEAST-POSITIVE-SHORT-FLOAT LOOP LEAST-POSITIVE-SINGLE-FLOAT
    LOOP-FINISH LENGTH LOWER-CASE-P LET MACHINE-INSTANCE LET* MACHINE-TYPE
    MACHINE-VERSION MASK-FIELD MACRO-FUNCTION MAX MACROEXPAND MEMBER
    MACROEXPAND-1 MEMBER-IF MACROLET MEMBER-IF-NOT MAKE-ARRAY MERGE
    MAKE-BROADCAST-STREAM MERGE-PATHNAMES MAKE-CONCATENATED-STREAM METHOD
    MAKE-CONDITION METHOD-COMBINATION MAKE-DISPATCH-MACRO-CHARACTER
    METHOD-COMBINATION-ERROR MAKE-ECHO-STREAM METHOD-QUALIFIERS
    MAKE-HASH-TABLE MIN MAKE-INSTANCE MINUSP MAKE-INSTANCES-OBSOLETE
    MISMATCH MAKE-LIST MOD MAKE-LOAD-FORM MOST-NEGATIVE-DOUBLE-FLOAT
    MAKE-LOAD-FORM-SAVING-SLOTS MOST-NEGATIVE-FIXNUM MAKE-METHOD
    MOST-NEGATIVE-LONG-FLOAT MAKE-PACKAGE MOST-NEGATIVE-SHORT-FLOAT
    MAKE-PATHNAME MOST-NEGATIVE-SINGLE-FLOAT MAKE-RANDOM-STATE
    MOST-POSITIVE-DOUBLE-FLOAT MAKE-SEQUENCE MOST-POSITIVE-FIXNUM
    MAKE-STRING MOST-POSITIVE-LONG-FLOAT MAKE-STRING-INPUT-STREAM
    MOST-POSITIVE-SHORT-FLOAT MAKE-STRING-OUTPUT-STREAM
    MOST-POSITIVE-SINGLE-FLOAT MAKE-SYMBOL MUFFLE-WARNING
    MAKE-SYNONYM-STREAM MULTIPLE-VALUE-BIND MAKE-TWO-WAY-STREAM
    MULTIPLE-VALUE-CALL MAKUNBOUND MULTIPLE-VALUE-LIST MAP
    MULTIPLE-VALUE-PROG1 MAP-INTO MULTIPLE-VALUE-SETQ MAPC
    MULTIPLE-VALUES-LIMIT MAPCAN NAME-CHAR MAPCAR NAMESTRING MAPCON
    NBUTLAST MAPHASH NCONC MAPL NEXT-METHOD-P MAPLIST NIL NINTERSECTION
    PACKAGE-ERROR NINTH PACKAGE-ERROR-PACKAGE NO-APPLICABLE-METHOD
    PACKAGE-NAME NO-NEXT-METHOD PACKAGE-NICKNAMES NOT
    PACKAGE-SHADOWING-SYMBOLS NOTANY PACKAGE-USE-LIST NOTEVERY
    PACKAGE-USED-BY-LIST NOTINLINE PACKAGEP NRECONC PAIRLIS NREVERSE
    PARSE-ERROR NSET-DIFFERENCE PARSE-INTEGER NSET-EXCLUSIVE-OR
    PARSE-NAMESTRING NSTRING-CAPITALIZE PATHNAME NSTRING-DOWNCASE
    PATHNAME-DEVICE NSTRING-UPCASE PATHNAME-DIRECTORY NSUBLIS
    PATHNAME-HOST NSUBST PATHNAME-MATCH-P NSUBST-IF PATHNAME-NAME
    NSUBST-IF-NOT PATHNAME-TYPE NSUBSTITUTE PATHNAME-VERSION
    NSUBSTITUTE-IF PATHNAMEP NSUBSTITUTE-IF-NOT PEEK-CHAR NTH PHASE
    NTH-VALUE PI NTHCDR PLUSP NULL POP NUMBER POSITION NUMBERP POSITION-IF
    NUMERATOR POSITION-IF-NOT NUNION PPRINT ODDP PPRINT-DISPATCH OPEN
    PPRINT-EXIT-IF-LIST-EXHAUSTED OPEN-STREAM-P PPRINT-FILL OPTIMIZE
    PPRINT-INDENT OR PPRINT-LINEAR OTHERWISE PPRINT-LOGICAL-BLOCK
    OUTPUT-STREAM-P PPRINT-NEWLINE PACKAGE PPRINT-POP PPRINT-TAB READ-CHAR
    PPRINT-TABULAR READ-CHAR-NO-HANG PRIN1 READ-DELIMITED-LIST
    PRIN1-TO-STRING READ-FROM-STRING PRINC READ-LINE PRINC-TO-STRING
    READ-PRESERVING-WHITESPACE PRINT READ-SEQUENCE PRINT-NOT-READABLE
    READER-ERROR PRINT-NOT-READABLE-OBJECT READTABLE PRINT-OBJECT
    READTABLE-CASE PRINT-UNREADABLE-OBJECT READTABLEP PROBE-FILE REAL
    PROCLAIM REALP PROG REALPART PROG* REDUCE PROG1 REINITIALIZE-INSTANCE
    PROG2 REM PROGN REMF PROGRAM-ERROR REMHASH PROGV REMOVE PROVIDE
    REMOVE-DUPLICATES PSETF REMOVE-IF PSETQ REMOVE-IF-NOT PUSH
    REMOVE-METHOD PUSHNEW REMPROP QUOTE RENAME-FILE RANDOM RENAME-PACKAGE
    RANDOM-STATE REPLACE RANDOM-STATE-P REQUIRE RASSOC REST RASSOC-IF
    RESTART RASSOC-IF-NOT RESTART-BIND RATIO RESTART-CASE RATIONAL
    RESTART-NAME RATIONALIZE RETURN RATIONALP RETURN-FROM READ REVAPPEND
    READ-BYTE REVERSE ROOM SIMPLE-BIT-VECTOR ROTATEF SIMPLE-BIT-VECTOR-P
    ROUND SIMPLE-CONDITION ROW-MAJOR-AREF
    SIMPLE-CONDITION-FORMAT-ARGUMENTS RPLACA
    SIMPLE-CONDITION-FORMAT-CONTROL RPLACD SIMPLE-ERROR SAFETY
    SIMPLE-STRING SATISFIES SIMPLE-STRING-P SBIT SIMPLE-TYPE-ERROR
    SCALE-FLOAT SIMPLE-VECTOR SCHAR SIMPLE-VECTOR-P SEARCH SIMPLE-WARNING
    SECOND SIN SEQUENCE SINGLE-FLOAT SERIOUS-CONDITION
    SINGLE-FLOAT-EPSILON SET SINGLE-FLOAT-NEGATIVE-EPSILON SET-DIFFERENCE
    SINH SET-DISPATCH-MACRO-CHARACTER SIXTH SET-EXCLUSIVE-OR SLEEP
    SET-MACRO-CHARACTER SLOT-BOUNDP SET-PPRINT-DISPATCH SLOT-EXISTS-P
    SET-SYNTAX-FROM-CHAR SLOT-MAKUNBOUND SETF SLOT-MISSING SETQ
    SLOT-UNBOUND SEVENTH SLOT-VALUE SHADOW SOFTWARE-TYPE SHADOWING-IMPORT
    SOFTWARE-VERSION SHARED-INITIALIZE SOME SHIFTF SORT SHORT-FLOAT SPACE
    SHORT-FLOAT-EPSILON SPECIAL SHORT-FLOAT-NEGATIVE-EPSILON
    SPECIAL-OPERATOR-P SHORT-SITE-NAME SPEED SIGNAL SQRT SIGNED-BYTE
    STABLE-SORT SIGNUM STANDARD SIMPLE-ARRAY STANDARD-CHAR
    SIMPLE-BASE-STRING STANDARD-CHAR-P STANDARD-CLASS SUBLIS
    STANDARD-GENERIC-FUNCTION SUBSEQ STANDARD-METHOD SUBSETP
    STANDARD-OBJECT SUBST STEP SUBST-IF STORAGE-CONDITION SUBST-IF-NOT
    STORE-VALUE SUBSTITUTE STREAM SUBSTITUTE-IF STREAM-ELEMENT-TYPE
    SUBSTITUTE-IF-NOT STREAM-ERROR SUBTYPEP STREAM-ERROR-STREAM SVREF
    STREAM-EXTERNAL-FORMAT SXHASH STREAMP SYMBOL STRING SYMBOL-FUNCTION
    STRING-CAPITALIZE SYMBOL-MACROLET STRING-DOWNCASE SYMBOL-NAME
    STRING-EQUAL SYMBOL-PACKAGE STRING-GREATERP SYMBOL-PLIST
    STRING-LEFT-TRIM SYMBOL-VALUE STRING-LESSP SYMBOLP STRING-NOT-EQUAL
    SYNONYM-STREAM STRING-NOT-GREATERP SYNONYM-STREAM-SYMBOL
    STRING-NOT-LESSP T STRING-RIGHT-TRIM TAGBODY STRING-STREAM TAILP
    STRING-TRIM TAN STRING-UPCASE TANH STRING/= TENTH STRING< TERPRI
    STRING<= THE STRING= THIRD STRING> THROW STRING>= TIME STRINGP TRACE
    STRUCTURE TRANSLATE-LOGICAL-PATHNAME STRUCTURE-CLASS
    TRANSLATE-PATHNAME STRUCTURE-OBJECT TREE-EQUAL STYLE-WARNING TRUENAME 
    TRUNCATE VALUES-LIST TWO-WAY-STREAM VARIABLE
    TWO-WAY-STREAM-INPUT-STREAM VECTOR TWO-WAY-STREAM-OUTPUT-STREAM
    VECTOR-POP TYPE VECTOR-PUSH TYPE-ERROR VECTOR-PUSH-EXTEND
    TYPE-ERROR-DATUM VECTORP TYPE-ERROR-EXPECTED-TYPE WARN TYPE-OF WARNING
    TYPECASE WHEN TYPEP WILD-PATHNAME-P UNBOUND-SLOT WITH-ACCESSORS
    UNBOUND-SLOT-INSTANCE WITH-COMPILATION-UNIT UNBOUND-VARIABLE
    WITH-CONDITION-RESTARTS UNDEFINED-FUNCTION WITH-HASH-TABLE-ITERATOR
    UNEXPORT WITH-INPUT-FROM-STRING UNINTERN WITH-OPEN-FILE UNION
    WITH-OPEN-STREAM UNLESS WITH-OUTPUT-TO-STRING UNREAD-CHAR
    WITH-PACKAGE-ITERATOR UNSIGNED-BYTE WITH-SIMPLE-RESTART UNTRACE
    WITH-SLOTS UNUSE-PACKAGE WITH-STANDARD-IO-SYNTAX UNWIND-PROTECT WRITE
    UPDATE-INSTANCE-FOR-DIFFERENT-CLASS WRITE-BYTE
    UPDATE-INSTANCE-FOR-REDEFINED-CLASS WRITE-CHAR
    UPGRADED-ARRAY-ELEMENT-TYPE WRITE-LINE UPGRADED-COMPLEX-PART-TYPE
    WRITE-SEQUENCE UPPER-CASE-P WRITE-STRING USE-PACKAGE WRITE-TO-STRING
    USE-VALUE Y-OR-N-P USER-HOMEDIR-PATHNAME YES-OR-NO-P VALUES ZEROP
    )) ;;*common-lisp-exports*


(defun up-down-case (sym) 
  "
RETURN: A list containg the name of sym in down case and in up case.
"
  (list (string-downcase (symbol-name sym)) 
        (string-upcase   (symbol-name sym))))


(defconst +separator-regexp+ "[ \"',`()\n\t]")

(defun token-regexp (regexp)
  "
RETURN: A regexp that match the given REGEXP, but
        only if standing alone as a single lisp token.
"
  (format "\\<\\(%s\\)\\>" regexp))


;; MATCH-ANCHORED in C-h v font-lock-keywords
(defun clfl-nop   () nil)
(defun clfl-front () (message "clfl-front") (goto-char (match-beginning 0)))
(defun clfl-back  () (message "clfl-back")  (goto-char (match-end 1)))


(defun common-lisp-font-lock ()
  (mapcan
   (lambda (kf)
     (let* ((kind (kf-kind kf))
            (lock (kf-lock kf))
            (face (kf-face kf))
            (symbols (mapcar (function first)
                             (remove* kind *common-lisp-symbols*
                                      :test (function cl:string/=)
                                      :key  (function second)))))
       (labels ((format-regexp-in (tree)
                  (cond
                    ((and (stringp tree)  (string-match "%s" tree))
                     (format tree
                       (token-regexp
                        (regexp-opt
                         (mapcar (lambda (x) (string-upcase (symbol-name x))) symbols)
                         ;; (mapcan (function up-down-case) symbols)
                         'words))))
                    ((consp tree) (cons (format-regexp-in (car tree))
                                        (format-regexp-in (cdr tree))))
                    (t tree)))
                (some-format-p (tree)
                  (cond
                    ((and (stringp tree)  (string-match "%s" tree)) t)
                    ((consp tree) (or (some-format-p (car tree))
                                      (some-format-p (cdr tree))))
                    (t nil))))
         (cond
           (symbols
            (setf lock (copy-seq (or lock `("%s" (0 ,face t)))))
            ;;             ("%s" (goto-char (match-beginning 0))
            ;;                   (goto-char (match-end 1))
            ;;                   ;;,(function clfl-front)
            ;;                   ;;,(function clfl-back)
            ;;                   (1 ,face t))))))
            (list (format-regexp-in lock)))
           ((null lock)
            ;; "Warning" 
            ;; (error "No lock and no symbols")
            nil)
           ((some-format-p (car lock))
            (error "No symbol to fill %%s in lock regexp"))
           (t
            (list lock)) ))))
   *kind-to-face-map*))


(defvar *common-lisp-font-lock-cache* '(:undef :undef nil))
(defvar *common-lisp-font-lock-keywords* '())

(defun common-lisp-font-lock-keywords ()
  (third
   (if (and (eq window-system (first  *common-lisp-font-lock-cache*))
            (eq pretty-greek  (second *common-lisp-font-lock-cache*)))
       *common-lisp-font-lock-cache*
       (setf *common-lisp-font-lock-cache*
             (list
              window-system
              pretty-greek
              (nconc
               (when (and  (eq 'x window-system) pretty-greek)
                 (greek-letter-font-lock))
               (copy-list
                `(
                  (,(format "(%s[ 	']*\\(\\sw+\\)?" 
                            (regexp-opt
                             (mapcar (function cl:string)
                                     '(catch throw block return-from))
                             ;; (mapcan (function up-down-case)
                             ;;        '(catch throw block return-from))
                             'words))
                    (1 font-lock-cl-special-operator-face t)
                    (2 font-lock-constant-face nil t))
                  (,(format "(%s[ 	']*\\(\\sw+\\)?" 
                            (regexp-opt
                             (mapcar (function cl:string)
                                     '(provide require))
                             ;; (mapcan (function up-down-case) '(provide require))
                             'words))
                    (1 font-lock-cl-function-face t)
                    (2 font-lock-constant-face nil t)) 
                  (,(format "(%s" 
                            (regexp-opt
                             (mapcar (function cl:string)
                                     '(abort assert error signal))
                             ;; (mapcan (function up-down-case)
                             ;;         '(abort assert error signal)) 
                             'words))
                    (1 font-lock-cl-warning-face t))))
               (common-lisp-font-lock)))))))

;; (remove* 'font-lock-cl-string-face *common-lisp-font-lock-keywords*
;;  :test (lambda (x y) (not (eq x y))) :key (lambda (x) (second (second x))))

(defun common-lisp-font-lock-hook ()
  (interactive)
  (setf *common-lisp-font-lock-keywords* (common-lisp-font-lock-keywords))
  (setq font-lock-defaults
        (list '*common-lisp-font-lock-keywords*
              nil                       ; keyword only
              t                         ; case fold
              '(("!#$%&*+,-./:<=>?@[]^_{}~ÃÂ¡ÃÂ¢ÃÂ£ÃÂ¤ÃÂ¥ÃÂ¦ÃÂ§ÃÂ¨ÃÂ©ÃÂªÃÂ«ÃÂ¬ÃÂ­ÃÂ®ÃÂ¯ÃÂ°ÃÂ±ÃÂ²ÃÂ³ÃÂ´ÃÂµÃÂ¶ÃÂ·ÃÂ¸ÃÂ¹ÃÂºÃÂ»ÃÂ¼ÃÂ½ÃÂ¾ÃÂ¿ÃÂÃÂ·"
                 . "w"))))
  (setq font-lock-keywords
        *common-lisp-font-lock-keywords*))

;;;; THE END ;;;;
