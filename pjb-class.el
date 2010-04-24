;;;;******************************************************************************
;;;;FILE:               pjb-class.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    Some stuff for classes.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon 
;;;;MODIFICATIONS
;;;;    2002-02-19 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2002 - 2002
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
(require 'pjb-list)

(provide 'pjb-class)



(defun responds-to (class method)
  "
RETURN: Whether the CLASS responds to the METHOD, 
        either directly or by inheritance.
"
  (if (method-implem class method)
      t
    (let ((parents (class-parents class))
          (result nil))
      (while parents
        (if (responds-to (car parents) method)
            (setq result t parents nil)
          (setq parents (cdr parents))))
      result))
  );;responds-to


(defun all-classes ()
  "
RETURN: a list with all symbols that are class names.
"
  (let ( (classes nil) )
    (mapatoms (lambda (sym) 
                (when (class-p sym)
                  (push sym classes))))
    classes)
  );;all-classes


;;(defun documentation (class) ...)


(defun attributes (class)
  "
RETURN: a list with all the slots of the given CLASS.
        Each entry contains a list with (name type default prot doc).
"
  (let* ((cv     (class-v class))
         (chk-cv (if (null cv) (error "Unknown class '%s'." class) nil))
         (names  (aref cv class-public-a))
         (types  (aref cv class-public-type))
         (deflt  (aref cv class-public-d))
         (prot   (aref cv class-protection))
         (docs   (aref cv class-public-doc))
         (i      0)
         (result nil)
         )
    (while names
      (push (list 
             (car names) 
             (if types (if (eq t (aref types i)) nil (aref types i)) nil)
             (car deflt) 
             (if (car prot) 'private 'public) 
             (car docs) )
            result)
      (setq names (cdr names)
            docs  (cdr docs)
            deflt (cdr deflt)
            prot  (cdr prot)
            i     (1+ i))
      );;while
    result
    );;let
  );;attributes


(defun methods (class)
  "
RETURN: a list with all symbols that are methods of the given CLASS.
"
  (eieio-all-generic-functions class)
  );;methods


(defun method-implem (class method)
  "
RETURN: a list with tree entries defining the method implementation.
        (before primary after), with each containing (signature doc func).
"
  (let ( (tree (get method 'eieio-method-obarray) )
         (cn   (symbol-name class))
         before primary after)
    (if (not tree)
        nil
      ;; A symbol might be interned for that class in one of
      ;; these three slots in the method-obarray.
      (setq before  (intern-soft cn (aref tree 0))
            primary (intern-soft cn (aref tree 1))
            after   (intern-soft cn (aref tree 2)))
      (if (not (or (fboundp before)
                   (fboundp primary)
                   (fboundp after)))
          nil

        (list (if (fboundp before)
                  (list (eieio-lambda-arglist before)
                        (documentation before)
                        (symbol-function before))
                nil)
              (if (fboundp primary)
                  (list (eieio-lambda-arglist primary)
                        (documentation   primary)
                        (symbol-function primary))
                nil)
              (if (fboundp after)
                  (list (eieio-lambda-arglist after)
                        (documentation   after)
                        (symbol-function after))
                nil)
              ))))
  );;method-implem


(defun class-data (class)
  "
RETURN: a list containing : 
             - the class name,
             - a list of attributes as returned by (attributes class),
             - a list of methods as (method-name signature bpa).
"
    (list
     class
     (mapcar (lambda (att) (butlast att))
             (attributes class))

     (nremove-nil
      (apply 'append
             (mapcar (lambda (met)
                       (let ( (bpa-labels '(before primary after))
                              (bpa-label) )
                         (mapcar (lambda (bpa)
                                   (setq bpa-label  (car bpa-labels)
                                         bpa-labels (cdr bpa-labels))
                                   (when bpa
                                     (list met (car bpa) bpa-label)))
                                 (method-implem class met))))
                     (methods class))))
     );;list
    );;class-data


(defun format-quote (arg)
  "
RETURN:  if arg is (quote atom) or 'atom 
         then return \"'atom\" otherwise return (format \"%S\" arg).
"

  (cond
   ((and arg (atom arg) (not (stringp arg)) (not (numberp arg)))
    (format "'%s" arg))
   ((and (consp arg) 
         (= 2 (length arg))
         (eq 'quote (car arg)) )
    (format "'%s" (cadr arg)))
   (t
    (format "%S" arg)))
  );;format-quote


(defun class-display (class &optional return-list)
  "
RETURN: An ASCII-art diagram of the CLASS.
"
  (let* ( (cd        (class-data class)) 
          (name      (symbol-name (car cd)))
          (maxlen    (length name)) 
          (att-lines (sort
                      (mapcar 
                       (lambda (att)
                         (format 
                          "%s%s%s%s" 
                          (if (eq 'public (nth 3 att)) "+ " "- ")
                          (car att)
                          (if (nth 1 att) (format " : %s" (nth 1 att)) "")
                          (if (nth 2 att) 
                              (format " = %s" (format-quote (nth 2 att))) "")
                          ))
                       (nth 1 cd))
                      'string-lessp))
          (met-lines (sort
                      (mapcar 
                      (lambda (met)
                        (format 
                         "%s%s%s%s"
                         "+ "
                         (car met)
                         (let ( (args (if (or (eq 'self (car (nth 1 met)))
                                              (eq 'this (car (nth 1 met))))
                                          (cdr (nth 1 met))
                                        (nth 1 met)))
                                (fargs) )
                           (if args 
                               (let ((fargs (format "%S" args)))
                                 (if (< 44 (length fargs))
                                     "(...)" fargs))
                             "()"))
                         (if (eq 'primary (nth 2 met)) 
                             "" (format "{%s}" (nth 2 met)))
                        ))
                      (nth 2 cd))
                      'string-lessp))
          )
    (setq maxlen (apply 'max (mapcar (lambda (s) (length s))
                                     (append (list name) att-lines met-lines))))
    (setq line (format "+-%s-+\n" (make-string maxlen ?-)))
    (apply (if return-list 'list 'concat)
           (append 
            (list
             line
             (let* ( (left  (/ (- maxlen (length name)) 2))
                     (right (- maxlen left (length name))) )
               (format "| %s%s%s |\n" 
                       (make-string left 32) name (make-string right 32)))
             line
             )
            (mapcar (lambda (l) 
                      (format "| %s%s |\n" 
                              l (make-string (- maxlen (length l)) 32)))
                    att-lines)
            (list line)
            (mapcar (lambda (l) 
                      (format "| %s%s |\n" 
                              l (make-string (- maxlen (length l)) 32)))
                    met-lines)
            (list line)
            ))
    );;let
  );;class-display



(defun class-hierarchy-display (class &optional left)
  "
RETURN: a formated string with classes.
"
  (let ( (children (class-children class))
         (indentf  "%s")
         (above    t) )

    (setq left (if left left ""))
    (setq indentf (format "%s%%s" left))


    (apply 
     'concat 
     (append
      (mapcar (lambda (l) (format indentf l)) (class-display class t))
      (when children
        (append
         (list (format indentf "       |\n")
               (format indentf "      / \\\n")
               (format indentf "     /___\\\n")
               (format indentf "       |\n") 
               )
         (if (= 1 (length children))
             (list (class-hierarchy-display (car children) left) )
           (let ( (first (list
                          (format indentf "+------+\n")
                          (format indentf "|      |\n")
                          (class-hierarchy-display (car children) 
                                                   (format indentf "|   "))) )
                  (mid   (apply 
                          'append
                          (mapcar (lambda (child)
                                    (list
                                     (format indentf "|\n")
                                     (format indentf "+------+\n")
                                     (format indentf "|      |\n")
                                     (class-hierarchy-display 
                                      child (format indentf "|   "))) )
                                  (butlast (cdr children)))) )
                 
                  (laste (list
                          (format indentf "|\n")
                          (format indentf "+------+\n")
                          (format indentf "       |\n")
                          (class-hierarchy-display (car (last children))
                                                   (format indentf "    "))) ))
             (append first mid laste))
           ))))))
  );;class-hierarchy-display

  
(defun is-subclass-of (subclass class)
  "
RETURN: Whether `subclass' is a subclass (or sub*class) of `class'.
"
  (let ( (checked '())
         (parents (class-parents subclass))
         (current)
         (result nil) )
    (while parents 
      (setq current (car parents)
            parents (cdr parents))
      (unless (memq current checked)
        (if (eq current class)
            ;; found
            (setq result  t
                  parents nil)
          (progn
            (push current checked)
            (dolist (newparent (class-parents current))
              (unless (memq newparent parents) (push newparent parents))
              );;dolist
            )
          ));;unless
      );;while
    result)
  );;is-subclass-of


(defun is-kind-of (object class)
  "
RETURN: Whether the class of the `object'  is `class', 
        or the class of the `object' is a subclass of `class'.
"
  (let ((class-of-object (class-of object)))
    (or (eq class-of-object class) 
        (is-subclass-of class-of-object class)))
  );;is-kind-of


;;; (eval-when nil
;;;     (defclass M () () ())
;;;     (defclass A () () ())
;;;     (defclass B1 (A) () ())
;;;     (defclass C1 (B1) () ())
;;;     (defclass D1 (C1) () ())
;;;     (defclass B2 (A) () ())
;;;     (defclass C2 (B2) () ())
;;;     (defclass D2 (C2) () ())
;;;     (defclass C12 (B1 B2) () ())
;;;     (defclass E12 (D1 D2) () ())
;;;     (defclass E1M (D1 M)  () ())
;;;     (mapcar (lambda (predicat)
;;;               (unless (functionp predicat)
;;;                 (error "Expecting a function."))
;;;               (let ( (cl '(M A B1 C1 D1 B2 C2 D2 C12 E12 E1M)) 
;;;                      line )
;;;                 ;; line 0 -------------------
;;;                 (setq line  "+----+")
;;;                 (dotimes (i (length cl)) (setq line (concat line "----+")))
;;;                 (printf "%s\n" line)
;;;                 ;; line 1
;;;                 (printf "|    |") 
;;;                 (dolist (b cl) (printf "%3s |" (symbol-name b)))  
;;;                 (printf "\n%s\n" line)
;;;                 ;; array
;;;                 (dolist (a cl)
;;;                   (printf "|%3s |" (symbol-name a))
;;;                   (dolist (b cl) 
;;;                     (printf "%3s |" 
;;;                             (if (funcall predicat  a b)
;;;                                 "IS" "no")))
;;;                   (printf "\n%s\n" line))
;;;                 )
;;;               )
;;;             (list (function is-subclass-of)
;;;                   ;; We don't have subtypep... (function subtypep)
;;;                   ))
;;;     )



;;;; pjb-class.el                     -- 2003-01-29 03:59:14 -- pascal   ;;;;

