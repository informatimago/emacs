;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-objc-genel
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Generate some Objective-C code.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pbourguignon@dxo.com>
;;;;MODIFICATIONS
;;;;    2012-11-12 <PJB> Created.
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

(require 'pjb-cl)

(defun ensure-string (object)
  (typecase object
    (string object)
    (symbol (symbol-name object))
    (list   (apply 'string object))
    (t      (prin1-to-string object))))


(defun make-set-selector (selector)
  "Return a setSelector: selector."
  (let ((name (ensure-string selector)))
    (intern (if (and (< 2 (length name))
		     (string= "is" (substring name 0 2))
		     (upper-case-p (aref name 2)))
		(format "set%s:" (subseq name 2))
		(format "set%c%s:" (char-upcase (aref name 0)) (subseq name 1))))))

(defun split-selector (selector)
  (let* ((parts           (reverse (split-string (ensure-string selector) ":")))
	 (parts-with-args (mapcar (lambda (part) (format "%s:" part)) (rest parts))))
    (mapcar (function intern)
	    (nreverse (if (string= "" (first parts))
			  parts-with-args
			  (cons (first parts)parts-with-args))))))

(defun test/split-selector ()
  (assert (equal (split-selector 'isObscure) '(isObscure)))
  (assert (equal (split-selector 'setObscure:) '(setObscure:)))
  (assert (equal (split-selector 'initWithCoordinates::) '(initWithCoordinates: :)))
  (assert (equal (split-selector 'initWithFrame:andObject:) '(initWithFrame: andObject:)))
  :success)



(defun generate (node)
  (if (typep node 'c-node)
      (generate-node node)
      (insert (format "%S" node))))


(defmacro* defclass/c (class-name (&rest superclasses) (&rest slots) &rest options)
  `(progn
     (defclass ,class-name ,superclasses ,slots ,@options)
     (defun* ,(intern (format "make-%s" class-name)) (&rest arguments &key &allow-other-keys)
       (apply 'make-instance ',class-name arguments))
     ',class-name))


(defmacro* with-parens (parens &body body)
  `(progn
     (insert ,(elt parens 0))
     (prog1 (progn ,@body)
       (insert ,(elt parens 1)))))




(defclass c-node ()
  ())

(defmethod generate-node ((node c-node))
  (with-parens ("/*" "*/\n")
    (insert (format "ERROR: Missing generate-node method for %S" node))))


(defclass c-declaration (c-node)
  ())

(defclass c-definition (c-node)
  ())

(defclass c-statement (c-node)
  ())

(defclass c-expression (c-node)
  ())


(defclass/c c-dot    (c-expression)
  ((object :initarg :object :accessor c-dot-object)
   (slot   :initarg :slot :accessor c-dot-slot)))

(defmethod generate-node ((node c-dot))
  (insert (format "%s.%s" (c-dot-object node) (c-dot-slot node))))


(defclass/c c-assign    (c-expression)
  ((lval :initarg :lval :accessor c-assign-lval)
   (rval :initarg :rval :accessor c-assign-rval)))

(defmethod generate-node ((node c-assign))
  (generate (c-assign-lval node))
  (insert " = ")
  (generate (c-assign-rval node)))


(defclass/c c-cond   (c-statement)
  ((clauses :initarg :clauses :accessor c-cond-clauses
            :documentation "A list, the first element is the condition expression, the rest is a list of statement body."))) 

(defmethod generate-node ((node c-cond))
  (loop
     for insert-else = nil then t
     for (condition . body) in (c-cond-clauses node)
     do (progn
          (when insert-else
            (insert "else "))
          (with-parens ("if(" ")\n")
            (generate condition))
          (generate (make-c-block :statements body)))))


(defclass/c c-return (c-statement)
  ((result :initarg :result :accessor c-return-result)))

(defmethod generate-node ((node c-return))
  (with-parens ("return " "")
    (generate (c-return-result node))))



(defclass/c c-block (c-statement)
  ((statements :initarg :statements :accessor c-block-statements)))

(defmethod generate-node ((node c-block))
  (with-parens ("{\n"  "}\n")
    (dolist (statement (c-block-statements node))
      (generate statement)
      (insert ";\n"))))




(defclass objc-definition (c-definition)
  ())

(defclass objc-declaration (c-declaration)
  ())

(defclass objc-expression (c-expression)
  ())





(defun generate-intermingled-selector-and-things (selector things)
  "Inserts a method signature, or a message sending.
`selector': an Objective-C selector designator.
`things': a list of objc-parameter or objc-argument.
"
  (loop
     with selector-parts = (split-selector selector)
     with arguments = things
     for sep = "" then " "
     while selector-parts
     do (progn
          (insert sep)
	  (generate (pop selector-parts))
	  (when arguments
	    (generate (pop arguments))))
     finally (loop while arguments
		do (insert "," (pop arguments)))))



(defclass/c objc-string (objc-expression)
  ((string :initarg :string :accessor objc-string-string)))

(defmethod generate-node ((node objc-string))
  (let ((string (objc-string-string node)))
   (insert (format "@%S" (typecase string
                           (string string)
                           (symbol (symbol-name string))
                           (t      (prin1-to-string string)))))))


(defclass/c objc-method (objc-definition)
  ((instance-method :initform t :reader objc-method-instance-method-p)
   (result-type :initform 'void :initarg :result-type :accessor objc-method-result-type)
   (selector :initarg :selector :accessor objc-method-selector)
   (parameters :initform '() :initarg :parameters :accessor objc-method-parameters)
   (body :initform '() :initarg :body :accessor objc-method-body)))

(defclass/c objc-class-method (objc-method)
  ((instance-method :initform nil :reader objc-method-instance-method-p)))

(defmethod objc-method-sign ((node objc-method))       "-")
(defmethod objc-method-sign ((node objc-class-method)) "+")

(defmethod generate-node ((node objc-method))
  (insert (format "\n\n%s " (objc-method-sign node)))
  (with-parens ("(" ")")
    (insert (ensure-string (objc-method-result-type node))))
  (generate-intermingled-selector-and-things (objc-method-selector node) (objc-method-parameters node))
  (insert "\n")
  (generate (make-c-block :statements (objc-method-body node))))





(defclass/c objc-send (objc-expression)
  ((recipient :initarg :recipient :accessor objc-send-recipient)
   (selector :initarg :selector :accessor objc-send-selector)
   (arguments :initform '() :initarg :arguments :accessor objc-send-arguments)))

(defmethod generate-node ((node objc-send))
  (with-parens ("[" "]")
    (generate (objc-send-recipient node))
    (insert " ")
    (generate-intermingled-selector-and-things (objc-send-selector node) (objc-send-arguments node))))



(defclass/c objc-parameter (objc-declaration)
  ((type :initarg :type :accessor objc-parameter-type)
   (name :initarg :name :accessor objc-parameter-name)))

(defmethod generate-node ((node objc-parameter))
  (insert (format "(%s)%s " (objc-parameter-type node) (objc-parameter-name node))))






(defun generate-forward-methods (properties target)
  "Inserts methods for the given properties that forward to the give target.
`properties': a list of property descriptor. Each one is a list
containing a type, and a property name or a list (getter setter).
`target': a target object to which the messages are forwarded.
"
  (loop
     for (type prop-name) in properties
     do (let ((getter (if (atom prop-name)
			  prop-name
			  (first prop-name)))
	      (setter (if (atom prop-name)
			  (make-set-selector prop-name)
			  (second prop-name))))
	  (generate (make-objc-method :result-type type
				      :selector getter
				      :parameters '()
				      :body (list (make-c-return :result (make-objc-send :recipient target
											 :selector getter)))))
	  (generate (make-objc-method :result-type 'void
				      :selector setter
				      :parameters (list (make-objc-parameter :type type :name 'value))
				      :body (list (make-objc-send :recipient target
								  :selector setter
								  :arguments '(value))))))))






(defun test/pjb-objc-gen ()
 (test/split-selector))

(test/pjb-objc-gen)

(provide 'pjb-objc-gen)
;;;; THE END ;;;;

