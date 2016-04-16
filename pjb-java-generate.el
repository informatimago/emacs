(defparameter *dirpath* "~/src/Android-SDK/ubudu-sdk/src/com/ubudu/sdk/dto/")

(defparameter *java-current-package* 'com.example)
(defparameter *java-operators* '(+ - * / < > <= >= == ! && ||))

(defparameter *java-predefined-classes* '((java.lang . Object)
                                          (java.lang . String)
                                          (java.lang . Boolean)
                                          (java.lang . Integer)
                                          (java.lang . Double)
                                          (java.util . Date)
                                          (java.util . Vector)))



(defun java-in-package (package)
  (setf *java-current-package* package)
  (insert (format "package %s;\n\n" *java-current-package*)))

(defun java-import (full-qualified-class-name)
  (insert (format "import %s;\n" full-qualified-class-name)))

(defun* java-class (class-name &key superclass interfaces throws import-thunk body-thunk)
  (when import-thunk (funcall import-thunk))
  (insert (format  "public class %s" class-name))
  (when superclass
    (insert (format " extends %s" superclass)))
  (when interfaces
    (insert (format " implements %s" (join interfaces ","))))
  (when throws
    (insert (format " throws %s" (join throws ","))))
  (insert "{\n")
  (when body-thunk (funcall body-thunk))
  (insert "\n}\n"))

(defun java-parameters (parameters)
  (insert (format "(%s)" (join (mapcar (lambda (parameter)
                                         (destructuring-bind (name type) parameter
                                           (format "%s %s" (prepare-type type) name)))
                                       parameters)
                               ","))))

(defun java-expression (expression)
  (if (atom expression)
    (format "%s" expression)
    (let ((op (first expression))
          (args (rest expression)))
      (cond
       ((member op *java-operators*)
        (if (endp (rest args))
          (format "(%s%s)" op (java-expression (first args)))
          (format "(%s)" (join (mapcar (function java-expression) args)
                               (format "%s" op)))))
       ((eq op '\.)
        (java-send (first args) (second args) (cddr args)))
       (t
        (java-send nil op args))))))

(defun java-arguments (arguments)
  (insert (format "(%s)" (join (mapcar (function java-expression) arguments) ","))))

(defun arguments-from-parameters (parameters)
  (mapcar (lambda (parameter)
            (destructuring-bind (name type) parameter
              name))
          parameters))

(defun java-send (recipient message arguments)
  (insert (if recipient
             (format "%s.%s" recipient message)
             (format "%s" message)))
  (java-arguments arguments))


(defun java-constructor (class-name parameters)
  (insert (format "public %s" class-name))
  (java-parameters parameters)
  (insert "{" "\n")
  (java-send nil 'super (arguments-from-parameters parameters)) (insert ";" "\n")
  (insert "}" "\n"))



(defun java-class-package (class)
  (car (rassoc class *java-predefined-classes*)))

(defun java-fully-qualified-class (class)
  (intern (format "%s.%s" (or (java-class-package class)
                              *java-current-package*)
                  class)))

;; (java-fully-qualified-class 'Integer) java\.lang\.Integer
;; (java-fully-qualified-class 'Geofence) com\.example\.Geofence

(defun prepare-type (type)
  (if (atom type)
    type
    (format "%s<%s>" (first type) (join (mapcar (function prin1-to-string) (rest type)) ","))))

(defun* generate-java-class (file-name package-name class-name &key superclass interfaces throws fields)
  (save-excursion
    (find-file file-name)
    (erase-buffer)
    (insert "// -*- mode:java; coding:utf-8 -*-" "\n")
    (insert "// Generated automatically by generate.el" "\n" "\n")
    (java-in-package package-name)
    (dolist (class (remove-duplicates (append (when superclass (list superclass))
                                              interfaces
                                              (mapcan (lambda (field)
                                                        (if (atom (second field))
                                                          (list (second field))
                                                          (copy-list (second field))))
                                                      fields))))
      (java-import (java-fully-qualified-class class)))

    (java-class class-name
                :superclass superclass
                :interfaces interfaces
                :throws throws
                :import-thunk (lambda ()
                                (java-import 'com.google.gson.annotations.SerializedName))
                :body-thunk   (lambda ()
                                (java-constructor class-name '())
                                (dolist (field fields)
                                  (destructuring-bind (name type) field
                                    (let ((ptype  (prepare-type type)))
                                      (insert (format "@SerializedName(\"%s\")" name) "\n")
                                      (insert (format "public %s %s;" ptype name) "\n")
                                      )))))
    (save-buffer 0)
    (kill-buffer)))



(defmacro define-entity (class &rest fields)
  (let ((class-name (if (atom class)
                      class
                      (first class)))
        (superclass (if (atom class)
                      'Object
                      (second (assoc :superclass (rest class))))))
    `(generate-java-class ,(format "%s%s.java" *dirpath* class-name)
                          *java-current-package* ',class-name
                          :superclass ',superclass
                          :fields ',fields)))
