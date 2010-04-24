;; Extracting COMMON-LISP  argument lists from SBCL
(require 'sb-introspect)

(DEFUN no-package (tree)
  (COND ((SYMBOLP tree)
         (COND ((CHAR= (CHARACTER "?") 
                       (CHAR (SYMBOL-NAME tree)
                             (1- (LENGTH (SYMBOL-NAME tree)))))
                (INTERN (FORMAT nil "~A-P"
                                (SUBSEQ (SYMBOL-NAME tree) 0 
                                        (1- (LENGTH (SYMBOL-NAME tree)))))))
               (t (INTERN (SYMBOL-NAME tree)))))
        ((ATOM tree) tree)
        (t (CONS (no-package (CAR tree)) (no-package (CDR tree))))));;no-package


(DEFPARAMETER +cl-lambda-list-keywords+
  '(&OPTIONAL &REST &KEY &ALLOW-OTHER-KEYS &AUX &BODY &WHOLE &ENVIRONMENT))


(DEFUN split-lambda-list-on-keywords (lambda-list lambda-list-kind)
  "
lambda-list-kind:  (member +cl-lambda-list-kinds+)
"
  (declare (ignore lambda-list-kind))
  (let ((sing-result '())
        (env (position '&ENVIRONMENT lambda-list)))
    (when env
      (push (list '&ENVIRONMENT (elt lambda-list (1+ env))) sing-result)
      (setf lambda-list (remove-if (lambda (x) (declare (ignore x)) t)
                              lambda-list :start env :end (+ env 2))))
    (when (eq '&WHOLE (first lambda-list))
      (push (subseq lambda-list 0 2) sing-result)
      (setf lambda-list (cddr lambda-list)))
    (do ((llk '(&MANDATORY &OPTIONAL &KEY &ALLOW-OTHER-KEYS &AUX &REST &BODY))
         (args (if (member (first lambda-list) +cl-lambda-list-keywords+)
                   lambda-list
                   (cons '&MANDATORY lambda-list)) 
               (cdr args))
         (chunk '())
         (result '()))
        ((null args)
         (when chunk (push (nreverse chunk) result))
         (nreverse (nconc sing-result result)))
      (if (member (car args) llk)
          (progn
            (when chunk (push (nreverse chunk) result))
            (setf chunk (list (car args))))
          (push (car args) chunk)))));;split-lambda-list-on-keywords


(DEFUN clean-keywords (arglist &optional macro)
  (when macro
    (setf arglist (mapcar (lambda (x) (if (listp x) (clean-keywords x macro) x))
                          arglist)))
  (let*  ((splited (split-lambda-list-on-keywords arglist))
          (keys  (MEMBER '&KEY splited :key (function first))))
    (if keys
        (progn (SETF (CDAR keys)
                     (mapcar
                      (lambda (x)
                        (print x)
                        (if (CONSP x)
                            (progn
                              (format  t "~A" (SECOND x))
                              (WHEN (EQUALP (SECOND x) ''character)
                                (SETF (SECOND x) 'character))
                              (if (consp (car x)) (CONS (caar x) (CDR x)) x))
                            x))
                      (CDAR keys)))
               (APPLY (function APPEND) splited))
        arglist)));;clean-keywords


(WITH-OPEN-FILE (out (MAKE-PATHNAME :defaults (USER-HOMEDIR-PATHNAME)
                                    :NAME "CL-INTRO" :type "DATA"
                                    :CASE :common) 
                     :direction :output
                     :if-does-not-exist :create :if-exists :supersede)
  (let ((*print-pretty* nil))
    ;;(FORMAT out ";; -*- mode:Lisp -*-~%")
    ;;(FORMAT out "(setq *raw-lambda-lists* '(~%")
    (dolist (symbol (list-external-symbols "COMMON-LISP"))
      (catch :abort
        (let ((m nil))
          (PRINT
           (LIST
            (cond ((special-operator-p symbol) :special-operator)
                  ((MACRO-FUNCTION symbol) (setf m t) :macro)
                  ((AND (FBOUNDP symbol)
                        (typep (symbol-function symbol) 'generic-function))
                   :generic)
                  ((fboundp symbol) :function)
                  (t (throw :abort nil)))
            symbol
            (no-package (sb-introspect:function-arglist symbol)))
           out))))
    ;;(FORMAT out "~&))~%")
    ))

;;;; cl-intro.lisp                    --                     --          ;;;;
