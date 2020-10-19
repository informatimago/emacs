
;; ------------------------------------------------------------------------
;; pjb-defclass
;; ------------------------------------------------------------------------
;; Syntactic sugar for defclass
;;

;; (defmacro pjb-attrib (name type &rest args)
;;   "
;; This macro outputs an attribute s-exp as used in defclass.
;; ARGS  may be of length 1 or 2.
;;       If (LENGTH ARGS) = 1
;;       then if the argument is a string,
;;            then it's taken as the documentation and the initial value is NIL
;;            else it's taken as the initial value and the documentation is NIL.
;;       else the first is the initial value and the second is the documentation.
;; The initarg an accessor are the same keyword built from the name.
;; "
;;   (let ((iarg (intern (format ":%s" name)))
;;         init doc)
;;     (cond
;;       ((= 2 (length args))
;;        (setq init (car  args)
;;              doc  (cadr args)) )
;;       ((= 1 (length args))
;;        (if (stringp (car args))
;;            (setq init nil
;;                  doc  (car args))
;;            (setq init (car args)
;;                  doc  nil)) )
;;       (t (error "Invalid arguments to pjb-attrib.")))
;;     (if (and (symbolp type) (null init))
;;         (setq type (list 'or 'null type)))
;;     (if (null doc)
;;         (setq doc (symbol-name name)))
;;     `(,name
;;       :initform ,init
;;       :initarg  ,iarg
;;       :accessor ,name
;;       :type     ,type
;;       :documentation ,doc)
;;     )) ;;pjb-attrib


;; (defmacro pjb-defclass (name super &rest args)
;;   "
;; This macro encapsulate DEFCLASS and allow the declaration of the attributes
;; in a shorter syntax.
;; ARGS  is a list of s-expr, whose car is either :ATT (to declare an attribute)
;;       or :DOC to give the documentation string of the class.
;;       (:OPT is not implemented yet).
;;       See PJB-ATTRIB for the syntax of the attribute declation.
;;       (:ATT name type [ init-value [doc-string] | doc-string ] )
;; "
;;   (let ((fields  nil)
;;         (options nil))
;;     (while args
;;       (cond ((eq :att (caar args))
;;              (push (macroexpand (cons 'pjb-attrib (cdar args))) fields))
;;             ((eq :doc (caar args))
;;              (push (cons :documentation (cdar args)) options))
;;             )
;;       (setq args (cdr args)))
;;     (setq fields (nreverse fields))
;;     (setq options (nreverse options))
;;     `(defclass ,name ,super ,fields ,options)))




(defun camel-case (text)
  (if (find ?- text)
      (mapconcat (function capitalize)  (split-string text "-_") " ")
    (capitalize text)))

(defun composition-juxtaposed-text-components (text)
  (let ((components (make-vector (* 2 (length text))  '(base-right . base-left))))
    (dotimes (i (length text) components)
      (setf (aref components (* 2 i)) (aref text i)))))

(defun camel-case-symbols-font-lock ()
  "
RETURN: A font-lock-keywords list lisp-style symbols to camel case.
"
  (when (<= 21 emacs-major-version)
    `(("\\<\\([-_!@#$%^&*A-Za-z0-9]+\\)\\>"
       (1 (progn (compose-region (match-beginning 1) (match-end 1)
                                 (composition-juxtaposed-text-components
                                  (camel-case (buffer-substring (match-beginning 1) (match-end 1))))
                                 'decompose-region)
                 nil))))))
"camel-case-symbols-font-lock"

;; (camel-case "camel-case-symbols-font-lock")
;; (font-lock-add-keywords nil (camel-case-symbols-font-lock))


