;; Implements structure as alists prefixed by the structure type name.
;; json uses this kind of alist.

(defun alistp% (slow fast)
  (or (null slow)
      (and (not (eq slow fast))
           (consp slow)
           (consp (car slow))
           (listp fast)
           (listp (cdr fast))
           (alistp% (cdr slow) (cddr fast)))))

(defun alistp (object)
  (or (null object)
      (and (consp object)
           (alistp% object (cdr object)))))

(assert (not (alistp '#1=((1 . 2) . #1#))))
(assert (not (alistp '((1 . 2) (3 . 4) . x))))
(assert (not (alistp '((1 . 2) (3 . 4)  x (5 . 6)))))
(assert (alistp '((1 . 2) (3 . 4) (5 . 6))))
(assert (alistp '((1 . 2) (3 . 4))))
(assert (alistp '((1 . 2))))
(assert (alistp '()))


(defmacro define-structure (name fields)
  `(progn
     (defun* ,(intern (format "make-%s" name)) (&rest fields &key ,@fields)
       (cons ',name (mapcar* (function cons) ',fields fields)))
     ,@(mapcan (lambda (field)
                 (list
                  `(defun ,(intern (format "%s-%s" name field)) (structure)
                    (cdr (assoc ',field (cdr structure))))
                  `(defun ,(intern (format "set-%s-%s" name field)) (structure value)
                    (let ((entry (assoc ',field (cdr structure))))
                      (if (null entry)
                          (push (cons ',field value) (cdr structure))
                          (setf (cdr entry) value))
                      value))
                  `(defun ,(intern (format "%s-p" name)) (object)
                     (and (consp object)
                          (eq ',name (car object))
                          (alistp (cdr object))))))
               fields)
     ',name))
