(require 'cl)

(defun make-function-buffer-local (fname)
  "FNAME is a symbol naming a function"
  (setf (get fname 'buffer-local-default-function)
        (if (fbound fname)
            (symbol-function fname)
            (lambda (&rest args) (declare (ignore args)) nil)))
  (let ((buffer-local-variable (gensym)))
    (make-variable-buffer-local buffer-local-variable)
    (setf (get fname 'buffer-local-function) buffer-loccal-variable)
    (setf (symbol-function fname)
          (byte-compile
           `(lambda (&rest args)
              (if (and (boundp ',buffer-local-variable) ,buffer-local-variable)
                  (apply ,buffer-local-variable args)
                  (apply (get ',fname 'buffer-local-default-function) args)))))))
