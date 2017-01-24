(require 'xml)

;;; patches.

(defun pprint (object &optional stream)
  (pp object stream))

(defadvice xml-parse-region (around xml-parse-region/dtd-patch last
                                    (beg end &optional buffer parse-dtd parse-ns)
                                    activate)
  "DTD are file-level, so we move elements out of the dtd sexp."
  (if (or xml-validating-parser parse-dtd)
      (let ((sexp ad-do-it))
        (if (and (listp sexp)
                 (consp (car sexp))
                 (listp (caar sexp))
                 (eq 'dtd (caaar sexp)))
            (list* :hi
                   (caar sexp)
                   (cdar sexp)
                   (cdr sexp))
            sexp))
      ad-do-it))


;;; documents additions.

(defun* make-doc (&key dtd root)
  "dtd may be nil."
  (if dtd
      (list dtd root)
      (list root)))

(defun doc-has-dtd (doc)
  (and (listp doc)
       (listp (car doc))
       (eq 'dtd (caar doc))))

(defun doc-dtd (doc)
  (when (doc-has-dtd doc)
    (first doc)))

(defun doc-root (doc)
  (cond
    ((doc-has-dtd doc) (second doc))
    ((null (first doc)) (second doc))
    (t (first doc))))


;;; xml nodes additions.

(defun* make-xml-node (&key name attributes children)
  (list* name attributes children))

(defun xml-node-p (object)
  (and (consp object)
       (or (symbolp (car object))
           (and (consp (car object))
                (consp (cadr object))
                (null (cddr object))))
       (consp (cdr object))))


;;; attributes additions.

(defun make-xml-attribute (name value) (cons name value))
(defun xml-attribute-name  (attribute) (car attribute))
(defun xml-attribute-value (attribute) (cdr attribute))


;;; Let's clean up xml inputs.

(defun* xml-remove-blank-elements (xml-node &optional (recursively t))
  (make-xml-node :name (xml-node-name xml-node)
                 :attributes (xml-node-attributes xml-node)
                 :children (mapcan (lambda (node)
                                     (typecase node
                                       (string
                                        (if (every (lambda (ch) (find ch " \n\t\r\l\f\v")) node)
                                            '()
                                            (cons node nil)))
                                       (cons   (cons (if recursively
                                                         (xml-remove-blank-elements node recursively)
                                                         node)
                                                     nil))
                                       (t      (cons node nil))))
                                   (xml-node-children xml-node))))


;;; Conversion routines.

(defun xml-to-sexp-buffer ()
  "Convert the XML text in the buffer into a lisp S-exp."
  (interactive)
  (let* ((rdoc (xml-parse-region (point-min) (point-max) (current-buffer) t))
         (di (message "%S" rdoc))
         (pdoc (make-doc :dtd (doc-dtd rdoc)
                         :root (xml-remove-blank-elements (doc-root rdoc)))))
    (delete-region (point-min) (point-max))
    (lisp-mode)
    (pprint pdoc (current-buffer))
    (goto-char (point-min))
    (down-list)
    (when (doc-has-dtd rdoc)
      (forward-sexp 2)
      (backward-sexp 1))))




(defun* xml-insert-header (&optional (encoding 'utf-8))
  (insert (format  "<?xml version=\"1.0\" encoding=\"%s\"?>\n" (string-upcase encoding))))

(defun xml-quote-attribute-value (value)
  (with-output-to-string
      (loop
         for code across value
         for ch = (string code)
         do (princ (cond
                     ((string= ch "\"")  "\\\"")
                     ((string= ch "\\")  "\\\\")
                     (t                  ch))))))

(defun test/xml-quote-attribute-value ()
  (assert (string= "1.0" (xml-quote-attribute-value "1.0")))
  (assert (string= "There is no \\\"spoon\\\"!"
                   (xml-quote-attribute-value "There is no \"spoon\"!")))
  (assert (string= "A new line is inserted writting \\\\n in the string."
                   (xml-quote-attribute-value "A new line is inserted writting \\n in the string."))))

;; (dtd "plist"
;;      ("-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd" public)
;; <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"

(defun xml-insert-dtd (dtd)
  ;; TODO: missing processing various kinds of doctype contents elements.
  (assert (eq 'dtd (first dtd)))
  (let ((name     (second dtd))
        (contents (cddr dtd)))
    (insert (format "<!DOCTYPE %s" name))
    (let ((item (car contents)))
      (if (member (car (last item)) '(public system))
          (case (car (last item))
            ((public)
             (insert (format " PUBLIC %S %S" (first item) (second item))))
            ((system)
             (insert (format " SYSTEM %S" (first item)))))
          (progn
            (insert " [\n")
            (dolist (element contents)
              (insert (format "<!ELEMENT %s (%s)>\n"
                              (first element)
                              (if (symbolp (second element))
                                  (format "#%s" (string-upcase (second element)))
                                  (second element)))))
            (insert "]"))))
    (insert ">\n")))


(defun xml-insert-xml-node (node)
  (if (xml-node-p node)
      (let ((name       (xml-node-name       node))
            (attributes (xml-node-attributes node))
            (children   (xml-node-children   node)))
        (insert (format "<%s" name))
        (dolist (attribute attributes)
          (insert (format " %s=\"%s\""
                          (xml-attribute-name attribute)
                          (xml-quote-attribute-value (xml-attribute-value attribute)))))
        (if children
            (progn
              (insert ">")
              (mapc (function xml-insert-xml-node) children)
              (insert (format "</%s>" name)))
            (insert " />")))
      (insert (format "%s" node))))


(defun sexp-to-xml-buffer ()
  "Convert the lisp S-exp in the buffer that represents a XML tree into XML text.
The buffer should contain a single lisp list, each element representing a XML node,
being a list (name attributes . children); attributes is an a-list of (name . value);
name is a string designator or a cons cell (namespace . tag).
children is a list of strings or sexp representing a XML tree.
"
  (interactive)
  (goto-char (point-min))
  (let ((doc (read (current-buffer))))
    (delete-region (point-min) (point-max))
    (nxml-mode)
    (let ((coding-system 'utf-8))
      (set-buffer-file-coding-system coding-system)
      (xml-insert-header coding-system))
    (when (doc-has-dtd doc)
      (xml-insert-dtd (doc-dtd doc)))
    (xml-insert-xml-node (doc-root doc))
    (insert "\n")))



(provide 'pjb-xml)
