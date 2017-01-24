(require 'pjb-html)
;; (require 'jde nil t)
;; (require 'jde-import nil t)

(defun make-android-package-info (name documentation-url) (list name documentation-url))
(defun android-class-info-name (info) (first info))
(defun android-class-info-documentation-url (info) (second info))

(defun make-android-class-info (name package documentation-url) (list name package documentation-url))
(defun android-class-info-name (info) (first info))
(defun android-class-info-package (info) (second info))
(defun android-class-info-documentation-url (info) (third info))

(defun android-packages-and-classes (android-classes-xhtml)
 (let* ((root "http://developer.android.com/reference/")
        (android-xml (with-file android-classes-xhtml
                       (pjb-parse-html (buffer-substring (point-min) (point-max)))))
        (body  (get-first-child-tagged (car android-xml) 'body))
        (ul    (get-first-child-tagged body 'ul))
        (lis   (get-children-tagged ul 'li))
        (packages (mapcar (lambda (li)
                            (let* ((a (get-first-child-tagged li 'a))
                                   (href (attribute-value (get-attribute-named a 'href )))
                                   (name (string-trim " \n\t" (first (element-children a)))))
                              (make-android-package-info name href)))
                          lis))
        (table (get-first-child-tagged body 'table))
        (trs   (get-children-tagged table 'tr))
        (classes (mapcar (lambda (tr)
                           (let* ((linkcol (get-first-child-valued tr 'class "jd-linkcol"))
                                  (a (get-first-child-tagged linkcol 'a))
                                  (url  (attribute-value (get-attribute-named a 'href)))
                                  (name (string-trim " \n\t" (first (element-children a))))
                                  (package (if (and (prefixp root url)
                                                    (suffixp ".html" url))
                                               (substitute (character ".") (character "/")
                                                           (substring url (length root) (- (length url) (length ".html"))))
                                               name)))
                             (make-android-class-info name package url)))
                         trs)))
   (list packages classes)))

(defvar *android-packages* '())
(defvar *android-classes*  '())

(let ((pc (android-packages-and-classes "~/src/public/emacs/android-classes.xhtml")))
  (setf *android-packages* (first pc)
        *android-classes* (second pc))
  nil)


(defun java-all-program-classes ()
  "Return a list of fully qualified names of all the java classes found in the current project."
  (let ((path (or (buffer-file-name) default-directory)))
    (and path
         (let* ((path            (file-name-directory path))
                (root-component  "/src/")
                (root-position   (search root-component path :from-end t)))

           (and root-position
                (let ((root-directory  (subseq path 0 (+ root-position (length root-component)))))
                  (mapcar (lambda (class-path)
                            (let ((components (split-string (subseq class-path (length root-directory)
                                                                    (- (length class-path)
                                                                       (length ".java")))
                                                            "/")))
                              (make-android-class-info (car (last components))
                                                       (mapconcat (function identity)
                                                                  components
                                                                  ".")
                                                       nil)))
                          (let ((result (quote())))
                            (mapfiles (lambda (path)
                                        (when (suffixp ".java" path)
                                          (push path result)))
                                      root-directory :recursive)
                            result))))))))


(defun android-all-classes ()
  "Return a list of fully qualified names of  all the android classes found in the buffer."
  (let ((packages '())
        (class-package-alist (append (java-all-program-classes) *android-classes*)))
    (dolist (class-name (jde-import-all-find-classes-to-import) packages)
      (let ((class (assoc class-name class-package-alist)))
        (when class
          (pushnew (android-class-info-package class) packages :test (function string=)))))))


(defun android-browse-documentation-of-class (class-name)
  (let ((class-info (assoc class-name *android-classes*)))
    (if class-info
      (browse-url (android-class-info-documentation-url class-info))
      (error "Not a known android class: %s" class-name))))

(defun android-browse-documentation-of-class-at-point ()
  (interactive)
  (android-browse-documentation-of-class (thing-at-point 'symbol)))


(defun android-import-all ()
  (interactive)
  (dolist (class (android-all-classes))
    (jde-import-one-class class)))

(provide 'android-classes)
