(require 'pjb-html)

(defun android-packages-and-classes ()
 (let* ((root "http://developer.android.com/reference/")
        (android-xml (with-file "~/src/public/emacs/a.xhtml"
                       (pjb-parse-html (buffer-substring (point-min) (point-max)))))
        (body  (get-first-child-tagged (car android-xml) 'body))
        (ul    (get-first-child-tagged body 'ul))
        (lis   (get-children-tagged ul 'li))
        (packages (mapcar (lambda (li)
                            (let* ((a (get-first-child-tagged li 'a))
                                   (href (attribute-value (get-attribute-named a 'href )))
                                   (name (string-trim " \n\t" (first (element-children a)))))
                              (list name href)))
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
                             (list name package url)))
                         trs)))
   (list packages classes)))

(defvar *android-packages* '())
(defvar *android-classes*  '())

(let ((pc (android-packages-and-classes)))
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
                              (list (car (last components))
                                    (mapconcat (function identity)
                                               components
                                               "."))))
                          (let ((result (quote())))
                            (mapfiles (lambda (path) 
                                        (when (suffixp ".java" path)
                                          (push path result)))
                                      root-directory :recursive)
                            result))))))))


(defun android-all-classes ()
  "Return a list of fully qualified names of  all the android classes found in the buffer."
  (let ((packages '())
        (class-package-alist (append (java-all-program-classes)
                                     *android-classes*)))
    (dolist (class-name (jde-import-all-find-classes-to-import) packages)
      (let ((class (assoc class-name class-package-alist)))
        (when class
          (pushnew (second class) packages :test (function string=)))))))


(defun android-import-all ()
  (interactive)
  (dolist (class (android-all-classes))
    (jde-import-one-class class)))

(provide 'android-classes)
