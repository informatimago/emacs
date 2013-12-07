
(defvar *c++-macros* (make-hash-table))
(defvar *c++-free-variables* '())


(defun c++/macro-function (symbol)
  (gethash symbol *c++-macros*))


(defun compile-c++-generator (expression)
  (if (atom expression)
      `(c++/insert-item expression)
      (case (first expression)
        ;; the special operators of the c++ generator
        ((progn)
         `(progn ,@(mapcar (function compile-c++-generator)
                           (rest expression))))
        (otherwise
         ;; a macro call or a function call
         (if (c++/macro-function (first expression))
             (compile-c++-generator (c++/macroexpand expression))
             expression
             ;; `(,(first expression)
             ;;    ,@(mapcar (function compile-c++-generator)
             ;;              (rest expression)))
             )))))


(defmacro define-c++-macro (name arguments &rest body)
  `(progn
     (setf (gethash ',name *c++-macros*)
           (lambda (&rest c++-macro-arguments)
             (destructuring-bind ,arguments c++-macro-arguments
               (let ((*c++-free-variables* (append ',arguments *c++-free-variables*)))
                 ,(compile-c++-generator (if (= 1 (length body))
                                             (first body)
                                             `(progn ,@body)))))))
     ',name))


(defun c++/macroexpand (form)
  (if (atom form)
      form
      (let ((c++-macro (c++/macro-function (first form))))
        (if c++-macro
             (apply c++-macro (rest form))
            form))))


(defun expand-last-c++-macro (point)
  (interactive "d")
  (let* ((form        (save-excursion (backward-sexp) (sexp-at-point)))
         (clocomp     (looking-at "[ \n\t]*\\*/"))
         (clocom-end  (match-end 0)))
    (when clocomp (goto-char clocom-end))
    (insert "\n")
    (eval (c++/macroexpand form))))



(defun c++/gen-insert-statement (expression)
  (if (atom expression)
      (if (member expression *c++-free-variables*)
          `(insert (format "%s;\n" expression))
          `(insert (format "%s;\n" ',expression)))
      (case (first expression)
        ((lisp)
         `(progn ,@(rest expression)))
        (otherwise
         (if (c++/macro-function (first expression))
             (c++/macroexpand expression)
             `(progn
                ,@(mapcan (lambda (item)
                            `((insert " ") ,(c++/gen-insert-item item)))
                          expression)
                (insert ";\n")))))))


(defun c++/gen-insert-item (item)
  (if (atom item)
      (if (member item *c++-free-variables*)
          `(insert (format "%s" ,item))
          `(insert ,(format "%s" item)))
      (case (first item)
        ((<> {} [] \(\) )
         `(progn ,(c++/gen-insert-item (second item))
                 (insert ,(format "%c" (aref (symbol-name (first item)) 0)))
                 ,@(let ((first-time t))
                        (mapcar (lambda (subitem)
                                  (if first-time
                                      (progn (setf first-time nil)
                                             (c++/gen-insert-item subitem))
                                      `(progn (insert ",")
                                              ,(c++/gen-insert-item subitem))))
                                (cddr item)))
                 ,@(when (eq '<> (first item)) `((insert " ")))
                 (insert ,(format "%c" (aref (symbol-name (first item)) 1)))))
        ((lisp)
         `(progn ,@(rest item)))
        (otherwise
         (if (c++/macro-function (first item))
             (c++/macroexpand item)
              (c++/gen-insert-item (cons '\(\) item)))))))


(defmacro c++ (&rest statements)
  `(c++/statements ,@statements))


(defmacro c++/statements (&rest statements)
  `(progn
     ,@(mapcar (lambda (statement) (c++/gen-insert-statement statement))
               statements)))


(defmacro c++/progn (&rest statements)
  `(progn
     (insert "{\n")
     (c++/statements ,@statements)
     (insert "}")))


(defmacro with-extended-c++-environment ((&rest vars) &rest body)
  `(let ((*c++-free-variables* (append ',vars *c++-free-variables*)))
     ,@body))


(defmacro c++/foreach ((type var container) &rest statements)
  (with-extended-c++-environment (type var container) 
    `(progn
       ,(c++/gen-insert-item (list "foreach" type var container))
       (insert "{\n")
       (c++/statements ,@statements)
       (insert "}\n"))))


(define-c++-macro progn (&rest body)
  `(progn
     (insert "{\n")
     (c++/statements ,@body)
     (insert "}\n")))


(define-c++-macro foreach ((type var container) &rest body)
  `(progn
     ,(c++/gen-insert-item (list "foreach" type var container))
     (c++/statements ,@body)))


(define-c++-macro for ((init test increment) &rest body)
  `(progn
     (insert "for(")
     ,(c++/gen-insert-item init)
     (insert ";")
     ,(c++/gen-insert-item test)
     (insert ";")
     ,(c++/gen-insert-item increment)
     (insert ")")
     (c++/statements ,@body)))

(define-c++-macro if (test then &optional else)
  `(progn
     (insert "if(")
     ,(c++/gen-insert-item test)
     (insert ")")
     (c++/statements ,then)
     (when else
       (insert "else")
       (c++/statements ,else))))


(define-c++-macro equivalence-classes (result-var element-type key-type container equalp keyf)
  ;; /*
  ;;  equivalenceClasses(container,equalp,keyf)
  ;;  Returns the equivalence classes of elements, via keyf, modulo equalp.
  ;;  This is a std::list of std::list of elements of container.
  ;;  All the elements in a sublist have their keyf equalp.
  ;;  */
  ;; template <class Container,class EqualP,class KeyF>
  ;; std::list<std::list<typename Container::value_type> >
  ;; equivalenceClasses(const Container& elements,EqualP equalp,KeyF keyf){
  ;;     typedef typename Container::value_type                  Element;
  ;;     typedef typename KeyF::result_type                      Key;
  ;;     typedef std::pair< Key, std::list< Element > >          ClassDesc;
  ;;     typedef std::list< ClassDesc >                          Classes;
  ;;     typedef std::list< std::list< Element > >               Result;
  ;;     Classes classes;
  ;;     foreach(Element,item,elements){
  ;;         Key itemKey=keyf(item);
  ;;         typename Classes::iterator classit=std::find_if(classes.begin(),classes.end(),boost::bind(equalp,itemKey,boost::bind(first<Key,std::list<Element> >,_1)));
  ;;         if(classit!=classes.end()){
  ;;             classit->second.push_back(item);
  ;;         }else{
  ;;             classes.push_back(std::make_pair(itemKey,std::list<Element>(1,item)));
  ;;         }
  ;;     }
  ;;     Result result;
  ;;     foreach(ClassDesc,classe,classes){
  ;;         std::list<Element> list;
  ;;         foreach(Element,element,classe.second){
  ;;             list.push_back(element);
  ;;         }
  ;;         result.push_back(list);
  ;;     }
  ;;     // std::transform(classes.begin(),classes.end(),result.begin(),boost::bind(second<Key,std::list<Element> >,_1));
  ;;     return(result);
  ;; }
  (c++
   (typedef element-type Element)
   (typedef key-type     Key)
   (typedef (<> std::pair Key (<> std::list Element))   ClassDesc)
   (typedef (<> std::list ClassDesc)   Classes)
   (typedef (<> std::list (<> std::list Element)) Result)
   (Classes classes)
   (foreach (element-type ecItem container)
            (progn
              (Key itemKey = (keyf item))
              (Classes::iterator classit = (classes.begin))
              (for ( (= classit (classes.begin))
                     (!= classit (classes.end))
                     (++ classit))
                   (progn
                     (if (funcall equalp classit->first itemKey)
                         (break))))
              (if (== classit (classes.end))
                  (classes.push_back (std::make_pair itemKey (<> std::list Element 1 item)))
                  (classit->second.push_back item))))))

;; (progn
;;   (setf (gethash (quote equivalence-classes) *c++-macros*)
;;         (lambda (&rest c++-macro-arguments)
;;           (destructuring-bind #1=(result-var element-type key-type container equalp keyf)
;;             c++-macro-arguments
;;             (let ((*c++-free-variables* (append (quote #1#) *c++-free-variables*)))
;;               (c++ (typedef element-type Element)
;;                    (typedef key-type Key)
;;                    (typedef (<> std::pair Key (<> std::list Element)) ClassDesc)
;;                    (typedef (<> std::list ClassDesc) Classes)
;;                    (typedef (<> std::list (<> std::list Element)) Result)
;;                    (Classes classes)
;;                    (foreach (element-type ecItem container)
;;                             (progn (Key itemKey = (keyf item))
;;                                    (Classes::iterator classit = (classes\.begin))
;;                                    (for ((= classit (classes\.begin)) (!= classit (classes\.end)) (++ classit))
;;                                         (progn (if (funcall equalp classit->first itemKey) (break))))
;;                                    (if (== classit (classes\.end))
;;                                        (classes\.push_back (std::make_pair itemKey (<> std::list Element 1 item)))
;;                                        (classit->second\.push_back item)))))))))
;;   (quote equivalence-classes))


(c++/macroexpand '(equivalence-classes schedule
                   Mappers::InputMappers Anevia::SmartPtr<Streamers::Streamer_intf>
                   spares (function ==) (lambda (x) (get-stream (get-device x))))) typedef Mappers::InputMappers Element;
;;  typedef Anevia::SmartPtr<Streamers::Streamer_intf> Key;
;;  typedef std::pair<Key,std::list<Element > > ClassDesc;
;;  typedef std::list<ClassDesc > Classes;
;;  typedef std::list<std::list<Element > > Result;
;;  Classes classes;
;; foreach(Mappers::InputMappers,ecItem,spares){
;;  Key itemKey = (lambda (x) (get-stream (get-device x)))(item);
;;  Classes::iterator classit = classes.begin();
;; for(=(classit,classes.begin());!=(classit,classes.end());++(classit)){
;; if(funcall((function ==),classit->first,itemKey)) break;



