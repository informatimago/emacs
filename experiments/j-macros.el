
;; for other examples cf. ~/firms/anevia/src/manager2/trunk/objing-macros/

;; // FUNCTION: computeCxxClassName
;; // Given the pathName of a class, computes the fully qualified C++
;; // name, ignoring the packages that have the C++NoNameSpace tag.
;; // EXAMPLE: classPathName=other.OwnerClass.pathName;eval(computeCxxClassName);otherClassName=cxxClassName;
;; String computeCxxClassName=" this{ Package currentPackage=rootPackage; string sep=~"~"; string s=classPathName; string[] items=s.segment(~":~"); int i; cxxClassName=~"~"; for(i=1; i<items.size()-1; i=i+1){ this{ Package subPackage; string item; boolean found=false; getItemSet(items,i,item); StdErr.write(item,NL); subPackage=currentPackage.getPackageByName(item); subPackage.TagTaggedValue.<while(not(found)){ getTagType().<while(not(found)){ found=Name==~"C++NoNameSpace~"; } } if(not(found)){ cxxClassName=cxxClassName+sep+subPackage.Name; sep=~"::~"; } currentPackage=subPackage; } } this{ string item; getItemSet(items,items.size()-1,item); cxxClassName=cxxClassName+sep+item; } } ";
;; // PARAMETERS:
;; String           classPathName;                              // input
;; String           cxxClassName;                               // output
;; // END FUNCTION

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; J macros utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun implode-j-string (source)
  "Return a string containing a J string containing the J source code,
properly escaped.."
  (with-temp-buffer
    (insert source)
    (goto-char (point-min))             ; remove // comments
    (while (and (< (point) (point-max))
                (re-search-forward "^\\([^\"/]\\|\"\\([^\"~]\\|~.\\)*\"\\|/[^/]\\)*\\(//.*\\)$" (point-max) t))
      (with-marker (current (match-end 3))
        (delete-region (match-beginning 3) current)
        (goto-char current))
      (beginning-of-line 1))
    (goto-char (point-min))             ; remove spaces before
    (while (and (< (point) (point-max)) (re-search-forward "^ +" (point-max) t))
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))          ; escape double-quotes and tildes
    (while (and (< (point) (point-max)) (re-search-forward "[\"~]" (point-max) t))
      (goto-char (match-beginning 0))
      (insert "~")
      (forward-char 1))
    (goto-char (point-min))             ; remove newlines
    (while (and (< (point) (point-max)) (search-forward "\n" (point-max) t))
      (delete-region (match-beginning 0) (match-end 0))
      (insert " "))
    (buffer-substring (point-min) (point-max))))


(defun explode-j-string (jstr)
  "Returns a string containing properly formated J code extracted from the JSTR."
  (with-temp-buffer
    (loop
       with state = 'out
       for ch across jstr
       do (case state
            ((init)
             (case ch
               ((?\") (setf state 'out))
               (otherwise
                (error "Unexected character %c at start fo j string" ch))))
            ((out)
             (case ch
               ((?\{) (insert "{\n"))
               ((?\;) (insert ";\n"))
               ((?\}) (insert "}\n"))
               (otherwise (insert ch))))
            ((escaped)
             (insert ch)
             (setf state 'out)))
       finally (progn (java-mode)
                      (indent-region (point-min) (point-max))
                      (return (buffer-substring (point-min) (point-max)))))))


(defun make-j-string (start end)
  (interactive "r")
  (let ((source (buffer-substring-no-properties start end)))
    (with-marker (end end)
      ;; 1- comment out the original source.
      (comment-region start end)
      (goto-char end)
      (insert "\nString xxx=" (implode-j-string source) ";\n"))))


(defstruct jfun start end name comments string parameters)
(defstruct jpar type name initial-value mode comment)


(defun string-prune-left (trim-bag skip-bag string)
  (loop for i from 0 below (length string)
     while (position (aref string i) trim-bag)
     finally (progn (when (and (< i (length string)) (position (aref string i) skip-bag))
                      (incf i))
                    (return (subseq string i)))))

(defun string-prune-right (trim-bag skip-bag string)
  (loop for i from (1- (length string))  downto 0
     while (position (aref string i) trim-bag)
     finally (progn (when (and (< 0 i) (position (aref string i) skip-bag))
                      (decf i))
                    (return (subseq string 0 (1+ i))))))



(defun* get-imploded-j-function-before-point (point)
  "Returns the jfun structure describing the J function found before the point."
  (macrolet ((abort (message &rest args)
               `(progn
                  (message ,message ,@args)
                  (return-from get-imploded-j-function-before-point nil)))
             (check (condition message &rest args)
               `(unless ,condition
                  (abort ,message ,@args))))
    (forward-line 1)
    (check (re-search-backward "^// FUNCTION: " (point-min) t)
           "No // FUNCTION: header.")
    (let ((fun (make-jfun :start (point))))
      (goto-char (match-end 0))
      (c-skip-ws-forward (line-end-position))
      (check (setf (jfun-name fun) (symbol-at-point))
             "Can't find a J function name after // FUNCTION: ")
      (forward-line 1) (beginning-of-line)
      (setf (jfun-comments fun)
            (loop while (looking-at "//")
               collect (string-trim " " (buffer-substring-no-properties
                                         (+ 2 (line-beginning-position))
                                         (line-end-position)))
               do (forward-line 1)))
      (check (looking-at "String ")
             "Can't find a String %s declaration for function %s"
             (jfun-name fun) (jfun-name fun))
      (forward-sexp 1) (c-skip-ws-forward)
      (check (eql (jfun-name fun) (symbol-at-point))
             "Different J function name in String %s declaration vs. // FUNCTION: %s"
             (symbol-at-point) (jfun-name fun))
      (forward-sexp 1) (c-skip-ws-forward)
      (check (looking-at "=") "Expected a = after String %s" (jfun-name fun))
      (forward-char 1) (c-skip-ws-forward)
      (setf (jfun-string fun)
            (loop
               with state = 'init
               with result = '()
               for ch = (prog1 (aref (buffer-substring-no-properties (point) (1+ (point))) 0)
                          (forward-char 1))
               do (ecase state
                    ((init)
                     (case ch
                       ((?\") (setf state 'string))
                       (otherwise
                        (abort "Invalid character %c, expected a string" ch))))
                    ((string)
                     (case ch
                       ((?\~) (setf state 'escape))
                       ((?\") (setf state 'done))
                       (otherwise (push ch result))))
                    ((escape)
                     (push ch result)
                     (setf state 'string)))
               until (eq state 'done)
               finally (return (coerce (nreverse result) 'string))))
      (c-skip-ws-forward)
      (check (looking-at ";")
             "Expected a ; after the string in // FUNCTION: %s" (jfun-name fun))
      (forward-line 1) (beginning-of-line)
      (when (looking-at "// PARAMETERS:")
        (forward-line 1) (beginning-of-line)
        (let ((pars '()))
          (while (let ((case-fold-search t))
                   (looking-at
                    (concat " *"
                            "\\([A-Za-z][A-Z0-9a-z]*\\(\\[\\]\\)?\\) +"
                            "\\([A-Za-z][A-Z0-9a-z]*\\) *"
                            "\\(= *\\(\\([^/]\\|/[^/]\\)*\\)\\)?; *"
                            "\\(// *"
                            "\\(input\\|output\\|inout\\|constant\\) +"
                            "\\(.*\\)?"
                            "\\)?$")))
            (push (make-jpar :type (match-string 1)
                             :name (match-string 3)
                             :initial-value (match-string 5)
                             :mode  (string-downcase (or (match-string 8) "inout"))
                             :comment (match-string 9)) pars)
            (forward-line 1)
            (beginning-of-line))
          (setf (jfun-parameters fun) (nreverse pars))))
      (check (looking-at "// END FUNCTION")
             "Expected // END FUNCTION at the end of the // FUNCTION: %s"
             (jfun-name fun))
      (forward-line 1) (beginning-of-line)
      (setf (jfun-end fun) (point))
      fun)))


(defun* get-exploded-j-function-before-point (point)
  "Returns the jfun structure describing the J function found before the point."
  (macrolet ((abort (message &rest args)
               `(progn
                  (message ,message ,@args)
                  (return-from get-exploded-j-function-before-point nil)))
             (check (condition message &rest args)
               `(unless ,condition
                  (abort ,message ,@args))))
    (forward-line 1)
    (check (re-search-backward "^function " (point-min) t)
           "No function header.")
    (let ((fun (make-jfun :start (point))))
      (goto-char (match-end 0))
      (c-skip-ws-forward (line-end-position))
      (check (setf (jfun-name fun) (symbol-at-point))
             "Can't find a J function name after function ")
      (forward-sexp 1)
      (check (looking-at "(")
             "Missing argument list in function %s" (jfun-name fun))
      (forward-char 1)
      (if (looking-at "[ \n]*)")
          (progn
            (goto-char (match-end 0))
            (forward-line 1)
            (beginning-of-line)
            (setf (jfun-parameters fun) '()))
          (let ((pars '())
                (done nil))
            (while (and (not done)
                        (let ((case-fold-search t))
                          (looking-at
                           (concat " *"
                                   "\\(input\\|output\\|inout\\|constant\\) +"
                                   "\\([A-Za-z][A-Z0-9a-z]*\\(\\[\\]\\)?\\) +"
                                   "\\([A-Za-z][A-Z0-9a-z]*\\) *"
                                   "\\(= *\\(\\([^/\n]\\|/[^/\n]\\)*\\)\\)?"
                                   "\\([,)]\\) *"
                                   "\\(//\\(.*\\)\\)?"))))
              (push (make-jpar :type (match-string 2)
                               :name (match-string 4)
                               :initial-value (match-string 6)
                               :mode (string-downcase (match-string 1))
                               :comment (match-string 10)) pars)
              (setf done (string= (match-string 6) ")"))
              (forward-line 1)
              (beginning-of-line))
            (setf (jfun-parameters fun) (nreverse pars))))
      (setf (jfun-comments fun)
            (loop while (looking-at "//")
               collect (string-trim " " (buffer-substring-no-properties
                                         (+ 2 (line-beginning-position))
                                         (line-end-position)))
               do (forward-line 1)))
      (c-skip-ws-forward)
      (let ((start (point)))
        (forward-sexp 1)
        (setf (jfun-string fun)
              (string-prune-right
               " " "}" (string-prune-left
                        " "  "{"
                        (implode-j-string (buffer-substring-no-properties start (point)))))))
      (setf (jfun-end fun) (point))
      fun)))


(defun insert-imploded-j-function (fun)
  "Insert a jfun with the imploded syntax:
 // FUNCTION: ... String ...=...; // PARAMETERS ... // END FUNCTION"
  (let ((start (point)))
    (insert (format "// FUNCTION: %s\n" (jfun-name fun)))
    (dolist (line (jfun-comments fun))
      (insert (format "// %s\n" line)))
    (insert (format "String %s=\"%s\";\n" (jfun-name fun) (jfun-string fun)))
    (insert "// PARAMETERS:\n")
    (dolist (par (jfun-parameters fun))
      (let ((decl  (format "%-16s %s" (jpar-type par) (jpar-name par))))
        (setf decl (if (jpar-initial-value par)
                       (format "%-38s=%s;" decl (jpar-initial-value par))
                       (format "%s;" decl)))
        (insert (format "%-60s // %-8s " decl (jpar-mode par))))
      (when (and (jpar-comment par) (string< "" (jpar-comment par)))
        (insert (jpar-comment par)))
      (insert "\n"))
    (insert "// END FUNCTION\n")))


(defun insert-exploded-j-function (fun)
  "Insert a jfun with the exploded syntax: function ...(...){...}"
  (let ((start (point)))
    (insert (format "function %s(" (jfun-name fun)))
    (flet ((insert-par (par sep)
             (let ((base (format "%-8s %-16s %s%s%s"
                                 (jpar-mode par) (jpar-type par) (jpar-name par)
                                 (if (jpar-initial-value par)
                                     (format "=%s" (jpar-initial-value par))
                                     "")
                                 sep)))
               (insert (if (and (jpar-comment par) (string< "" (jpar-comment par)))
                           (format "%-50s // %s\n" base (jpar-comment par))
                           (format "%s\n" base))))))
      (let ((pars (jfun-parameters fun)))
        (if pars
            (progn
              (dolist (par (butlast pars))
                (insert-par par ","))
              (insert-par (car (last pars)) ")"))
            (insert ")\n"))))
    (dolist (line (jfun-comments fun))
      (insert (format "// %s\n" line)))
    (insert "{\n")
    (insert (explode-j-string (jfun-string fun)))
    (insert "}\n")
    (indent-region start (point))))



(defun implode-last-j-function (point)
  "Transform the  function ...(...) {...} J function preceding the point
into a // FUNCTION: ... // END FUNCTION section."
  (interactive "d")
  (let ((fun (get-exploded-j-function-before-point point)))
    (when fun
      (delete-region (jfun-start fun) (jfun-end fun))
      (insert-imploded-j-function fun)
      t)))


(defun explode-last-j-function (point)
  "Transform the // FUNCTION: ... // END FUNCTION section preceding the point
into a function ...(...) {...} J function."
  (interactive "d")
  (let ((fun (get-imploded-j-function-before-point point)))
    (when fun
      (delete-region (jfun-start fun) (jfun-end fun))
      (insert-exploded-j-function fun)
      t)))



(defun implode-j-region (start end)
  (interactive "r")
  (goto-char end)
  (while (and (< start (point))
              (implode-last-j-function (point)))))


(defun implode-j-buffer ()
  (interactive)
  (implode-j-region (point-min) (point-max)))



(defun explode-j-region (start end)
  (interactive "r")
  (goto-char end)
  (while (and (< start (point))
              (explode-last-j-function (point)))))


(defun explode-j-buffer ()
  (interactive)
  (explode-j-region (point-min) (point-max)))

(defun trace-j-expressions (start end)
  (interactive "r")
  (goto-char start)
  (with-marker (end end)
    (let* ((es)
           (ee)
           (expression (progn (forward-sexp 1)
                              (setf ee (point))
                              (backward-sexp 1)
                              (setf es (point))
                              (buffer-substring-no-properties es ee))))
      (while (< ee end)
        (delete-region es ee)
        (insert (format "StdErr.write(\"%s = \",%s,NL);" expression expression))
        (setf expression (progn (forward-sexp 1)
                                (setf ee (point))
                                (backward-sexp 1)
                                (setf es (point))
                                (buffer-substring-no-properties es ee)))))))



(defun jmp-compile-buffer ()
  (interactive)
  (let ((jmp-file-name (buffer-file-name)))
    (cond
      ((null jmp-file-name)
        (error "Buffer %S has no file name. Expected a .jmp file."
               (buffer-name)))
      ((not (string-match "^\\(.*\\)\\.jmp$" jmp-file-name))
       (error "Buffer %S isn't a .jmp file buffer." (buffer-name)))
      (t (let ((jml-file-name (format "%s.jmf" (match-string 1 jmp-file-name)))
               (jmp-source (buffer-string)))
           (find-file jml-file-name)
           (erase-buffer)
           (insert jmp-source)
           (implode-j-buffer)
           (save-buffer 0)
           (kill-buffer (current-buffer)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
