;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:              pjb-make-depends.el
;;;;LANGUAGE:          emacs
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;
;;;;    This elisp script generates dependencies for lisp sources, based on
;;;;    (require) sexps, a load-path, and ad-hoc processing.
;;;;
;;;;    Object files can be either elisp compiled (.elc) or clisp compiled
;;;;    (.fas) or cmucl compiled (.x86f)
;;;;    and source files can be either elisp (.el) or clisp or cmucl (.lisp,
;;;;    .lsp, .cl), and elisp sources may (require) common-lisp files
;;;;    (.lisp, .lsp, .cl extensions for sources, but .elc compiled form).
;;;;
;;;;USAGE
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2002-11-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    Copyright Pascal J. Bourguignon 2002 - 2011
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU  General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This script is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this library; see the file COPYING.LIB.
;;;;    If not, write to the Free Software Foundation,
;;;;    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;****************************************************************************
;; (require 'pjb-cl)
(require 'pjb-strings) ;; import dirname
(require 'pjb-list)
(require 'pjb-utilities)

;;(require 'pjb-compatibility)
(provide 'pjb-make-depends)

;;(EMACS
(defalias 'regexp:match         'string-match)
(defalias 'regexp:match-string  'match-string)
;; )



(defun extract-source-from-require-sexp (sexp)
  "
PRE:    sexp is of the form: (REQUIRE module-name &OPTIONAL pathname-list)
        module-name can be 'toto or (quote toto).
        Each path name can be either a namestring, a physical path name or
        a logical path name.
RETURN: A new list containing the pathname-list if present, or a list
                              containing the symbol-name  of the module-name.
"
  (let ((symb  (nth 1 sexp))
        (files (cddr sexp)) )
    (if (null files)
      (list (symbol-name (eval symb)))
      (copy-seq files)))
  );;extract-source-from-require-sexp


(defun get-requires (source-file)
  "
RETURN:  A list of file or symbol names listed in require sexp in source-file.
"
  (save-excursion
    (find-file source-file)
    (goto-char (point-min))
    (prog1
        (loop with result = nil
              while (/= (point) (point-max))
              for sexp = (condition-case nil (sexp-at-point) (error nil))
              do
              (when (and (consp sexp)
                         (or (eq  (car sexp) 'require)
                             (eq  (car sexp) 'REQUIRE)))
                (setq result
                      (nconc (extract-source-from-require-sexp sexp) result )))
              (condition-case nil
                  (forward-sexp 1)
                (error (goto-char (point-max)))
                (wrong-type-argument (goto-char (point-max))))
              finally return result)
      (kill-buffer (current-buffer))
      (sleep 0))));;get-requires


(defun find-file-path (fname dir-paths)
  "
RETURN:  nil or the path of fname found in one of dir-paths.
"
  (do* ((paths dir-paths   (cdr paths))
        (dpath (car paths) (car paths))
        (fpath ) )
      ( (or (null dpath)
            (probe-file
             (setq fpath (if (cl:string= dpath ".")
                             fname
                             (concatenate 'string dpath "/" fname)))))
        (if dpath  fpath nil) ))
  );;find-file-path



(defvar *extensions-emacs* '((".el" . ".elc"))
  "A list of cons of extensions for source lisp and object lisp files.")

(defvar *extensions-clisp*
  '((".lisp" . ".fas")  (".lsp" . ".fas") (".cl" . ".fas")
    (".lisp" . ".fasl") (".lsp" . ".fasl") (".cl" . ".fasl")
    (".lisp" . ".x86f") (".lsp" . ".x86f") (".cl" . ".x86f"))
  "A list of cons of extensions for source lisp and object lisp files.");;*extensions-clisp*


;; object-file can be either .fas or .elc
;;
;; In  both cases, it  may have  as source,  either a  common-lisp source
;; (.lisp, .lsp  or .cl), or a elisp  source (.el). (If a  .fas, we seach
;; first for a .lisp, and if a .elc, we search first for a .el).
;;
;;
;; For  required files,  we search  whatever source  file (first  of same
;; class as  the source found for  object-file), and return  in anycase a
;; corresponding   object  file   of  the   same  class   (extension)  as
;; object-file.
;;
;; A .fas  cannot require and load a  .elc, it requires and  loads only a
;; .fas, while  a .elc cannot  require and load  a .fas, it  requires and
;; loads only a .elc.


(defun get-source-file (object-file)
  "
RETURN: The source file for OBJECT-FILE (finding the right extension);
        the extension of the object-file;
        and the list of extensions couples.
"
  (unless (regexp:match "^\\(.*\\)\\(\\.[^.]*\\)$" object-file)
    (printf *STANDARD-OUTPUT* "INVALID\nget-dependencies: please give be an object file name with an extension, not %S\n" object-file)
    (error "get-dependencies: please give be an object file name with an extension, not %S" object-file))
  (let* ((base-name    (regexp:match-string 1 object-file))
         (extension    (regexp:match-string 2 object-file))
         (*extensions* (if (or (string-equal extension ".fas")
                               (string-equal extension ".fasl")
                               (string-equal extension ".x86f"))
                         (append *extensions-clisp* *extensions-emacs*)
                         (append *extensions-emacs* *extensions-clisp*)))
         (source-file
          ;; given the object-file  extension, find the source file extension
          ;; for which a source file exists.
          (do* ((sext-oext *extensions* (cdr  sext-oext))
                (sext  (caar sext-oext) (caar sext-oext))
                (oext  (cdar sext-oext) (cdar sext-oext))
                (sname nil)
                (result nil) )
              ((or (null sext-oext)
                   (progn
                     (setq sname (concatenate 'string base-name sext))
                     (setq result (probe-file sname))))
               (if result sname nil))  )) )
    (values source-file extension *extensions*)
  ));;get-source-file


(defun get-dependencies (object-file load-paths)
  "
PRE:     Object-file is foo.fas or foo.elc, etc.
RETURN:  A list of dependency for this object-file, including the source-file
         and all the object files of required files.
"
  (multiple-value-bind
      (source-file extension *extensions*) (get-source-file object-file)
    (when source-file
      (cons source-file
            (flatten
             (mapcar ;; for each required file
              (lambda (item)
                (mapcar ;; find the object file path corresponding to an
                 ;;        existing source file path.
                 (lambda (sext-oext)
                   (let* ((sname (concatenate 'string item (car sext-oext)))
                          (spath (find-file-path sname load-paths)))
                     (if spath
                         (if (STRING= "." (dirname spath))
                             (concatenate 'string item extension)
                           (concatenate 'string
                             (dirname spath) "/" item extension))
                       nil)))
                 *extensions*))
              (get-requires source-file))))
      ))
  );;get-dependencies


(defun get-closed-dependencies (object-file load-paths)
  "
RETURN: A list of object files recursively required by OBJECT-FILE.
"
  (multiple-value-bind
      (source-file extension *extensions*)
      (get-source-file object-file)
    (when source-file
      (cons object-file
            (flatten
             (mapcar ;; for each required file
              (lambda (item)
                (mapcar ;; find the object file path corresponding to an
                 ;;        existing source file path.
                 (lambda (sext-oext)
                   (let* ((sname (concatenate 'string item (car sext-oext)))
                          (spath (find-file-path sname load-paths)))
                     (if spath
                         (get-closed-dependencies
                          (if (STRING= "." (dirname spath))
                              (concatenate 'string item extension)
                            (concatenate 'string
                              (dirname spath) "/" item extension))
                          load-paths)
                       nil)))
                 *extensions*))
              (get-requires source-file))))))
  );;get-closed-dependencies


(defun get-depends (object-files load-paths)
  "
RETURN:     A list of (cons object-file dependency-list)
"
  (mapcar (lambda (object) (cons object (get-dependencies object load-paths)))
          object-files))


(defun make-depends (object-files load-paths)
  "
DO:         Writes to *STANDARD-OUTPUT* Makefile rules for the object-files.
"
  (dolist (object object-files)
    (printf *STANDARD-OUTPUT* "%s :: %s\n" object
            (unsplit-string (get-dependencies object load-paths) " "))))



;;; (let ((*STANDARD-OUTPUT* (current-buffer)))
;;;   (make-depends (mapcar (lambda (x) (concatenate 'string x "c"))
;;;                         (directory "[a-zA-Z]*.el" :relative-paths  t))
;;;                 '("." "../common-lisp"))


;;;; pjb-make-depends.el              --                     --          ;;;;


