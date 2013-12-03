;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               loader.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Load pjb emacs sources.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-02-25 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2011
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************


(defun load-stuff (files &optional show-messages)
  (dolist (path files)
    (unless 
        (condition-case cc
            (load path  *pjb-load-noerror*  *pjb-load-silent*)
          ('error
           (setq show-messages t)
           (message (format "ERROR: %S" cc ))))))
  (when show-messages
    (switch-to-buffer "*Messages*")
    (split-window-vertically))) 


(defvar *pjb-sources* '())

(setf *pjb-sources*
      '(
        "pjb-advices.el"
        "pjb-asm7090.el"
        "pjb-blink.el"
        "pjb-cl-magic.el"
        "pjb-cl.el"
        "pjb-class.el"
        "pjb-dodo.el"
        "pjb-emacs.el"
        "pjb-erc.el"
        "pjb-euro.el"
        "pjb-font.el"
        "pjb-html.el"
        "pjb-list.el"
        "pjb-gnus.el"
        "pjb-mail.el"
        "pjb-object.el"
        "pjb-page.el"
        "pjb-pgp.el"
        "pjb-queue.el"
        "pjb-ruby.el"
        "pjb-shell.el"
        "pjb-sources.el"
        "pjb-state-coding.el"
        "pjb-strings.el"
        "pjb-utilities.el"
        "pjb-work.el"
        "pjb-xresources.el"
        "pjb-thi.el"
        "pjb-c-style.el"

        "pjb-java"
        
        "pjb-objc-edit.el" 
        "pjb-objc-gen.el"
        "pjb-objc-ide.el"
        ;; not yet ;; "pjb-objc-mode.el"
        "pjb-objc-parser.el"
        "android-classes.el"
        "pjb-pl1.el" ; testing
        ))


(unless *pjb-light-emacs*
  (setf *pjb-sources*
        (append *pjb-sources*
                '(

                  "pjb-vm-kill-file.el"

                  "pjb-computer-paper.el"
                  "pjb-constants.el"
                  "pjb-cvs.el"
                  "pjb-cvspass.el"
                  "pjb-dot.el"
                  "pjb-graph.el"


                  "pjb-cl-faces.el"
                  "pjb-cl-magic-lambda-lists.el"

                  "pjb-i2p-expression.el"
                  "pjb-s2p-expression.el"

                  "pjb-layers.el"
                  "pjb-make-depends.el"
                  "pjb-roman.el"
                  "pjb-secouer.el"
                  "pjb-server.el"
                  "pjb-transpose.el"

                  "pjb-worldfact.el"

                  "pjb-banks.el"
                  "pjb-bourse.el"
                  "pjb-selftrade.el"

                  ))))


 (when nil
   '(
     "pjb-banks-old.el"
     "pjb-c.el"
     "pjb-objc-mode.el"
     "pjb-comint"
     "slime-rpc.el"
     "split.el"
     ))






 ;; (load-stuff
 ;;  (let ((files-not-to-load
 ;;         (append
 ;;          ;; files NEVER to load.
 ;;          '("pjb-emacs-cl"
 ;;            "pjb-c"
 ;;            "pjb-w3"
 ;;            "pjb-objc-mode"
 ;;            "pjb-banks-old")
 ;;          (when *pjb-light-emacs*
 ;;            ;; files NOT to load when light
 ;;            '("pjb-banks"
 ;;              "pjb-bourse"
 ;;              "pjb-cl-faces"
 ;;              "pjb-computer-paper"
 ;;              "pjb-constants"
 ;;              "pjb-cvs"
 ;;              "pjb-cvspass"
 ;;              "pjb-dot"
 ;;              "pjb-graph"
 ;;              "pjb-i2p-expression"
 ;;              "pjb-invoices"
 ;;              "pjb-layers"
 ;;              "pjb-make-depends"
 ;;              "pjb-roman"
 ;;              "pjb-s2p-expression"
 ;;              "pjb-secouer"
 ;;              "pjb-selftrade"
 ;;              "pjb-server"
 ;;              "pjb-transpose"
 ;;              "pjb-vm-kill-file"
 ;;              "pjb-worldfact")))))
 ;;    (remove-if
 ;;     (lambda (file) (member* file files-not-to-load :test (function string=)))
 ;;     (let ((home-path (namestring (user-homedir-pathname))))
 ;;       (append
 ;;        ;; all the files
 ;;        (mapcar
 ;;         (function pathname-name)
 ;;         (file-expand-wildcards ; DIRECTORY doesn't work on "pjb-*" yet.
 ;;          (if (file-directory-p (concatenate 'string home-path "src/public/emacs"))
 ;;              (concatenate 'string home-path "src/public/emacs/pjb-*.el")
 ;;            (get-directory :share-lisp "packages/com/informatimago/emacs/pjb-*.el"))))
 ;;        (list ;; some additional dynamic data:
 ;;         (concatenate 'string home-path ".emacs-devises")))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Let's make a topological sort of *pjb-sources* to load them in the
;;; right order.
;;;
;;; First search all the toplevel require forms in the pjb-sources.
;;; Then sort them.
;;; Finally loaded them.


(defvar *el-walk-sexps-end-marker*)


(defun skip-comments ()
  "
DO:     Move the point over spaces and lisp comments ( ;...\n or #| ... |# ),
        in the current buffer.
RETURN: (not eof)
"
  (interactive)
  (let* ((comment-regexp   "\\(#|\\([^|]\\||[^#]\\)*|#\\)\\|\\(;.*$\\)")
         (space-or-comment (format "\\(%s\\)\\|\\(%s\\)" 
                             "[ \t\n\v\f\r]+"
                             comment-regexp)) )
    (while (looking-at space-or-comment)
      (goto-char (match-end 0)))
    (< (point) (point-max))))


(defun el-walk-sexps (fun)
  "
DO:     Recursively scan sexps from (point) in current buffer up to 
        the end-of-file or until scan-sexps raises a scan-error. 
        Call fun on each sexps and each of their children etc.
fun:    A function (sexp start end) 
        sexp:    The sexp parsed from a source file.
        start:   The point starting the sexp.
        end:     The point ending the sexp.
NOTE:   All positions are kept in markers, so modifying the buffer between 
        start and end should be OK.
        However  ' or ` are passed as (quote ...) or (backquote ...) 
        to the function fun without reparsing the sexp inside them. 
        Ie. if you modify such a source, (which can be detected looking at 
        the character at start position),  you still get the original sexp.
"
  (let ((quote-stack '())
        (start-stack '())
        (*el-walk-sexps-end-marker* (make-marker))
        quote-depth
        start-m sexp)
    (skip-comments)
    (when (/= (point) (point-max))
      ;; gather the quotes:
      (while (looking-at "['`] *")
        ;; quote or backquote
        ;; NOT NEEDED ANYMORE WITH GNU Emacs 21.
        ;; --- (push (set-marker (make-marker) (point)) start-stack)
        ;; --- (push (if (= (char-after) ?') 'quote 'backquote) quote-stack)
        (forward-char 1)
        (skip-comments))
      ;; get the sexp:
      (setq start-m (set-marker (make-marker) (point)))
      (forward-sexp 1)
      (set-marker *el-walk-sexps-end-marker* (point))
      ;; (forward-sexp -1)
      ;; (assert (= (marker-position start-m) (point)) t)
      (goto-char (marker-position start-m))
      (setq sexp (sexp-at-point))
      ;; push the quotes on the sexp:
      (setq quote-depth (length quote-stack))
      (while quote-stack 
        (setq sexp (cons (pop quote-stack) (list sexp))))
      ;; process the quotes:
      (setq start-stack (nreverse start-stack))
      (dotimes (i quote-depth)
        ;; (message "sexp = %S\nstart = %S\nend = %S\n" sexp (marker-position (car start-stack)) *el-walk-sexps-end-marker*)
        (funcall fun sexp 
                 (marker-position (car start-stack)) *el-walk-sexps-end-marker*)
        (set-marker (pop start-stack) nil)
        (setq sexp (cadr sexp)))
      ;; process the sexp:
      ;; (message "sexp = %S\nstart = %S\nend = %S\n" sexp  (marker-position start-m) *el-walk-sexps-end-marker*)
      (funcall fun sexp (marker-position start-m)  *el-walk-sexps-end-marker*)
      ;; (when *map-sexps-deeply*
      ;;   (when (= (char-syntax (char-after (marker-position start-m))) 40) ;; "("
      ;;     ;; then the subsexps:
      ;;     (goto-char (marker-position start-m))
      ;;     (down-list 1)
      ;;     (loop
      ;;        (condition-case nil
      ;;            (el-walk-sexps fun)
      ;;          (scan-error (return-from nil))))
      ;;     (up-list 1)))
      ;; then go to the next sexp:
      (goto-char (marker-position *el-walk-sexps-end-marker*))
      (set-marker start-m nil)
      (set-marker *el-walk-sexps-end-marker* nil)))
  nil)


(defun el-map-sexps (source-file fun &rest cl-keys)
  "
DO:     Scan all toplevel sexps in the source file. 
        (skipping spaces and comment between top-level sexps).
fun:    A function (sexp start end) 
        sexp:    The sexp parsed from a source file.
        start:   The point starting the sexp.
        end:     The point ending the sexp.
NOTE:   Scanning stops as soon as an error is detected by forward-sexp.
RETURN: The list of results from fun.
"
  (save-excursion
    (save-restriction
      (let ((old-buffer            (current-buffer))
            (existing-buffer       (get-buffer source-file))
            last-bosexp)
        (if existing-buffer
            (switch-to-buffer existing-buffer)
            (find-file source-file))
        (widen)
        (goto-char (point-min))
        (while (< (point) (point-max))
          (el-walk-sexps fun))
        (if existing-buffer
            (switch-to-buffer old-buffer)
            (kill-buffer (current-buffer)))))))


(defun source-file-requires (path)
  (let ((requires '()))
    (el-map-sexps path
               (lambda (form start end)
                 (when (and (listp form)
                            (eq 'require (first form)))
                   (push (let ((what (second form)))
                           (if (atom what)
                               what
                               (if (eq 'quote (first what))
                                   (second what)
                                   what)))
                         requires))))
    requires))


(defun pathname-name* (path)
  (cond ((string-match "^\\(.*/\\)?\\(.*\\)\\.\\([^.]*\\)\\(\\.~[0-9]+~\\)$" path)
         (match-string 2 path))
        ((string-match "^\\(.*/\\)?\\(.*\\)\\.\\([^.]*\\)$" path)
         (match-string 2 path))
	((string-match "^\\(.*/\\)?\\(.*\\)$" path)
         (match-string 2 path))
        (t :unspecific)))

(defun topological-sort (nodes lessp)
  "
RETURN: A list of NODES sorted topologically according to 
        the partial order function LESSP.
        If there are cycles (discounting reflexivity), 
        then the list returned won't contain all the NODES.
"
  (loop
     with sorted = '()
     with incoming = (map 'vector (lambda (to)
                                    (loop
                                        for from in nodes
                                        when (and (not (eq from to))
                                                   (funcall lessp from to))
                                        sum 1))
                           nodes)
     with q = (loop
                  for node in nodes
                  for inco across incoming
                  when (zerop inco)
                  collect node)
     while q
     do (let ((n (pop q)))
           (push n sorted)
           (loop
              for m in nodes
              for i from 0
              do (when (and (and (not (eq n m))
                                  (funcall lessp n m))
                             (zerop (decf (aref incoming i))))
                    (push m q))))
     finally (return (nreverse sorted))))





 
(defparameter *pjb-sources-order*
  (mapcar (lambda (file)
            (let ((path (concat (if load-file-name
                                  (file-name-directory load-file-name)
                                  (concat (getenv "HOME") "/src/public/emacs/")) 
                                file)))
              (cons (intern (pathname-name* file))
                    (source-file-requires path))))
          *pjb-sources*))


(defun pjb-sources-lessp (a b)
  (let ((aa (assoc a *pjb-sources-order*)))
    (and aa (member b (cdr aa)))))


(defun check-pjb-sources-lessp ()
 (let ((nodes (mapcar (lambda (path) (intern (pathname-name* path))) *pjb-sources*)))
   (loop
      for from in nodes
      do  (loop
             for to in nodes
             when (and (not (eq from to))
                       (pjb-sources-lessp from to)
                       (pjb-sources-lessp to from))
             do (print (list from to))))))

(let ((sorted (topological-sort
               (mapcar (lambda (path) (intern (pathname-name* path))) *pjb-sources*)
               (function pjb-sources-lessp))))
  (when (< (length sorted) (length *pjb-sources*))
    (error "There are circularities in the requires of PJB sources."))
  (setf *pjb-sources* (reverse (mapcar (lambda (name) (format "%s.el" name)) sorted))))

(load-stuff *pjb-sources* (not *pjb-load-silent*))

;;;; THE END ;;;;
