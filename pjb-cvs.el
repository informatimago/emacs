;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               pjb-cvs.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    This module exports an application that analyses CVS revision graphs.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2002-04-06 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2002 - 2011
;;;;
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This library is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    Lesser General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Lesser General Public
;;;;    License along with this library; if not, write to the Free Software
;;;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
;;;;
;;;;****************************************************************************
(require 'pjb-object)
(require 'pjb-graph)
(require 'pjb-dot)
(provide 'pjb-cvs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; revision = (file,version)
;; file = { revision=(file,*) }
;; tag  = { {file_i,version_j), k!=l ==> file_k!=file_l }
;;
;; arc_file ( (file,v1),(file,v2) ) si v2 succède v1 (+branches)
;; arc_tag  ( (tag1,tag) si existe arc_file(n1,n2) avec n1 dans tag1
;;                                                   et n2 dans tag2 )
;; poind(arc_tag(tag1,tag2)) = cardinal { arc_file(n1,n2) avec n1 dans tag1
;;                                                          et n2 dans tag2 }
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass CvsAttributedMixin ()
  ((attributes-plist
    :initform nil
    :initarg  :attributes-plist
    :accessor  attributes-plist
    :type      list
    :documentation
    "A plist of attributes."))
  (:documentation
   "A mixin to store attributes as a plist."))


(defmethod* setAttribute ((self CvsAttributedMixin)
                          (attrName (or symbol string)) (attrValue string))
  "
POST:   (string-equal attrValue (getAttribute self attrName))
"
  (when (stringp attrName)
    (setq attrName (intern attrName)))
  (setf (slot-value self 'attributes-plist)
        (plist-put (attributes-plist self) attrName attrValue)))


(defmethod* getAttribute ((self CvsAttributedMixin)
                          (attrName (or symbol string)))
  "
RETURN: The value of the attribute attrName.
"
  (when (stringp attrName)
    (setq attrName (intern attrName)))
  (plist-get (attributes-plist self) attrName))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some functions to manipulate revision strings ("1.2.3.1"...)
;; We convert them to vectors [1 2 3 1] instead...

(defun string-to-revision (rev-string)
  "
RETURN: A vector containing the revision in binary form.
"
  (let* ( (rev-list   (split-string rev-string "\\."))
          (rev-vector (make-vector (length rev-list) 0))
          (i 0)
          )
    (dolist (value rev-list)
      (aset rev-vector i (string-to-number value))
      (setq i (1+ i)))
    rev-vector))


(defun revision-to-string (rev)
  "
RETURN: A string containing the numbers in the rev.
"
  (unsplit-string (mapcar 'number-to-string rev) "."))


(defun revision-less-p (rev-a rev-b)
  (let ((i 0)
        (len-a (length rev-a))
        (len-b (length rev-b))
        )
    (while (and (< i len-a) (< i len-b) (= (aref rev-a i) (aref rev-b i)))
           (setq i (1+ i)))
    (or (and (= i len-a) (< i len-b))
        (and (< i len-a) (< i len-b) (< (aref rev-a i) (aref rev-b i))))))


(defun revision-magic-p (rev)
  "
RETURN: Whether the revision rev is a CVS magic revision, ie. its length is odd
        or the number before the last is 0.
"
  (or (oddp (length rev)) (= 0 (aref rev (- (length rev) 2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass CvsFile (PjbElement CvsAttributedMixin)
  ((name
    :initform nil
    :initarg  :name
    :accessor name
    :type     (or null string)
    :documentation "RCS file name.")
   (revisions
    :initform (lambda () (make-instance 'PjbSet))
    :initarg  :revisions
    :accessor revisions
    :type     PjbSet
    :documentation "A set of CvsRevision of this file.")
   (revision-graph
    :initform (lambda () (make-instance 'PjbGraph :edge-class PjbDirectedEdge))
    :initarg  :revision-graph
    :accessor  revision-graph
    :type      PjbGraph
    :documentation
    "The graph of the revisions of this file.
The edges denotes the derivation order of the revisions.
Note however that merges can't be known from the file revision numbers alone."))
  (:documentation
   "A CVS File (RCS file)."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass CvsRevision (PjbElement CvsAttributedMixin)
  ((file
    :initform nil
    :initarg  :file
    :accessor  file
    :type      (or null CvsFile)
    :documentation
    "The CVS file of which this is a revision.")
   (file-version
    :initform "1.0"
    :initarg  :file-version
    :accessor file-version
    :type     string
    :documentation "The CVS version of this revision.")
   (tags
    :initform (lambda () (make-instance 'PjbSet))
    :initarg  :tags
    :accessor tags
    :type     PjbSet
    :documentation
    "The tags that include this revision.")
   (name
    :initform nil
    :accessor cached-name
    :type     (or null string)
    :documentation "Derived name."))
  (:documentation
   "A CVS File (RCS file)."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CvsFile methods:

(defmethod* description ((self CvsFile))
  "
RETURN: A string describing this element.
"
  (format "<A %s named %S with %d revisions>"
          (class-name (class-of self))
          (name self)
          (cardinal (revisions self))))


(defmethod* setName ((self CvsFile) (newName string))
  "
POST:   (string-equal name (name self))
"
  (setf (slot-value self 'name) newName))


(defmethod* addRevision ((self CvsFile) (newRevision CvsRevision))
  "
PRE:    (eq (file newRevision) self)
DO:     Add the newRevision to the list of revisions of this file.
"
  (when (eq (file newRevision) self)
    (addElement (revisions self) newRevision)))



(defmethod* revisionWithVersion ((self CvsFile) (aVersion string))
  "
RETURN: The revision with the version.
"
  (let ( (rev (selectElements
               (revisions self)
               (lambda (element)
                 (string-equal aVersion (file-version element))))) )
    (if rev (car rev) nil)))


(defmethod* addRootRevisionIfMissing ((self CvsFile))
  "
DO:     If there's no revision 1.1, then add one.
RETURN: The root revision (ie. the revision 1.1).
"
  (let ((result (revisionWithVersion self "1.1")))
    (unless result
      (setq result (make-instance 'CvsRevision :file self :file-version "1.1"))
      (addRevision self result))
    result))



(defun pjb-cvs$$eat-revision (revision-root revision-list eat-revision-add-edge)
  "
PRIVATE. Used by `computeRevisionGraph'.
"
  (let ((current (car revision-list)))
    (while (and revision-list
                (<= (length (car revision-root)) (length (car current))))
           ;;(show revision-list)
           (while (and revision-list
                       (= (length (car revision-root)) (length (car current))))
                  (funcall eat-revision-add-edge revision-root current)
                  (setq revision-root current)
                  (setq revision-list (cdr revision-list))
                  (setq current (car revision-list)))
           (when (and revision-list
                      (< (length (car revision-root)) (length (car current))))
             (funcall eat-revision-add-edge revision-root current)
             (setq revision-list (pjb-cvs$$eat-revision current (cdr revision-list)
                                                        eat-revision-add-edge))
             (setq current (car revision-list)))))
  revision-list)


(defmethod* computeRevisionGraph ((self CvsFile))
  "
DO:     Compute the revision graph from the revision numbers of this file.
POST:   (revision-graph self) is a directed tree (directed cycleless graph).
RETURN: (revision-graph self)
"
  (let* ((sorted-revisions
           (sort
            (mapElements (revisions self)
                         (lambda (element)
                           (cons (string-to-revision (file-version element))
                                 element)))
            (lambda (vra vrb) (revision-less-p (car vra) (car vrb)))))
         (graph (make-instance 'PjbGraph
                               :nodes      (revisions self)
                               :edge-class PjbDirectedEdge))
         (eat-revision-add-edge
           (lambda  (from to)
             (addEdgeBetweenNodes graph (cdr from) (cdr to)))) )
    (pjb-cvs$$eat-revision (car sorted-revisions) (cdr sorted-revisions)
                           eat-revision-add-edge)
    (setf (slot-value self 'revision-graph) graph)
    (revision-graph self)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CvsRevision methods:

(defmethod* description ((self CvsRevision))
  "
RETURN: A string describing this element.
"
  (format "<A %s of file %S, version %s>"
          (class-name (class-of self))
          (name (file self))
          (file-version self)))


(defmethod* name ((self CvsRevision))
  "
RETURN: A name for this revision instance.
"
  (unless (cached-name self)
    (setf (slot-value self 'name)
          (format "%s-%s" (name (file self)) (file-version self))) )
  (cached-name self))



;; (defmethod* identicalTo ((self CvsRevision) (other PjbElement))
;;   "
;; RETURN:  Whether self and other are identical, that is, if they have the
;;          same file and their versions are equal.
;; "
;;   (or (eq self other)
;;       (and (is-kind-of other CvsRevision)
;;            (eq           (file self)         (file other))
;;            (string-equal (file-version self) (file-version other))))
;;   )



(defmethod* isMagicBranchRevision ((self CvsRevision))
  "
RETURN: Whether this revision file-version is a magic branch revision number.
"
  ;;elispism:
  (string-match "^.*\.0\.[0-9]+$" (file-version self)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass CvsTag (PjbSet)
  ;; ------------------------------------------------------------ ;;
  ;; ------------ CvsTag is a PjbSet of CvsRevision ------------- ;;
  ;; ------------------------------------------------------------ ;;
  ((name
    :initform nil
    :initarg  :name
    :accessor name
    :type     symbol
    :documentation "symbolic name of the tag.")
   (date
    :initform nil
    :accessor cached-date
    :type     (or null string)
    :documentation "Derived date, from revisions."))
  (:documentation
   "A tag is a named set of revisions. Its name identifies it."))


(defmethod* addTag ((self CvsRevision) (tag CvsTag))
  "
POST:    (containsElement (tags self) tag)
"
  (addElement (tags self) tag))


(defmethod* description ((self CvsTag))
  "
RETURN: A string describing this element.
"
  (format "<A %s named %S>"
          (class-name (class-of self))
          (name self)))


;; (defmethod* identicalTo ((self CvsTag) (other PjbElement))
;;   "
;; RETURN:  Whether self and other are identical, that is,
;;          if they have the same name.
;; "
;;   (or (eq self other)
;;       (and (is-kind-of other CvsTag)
;;            (eq (name self) (name other))))
;;   )


(defmethod* setName ((self CvsTag) (newName (or string symbol)))
  "
POST:    (eq (intern newName) (name self))
"
  (when (stringp newName)
    (setq newName (intern newName)))
  (setf (slot-value self 'name) newName))


(defun getRevisionList-dates (tag)
  "
RETURN: A list containing the dates of the revisions of tag.
"
  ;; (declare (tag CvsTag))
  (mapcar (lambda (revision) (getAttribute revision "date"))
          (elements tag)))


(defmethod* date ((self CvsTag))
  "
RETURN: The date of the youngest revision in this tag.
"
  (unless (cached-date self)
    (setf (slot-value self 'date)
          (car  (sort
                 (mapcar (lambda (revision) (getAttribute revision "date"))
                         (elements self))
                 'string> ))))
  (cached-date self))


(defmethod* revisionForFile ((self CvsTag) (file CvsFile))
  "
RETURN: The revision of the file `file' in this tag.
"
  (car (selectElements self (lambda (revision) (eq file (file revision))))))


(defmethod* isBranchTagForFile ((self CvsTag) (file CvsFile))
  "
RETURN: Whether this tag is a branch tag for the given `file', that is,
        the penultimian number of the version number of the file's revision
        in this tag is 0.
"
  (let ( (revision (revisionForFile self file)) )
    (if revision  (isMagicBranchRevision revision)   nil)))



(defmethod* getRevisionList ((self CvsTag))
  "
RETURN: A list containing the file revisions in the form of conses
        (file-name . version) of this tag.
"
  (mapcar (lambda (revision) (cons (name (file revision))
                                   (file-version revision)))
          (elements self)))



(defun pjb-cvs%find-tag-named (tag-set tag-name)
  "
RETURN: The tag named `tag-name' in the set of CvsTag `tags'.
"
  ;; (declare (tag-set PjbSet) (tag-name symbol))
  (car (selectElements tag-set (lambda (node) (eq (name node) tag-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating dot files

(defmethod* generate-dot ((self CvsRevision))
  "
RETURN: A string containing the dot file data for this CvsRevision node.
"
  (format
   "%S [ style=filled color=black fillcolor=LightYellow label=\"%s\\n%s\" ];\n"
   (name self)
   (let* ((got nil)
          (tag (selectElements
                (tags self) (lambda (elem) (if got nil (setq got t)))))
          )
     (if (car tag)  (name (car tag)) ""))
   (file-version self)))


(defmethod* generate-dot ((self CvsTag))
  "
RETURN: A string containing the dot file data for this CvsTag node.
"
  (let ((sn (cl:string (name self))))
    (format  "%S [ style=filled color=Black fillcolor=SkyBlue label=\"%s\\n%s\" ];\n"
             sn sn (date self))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defvar pjb-cvs%*ignore-descriptions* t
  "set to nil to keep the descriptions from cvs log. They tend to be big.")

(defun pjb-cvs%parse-log (cvs-log-output)
  (let ((files    (make-instance 'PjbSet))
        (tags     (make-instance 'PjbSet)))
    (dolist (file-data (split-string cvs-log-output "^=============================================================================\n"))
      ;; processing one file
      (let* ((file-revs-l    (split-string file-data
                                           "^----------------------------\n"))
             (file-lines     (split-string (car file-revs-l) "\n"))
             (revs-l         (cdr file-revs-l))
             (file           (make-instance 'CvsFile))
             (symbolic-names nil)
             line value tag)

        ;; ---------------------
        ;; 1- file header
        (while file-lines
               (setq line (car file-lines))
               (setq file-lines (cdr file-lines))

               (cond
                 ((string-equal line "description:")
                  (unless pjb-cvs%*ignore-descriptions*
                    ;; end of attributes, the rest of lines are description.
                    (when file-lines
                      (setAttribute file "description"
                                    (unsplit-string  file-lines "\n"))
                      (setq file-lines nil))))

                 ((setq value (chop-prefix line "RCS file:"))
                  (setName file (chop-spaces value)))

                 ((setq value (chop-prefix line "cvs server:"))
                  ;; ignore
                  )

                 ((setq value (chop-prefix line "? "))
                  ;; ignore
                  )

                 ((setq value (chop-prefix line "symbolic names:"))
                  ;; nop; the tags follow.
                  )

                 ((setq value (chop-prefix line "\t"))
                  ;; tag (symbolic names)
                  (setq symbolic-names (cons value symbolic-names)))

                 ((setq value (chop-prefix line "total revisions:"))
                  ;; we must split
                  (let ((rsr (split-string value "[;:][\t ]*")))
                    (setAttribute file "total revisions" (car rsr))
                    (setAttribute file (nth 1 rsr) (nth 2 rsr))))
                 (t
                  (setq line (split-string line ": *"))
                  (when (not (string-equal (cadr line) ""))
                    (setAttribute file (car line) (cadr line))))))

        ;; ---------------------
        ;; 2- revisions
        (dolist (rev revs-l)
          (let* ((rev-lines   (split-string rev "\n"))
                 (rev-num     (chop-prefix (car rev-lines) "revision "))
                 (attributes  (split-string (nth 1 rev-lines) "[:;][ \t]+"))
                 (description)
                 (revision    (make-instance 'CvsRevision
                                             :file file
                                             :file-version rev-num)))
            (unless pjb-cvs%*ignore-descriptions*
              (setq description (unsplit-string (cddr rev-lines) "\n"))
              (setAttribute revision "description" description))
            (while attributes
                   (setAttribute revision (car attributes) (cadr attributes))
                   (setq attributes (cddr attributes)))
            (addRevision file revision)))
        ;; ---------------------
        ;; 3- tag symbolic names
        (dolist (sn symbolic-names)
          (let* ((tag-revision (split-string sn ": "))
                 (tag-name     (intern (car tag-revision)))
                 (version      (cadr tag-revision))
                 (tag          (selectElements
                                tags (lambda (elem) (eq (name elem) tag-name))))
                 (revision))
            (if tag
                (setq tag (car tag))
                (setq tag (make-instance 'CvsTag :name tag-name)))
            (setq revision (revisionWithVersion file version))
            (unless revision
              (setq revision (make-instance 'CvsRevision
                                            :file file :file-version version))
              (addRevision file revision))
            (addElement tag revision)
            (addTag revision tag)
            (addElement tags tag)))
        (addElement files file)))
    (cons files tags)))


(defun pjb-cvs%computeTagGraph-2 (files tags)
  "
DO:     Compute the tag graph containing as nodes the tags, and edges
        deduced from the revision graphs of the files.
RETURN: The computed graph.
"
  (let ((graph (make-instance 'PjbGraph
                              :nodes      tags
                              :edges      (make-instance PjbHashedSet)
                              :edge-class PjbWeightedDirectedEdge)))
    ;; check if an INIT tag exists (tag with all revisions 1.1)
    ;; if not, create it.
    ;;
    ;; for each file in files,...
    (performWithElements
     files
     (lambda (file)

       ;; We need to do a walk of the revision tree,
       ;; starting from the root "1.1",
       ;; collecting a "pre-tag" node and a "cur-tag" such as:
       ;;    (not (isMagicBranchRevision pre-rev))
       ;;    (not (isMagicBranchRevision cur-rev))
       ;;    there is a pre-tag in (tags pre-rev)
       ;;    there is a cur-tag in (tags cur-rev)
       ;;    cur-rev is the first successor of pre-rev
       ;;            matching the previous conditions.

       (let ((rev-graph (revision-graph file))
             (rev-stack '()) ; stack of walked revisions: ((rev suc...)...)
             (tev-stack '()) ; stack of tagged revisions.
             cur-item cur-rev cur-suc cur-sucs newEdge edge)
         (setq cur-rev  (revisionWithVersion file "1.1"))
         (setq cur-sucs (successorNodes rev-graph cur-rev))
         (push (cons cur-rev cur-sucs) rev-stack)
         (when (and (not (isMagicBranchRevision cur-rev))
                    (< 0 (cardinal (tags cur-rev))))
           (push cur-rev tev-stack))
         (while rev-stack
                ;;(printf "\nrev-stack=%S\ntev-stack=%S\n"
                ;;  (mapcar (lambda (item) (mapcar 'name item)) rev-stack)
                ;;  (mapcar 'name tev-stack))
                (setq cur-item (pop rev-stack))
                (setq cur-rev  (car cur-item))
                (setq cur-sucs (cdr cur-item))
                (if (null cur-sucs)
                    ;; no more successors, let's pop.
                    (if (eq (car tev-stack) cur-rev)
                        (pop tev-stack))
                    (progn
                      (setq cur-suc (car cur-sucs))
                      ;; We need to push even when there's no other successor
                      ;; to be able to pop when needed.
                      (push (cons cur-rev (cdr cur-sucs)) rev-stack)

                      (if (and (not (isMagicBranchRevision cur-suc))
                               (< 0 (cardinal (tags cur-suc))))
                          (progn
                            ;; got an edge.
;;;                   (printf "%-16s ->  %-16s   %-16S => %S\n"
;;;                           (name cur-rev) (name cur-suc)
;;;                           (mapcar 'name (elements (tags (car tev-stack))))
;;;                           (mapcar 'name (elements (tags cur-suc))))
                            (if (car tev-stack)
                                (dolist (ftag (elements (tags (car tev-stack))))
                                  (dolist (ttag (elements (tags cur-suc)))
                                    (setq edge
                                          (car (edgesBetweenNodes graph ftag ttag)))
                                    ;; There should be only one edge from ftag to ttag.
                                    (if edge
                                        (setWeight edge (1+ (weight edge)))
                                        (addEdgeBetweenNodes graph ftag ttag)))))
                            (push cur-suc tev-stack))
                          ;;(printf "%-16s ->  %-16s\n" (name cur-rev) (name cur-suc))
                          )
                      (setq cur-rev cur-suc)
                      (setq cur-sucs (successorNodes rev-graph cur-rev))
                      (push (cons cur-rev cur-sucs) rev-stack)))))))
    graph))



(defun pjb-cvs%computeTagGraph (files tags)
  (let ((graph (make-instance 'PjbGraph
                              :nodes      tags
                              :edges      (make-instance PjbHashedSet)
                              :edge-class PjbWeightedDirectedEdge)))
    ;; for each tag
    ;;     for each revision in tag     (Files of revisions are unique)
    ;;         for each successor of revision
    ;;             find tags where successor belongs
    ;;             (if there's no tag, get successor's successor's).
    ;;             add and edge from current tag to these successor tags
    ;;             or increment existing edge weight.
    (performWithElements
     tags
     (lambda (tag)
       (printf "%s\n" (name tag))
       (performWithElements
        tag;; is a set of revision.
        (lambda (revision)

          (message "%-20s %-47s %s"
                   (name tag) (name (file revision)) (file-version revision))
          ;;(message "cardinal(edges)=%d" (cardinal (edges graph)))
          ;; successors are in the file's revision graph.
          (let* ( (rev-graph  (revision-graph (file revision)))
                  (successors (successorNodes rev-graph revision))
                  (checked    '())
                 successor succ-tags edge newEdge)
            (printf  "file=%S\nrevision=%S successors=%S\n\n" (name (file revision)) (file-version revision) (mapcar (lambda (s) (file-version s)) successors))
            ;;(message "cardinal(successors)=%d" (length successors))
            (while successors
                   (setq successor  (car successors)
                         successors (cdr successors)
                         succ-tags  (tags successor))

                   (unless (memq successor checked)
                     (push successor checked)

                     ;;(message "cardinal(tags successor)=%d" (if  succ-tags (cardinal succ-tags) 0))

                     (if (null succ-tags)

                         ;; let's look the successor's successors.
                         (dolist (new-succ (successorNodes rev-graph successor))
                           (unless (or (memq new-succ checked)
                                       (memq new-succ successors))
                             (push new-succ successors)))
                         ;; let's add edges or increment the existing edges' weight.
                         (performWithElements
                          (tags successor)
                          (lambda (to-tag)
                            (setq newEdge (make-instance PjbWeightedDirectedEdge
                                                         :from tag :to to-tag))
                            (setq edge (containsElement (edges graph) newEdge))
                            ;; There should be only one edge from tag to to-tag.
                            (if edge
                                (setWeight edge (1+ (weight edge)))
                                (addEdge graph newEdge))))))))))))
    graph))



(defun pjb-cvs%get-tag-graph-from-cvs-in-dir (dir-path &optional file-list)
  "
NOTE:   When file-list is given, then only cvs log those files
        (relative from dir-path).
RETURN: A (list tag-graph files tags) containing the tag-graph (PjbGraph),
        the files (PjbSet of CvsFile) and the tags (PjbSet of CvsTag).
"
  (let* ((cvs-log-out-fname (format "/tmp/%s" (gensym "cvs-log-output-")))
         (cmd (format "cd %s ; cvs log %s > %s 2> /dev/null"
                      (shell-quote-argument dir-path)
                      (if file-list
                          (unsplit-string file-list " ")
                          "")
                      (shell-quote-argument cvs-log-out-fname)))
         (cvs-log-out (progn (shell-command cmd nil nil)
                             (string-from-file-literally cvs-log-out-fname)))
         (pjb-cvs%*ignore-descriptions* t)
         (files-tags  (pjb-cvs%parse-log cvs-log-out))
         (files       (car files-tags))
         (tags        (cdr files-tags))
         tag-graph)
    (delete-file cvs-log-out-fname)
    (message "Got %d files, and %d tags." (cardinal files) (cardinal tags))
    (performWithElements files
                         (lambda (file)
                           (addRootRevisionIfMissing file)
                           (computeRevisionGraph file)))
    ;;    (setq tag-graph (pjb-cvs%computeTagGraph-2 files tags))
    (list tag-graph files tags)))


(defun pjb-cvs%order-edges-chronologically (graph)
  "
PRE:    graph is a graph of CvsTag.
DO:     All edges such as (STRING> (date (from edge)) (date (to edge)))
        are reversed (ie. their from and to nodes are exchanged).
"
  (performWithElements
   (edges graph)
   (lambda (edge)
     (when (STRING> (date (from edge)) (date (to edge)))
       (setNodes edge (to edge) (from edge))))))

;;;; THE END ;;;;

