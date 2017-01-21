;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               pjb-graph.el
;;;;LANGUAGE:           emacs-lisp
;;;;SYSTEM:             emacs
;;;;USER-INTERFACE:     emacs
;;;;DESCRIPTION
;;;;
;;;;    Graph class.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon
;;;;MODIFICATIONS
;;;;    2003-05-14 <PJB> Extracted from pjb-cvs.el
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2003 - 2011
;;;;    mailto:pjb@informatimago.com
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
;;;;****************************************************************************

(require 'pjb-cl);; Common-Lisp (cl.el, and some more functions).
(require 'eieio);; CLOS like.
(require 'pjb-object)

(require 'pjb-class)
(require 'pjb-strings)

(require 'pjb-utilities);; import printf

(provide 'pjb-graph)

;;; cat pjb-graph.el | sed -n  -e '/^(def/s/^/;; /p'

;; (defclass PjbElement ()
;; (defmethod* description ((self PjbElement))
;; (defmethod* setProperty ((self PjbElement) (prop-name symbol) prop-value)
;; (defmethod* getProperty ((self PjbElement) (prop-name symbol))
;; (defmethod* deleteProperty ((self PjbElement) (prop-name symbol))

;; (defclass PjbSet (PjbElement)
;; (defmethod* description ((self PjbSet))
;; (defmethod* cardinal ((self PjbSet))
;; (defmethod* containsElement ((self PjbSet) (anElement PjbElement))
;; (defmethod* addElement ((self PjbSet) (newElement PjbElement))
;; (defmethod* addElements ((self PjbSet) newElementList)
;; (defmethod* removeElement ((self PjbSet) (oldElement PjbElement))
;; (defmethod* performWithElements ((self PjbSet) lambda-body)
;; (defmethod* mapElements ((self PjbSet) lambda-body)
;; (defmethod* selectElements ((self PjbSet) select-lambda)

;; (defclass PjbHashedSet (PjbSet)
;; (defmethod* containsElement ((self PjbHashedSet) (anElement PjbElement))
;; (defmethod* addElement ((self PjbHashedSet) (newElement PjbElement))

;; (defclass PjbEdge (PjbElement)
;; (defmethod* description ((self PjbEdge))
;; (defgeneric copy ((self PjbEdge))
;; (defgeneric nodes ((self PjbEdge))
;; (defgeneric isBetweenNodes ((self PjbEdge)
;; (defgeneric successor-of ((self PjbEdge) (node PjbElement))

;; (defclass PjbWeightMixin ()
;; (defmethod* setWeight ((self PjbWeightMixin) (newWeight integer))
;; (defun node-cons-p (item)

;; (defclass PjbUndirectedEdge (PjbEdge)
;; (defmethod* copy ((self PjbUndirectedEdge))
;; (defun identical-nodes (nodes-cons-a nodes-cons-b)
;; (defmethod* isBetweenNodes ((self PjbEdge)
;; (defmethod* successor-of ((self PjbUndirectedEdge) (node PjbElement))
;; (defmethod* setNodes ((self PjbUndirectedEdge)

;; (defclass PjbWeightedUndirectedEdge (PjbUndirectedEdge PjbWeightMixin)
;; (defmethod* description ((self PjbWeightedUndirectedEdge))

;; (defclass PjbDirectedEdge (PjbEdge)
;; (defmethod* copy ((self PjbDirectedEdge))
;; (defmethod* nodes ((self PjbDirectedEdge))
;; (defmethod* isBetweenNodes ((self PjbEdge)
;; (defmethod* successor-of ((self PjbDirectedEdge) (node PjbElement))
;; (defmethod* setNodes ((self PjbDirectedEdge)

;; (defclass PjbWeightedDirectedEdge (PjbDirectedEdge PjbWeightMixin)
;; (defmethod* description ((self PjbWeightedDirectedEdge))
;; (defmethod* copy ((self PjbWeightedDirectedEdge))
;; (defun subclass-of-edge-p (item)

;; (defclass PjbGraph (PjbElement)
;; (defmethod* description ((self PjbGraph))
;; (defmethod* addNode ((self PjbGraph) (newNode PjbElement))
;; (defmethod* addNodes ((self PjbGraph) newNodeList)
;; (defmethod* removeNode ((self PjbGraph) (oldNode PjbElement))
;; (defmethod* removeNodes ((self PjbGraph) oldNodeList)
;; (defmethod* addEdge ((self PjbGraph) (newEdge PjbEdge))
;; (defmethod* addEdgeBetweenNodes ((self PjbGraph)
;; (defmethod* removeEdge ((self PjbGraph) (oldEdge PjbEdge))
;; (defmethod* removeEdges ((self PjbGraph) edge-list)
;; (defmethod* removeEdgesBetweenNodes ((self PjbGraph)
;; (defmethod* edgesBetweenNodes ((self PjbGraph)
;; (defmethod* directedEdgesBetweenNodes ((self PjbGraph)
;; (defmethod* directedEdgesFromNode ((self PjbGraph) (fromNode PjbElement))
;; (defmethod* successorNodes ((self PjbGraph) (node PjbElement))
;; (defmethod* adjacentNodes ((self PjbGraph) (node PjbElement))
;; (defmethod* flowDistanceFromNode ((self PjbGraph)
;; (defmethod* walkFromNode ((self PjbGraph) (startNode PjbElement) lambda-body)
;; (defmethod* walkEdgesFromNode ((self PjbGraph)
;; (defmethod* copy ((self PjbGraph) &rest keys)
;; (defmethod* show-graph ((self PjbGraph))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass PjbElement ()
  (
   (ident
    :reader   ident
    :type     symbol
    :documentation "A unique symbol identifying this element.")
   (properties
    :initform nil
    :initarg  :properties
    :accessor properties
    :type     list
    :documentation
    "A plist of properties for this elements.
It can be used to store markers while walking sets or graphs containing them.")
   )
  (:documentation
   "An element of a PjbSet."
   )
  );;PjbElement


(defmethod* initialize-instance ((self PjbElement) fields)
  "
DO:     Initalize the instance id.
"
  (setf (slot-value self 'ident)
        (gensym (format "%s-" (upcase (cl:string (class-of self))))))
  self);;initialize-instance


(defmethod* description ((self PjbElement))
  "
RETURN: A string describing this element.
"
  (format "<An instance of %s>" (class-name (class-of self)))
  );;description


;; (defmethod* identicalTo ((self PjbElement) (other PjbElement))
;;   "
;; RETURN: Whether self and other are the same element. (eq self other)
;; "
;;   (eq self other)
;;   );;identicalTo

(defmethod* property-names ((self PjbElement))
  "
RETUR: The list of property names (keys) of properties of this element.
"
  (do ((ps (properties self) (cddr ps))
       (res '()))
      ((null ps) res)
    (push (car ps) res))
  );;property-names


(defmethod* setProperty ((self PjbElement) (prop-name symbol) prop-value)
  "
POST:  (eq (getProperty self prop-name) prop-value)
"
  (setf (slot-value self 'properties)
        (plist-put (properties self) prop-name prop-value))
  );;setProperty


(defmethod* getProperty ((self PjbElement) (prop-name symbol))
  "
RETURN: the property `prop-name' of this element.
"
  (plist-get (properties self) prop-name)
  );;getProperty


(defmethod* deleteProperty ((self PjbElement) (prop-name symbol))
  "
DO:     Remove the property named `prop-name' from the property list of
        this element.
"
  (setf (slot-value self 'properties)
        (plist-remove (properties self) prop-name))
  );;deleteProperty


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass PjbSet (PjbElement)
  (
   (elements
    :initform nil
    :initarg  :elements
    :accessor elements
    :type     list
    :documentation
    "The list of elements in this set.")
   )
  (:documentation
   "A set of elements."
   )
  );;PjbSet


(defmethod* description ((self PjbSet))
  "
RETURN: A string describing this element.
"
  (format "<An instance of %s with %d elements>"
          (class-name (class-of self)) (cardinal self))
  );;description


(defmethod* cardinal ((self PjbSet))
  "
RETURN: The number of elements in this set.
"
  (length (elements self))
  );;cardinal


(defmethod* containsElement ((self PjbSet) (anElement PjbElement))
  "
RETURN: Whether this set contains anElement.
"

;;   (let ((elem-list (elements self))
;;         (result nil))
;;     (while elem-list
;;       (when (eq anElement (car elem-list))
;;         (setq result t)
;;         (setq elem-list nil))
;;       (setq elem-list (cdr elem-list)))
;;     result)
  (memq anElement (elements self))
  );;containsElement


(defmethod* addElement ((self PjbSet) (newElement PjbElement))
  "
PRE:    already_in   = (containsElement self newElement),
        old_cardinal = (cardinal self)
POST:   already_in       ==> (cardinal self) == old_cardinal
        (not already_in) ==> (cardinal self) == (1+ old_cardinal)
                             (containsElement self newElement)
"
  (when (not (containsElement self newElement))
    (setf (slot-value self 'elements) (cons newElement (elements self))))
  );;addElement


(defmethod* addElements ((self PjbSet) newElementList)
  "
DO:     Add each element of the newElementList to this set.
"
  (dolist (newElement newElementList)
    (addElement self newElement))
  );;addElements


(defmethod* removeElement ((self PjbSet) (oldElement PjbElement))
  "
PRE:    already_in   = (containsElement self newElement),
        old_cardinal = (cardinal self)
POST:   already_in       ==> (cardinal self) == (1- old_cardinal),
                             (not (containsElement self oldElement))
        (not already_in) ==> (cardinal self) == old_cardinal
"
  (setf (slot-value self 'elements) (delete oldElement (elements self)))
  );;removeElement


(defmethod* performWithElements ((self PjbSet) lambda-body)
  "
DO:     calls lambda-body with each element in the set.
NOTE:   lambda-body must not change this set.
"
  (mapc lambda-body (elements self))
  );;performWithElements


(defmethod* mapElements ((self PjbSet) lambda-body)
  "
RETURN: the list of results returned by lambda-body called with each element.
NOTE:   lambda-body must not change this set.
"
  (mapcar lambda-body (elements self)))


(defmethod* selectElements ((self PjbSet) select-lambda)
  "
RETURN: A list of elements for which select-lambda returned true.
"
  (let ((result nil))
    (mapc (lambda (elem)
            (when (funcall select-lambda elem)
              (push elem result)))
          (elements self))
    result
    );;let
;;     (dolist (elem (elements self))
;;         (setq result (cons elem result))))
;;     result)
  );;selectElements


(defmethod* element-list ((self PjbSet))
  "
RETURN: A new list of the elements in self.
"
  (map 'list (function identity) (elements self))
  );;element-list


(defmethod* find-elements-with-property ((self PjbSet) (property symbol) value)
  "
RETURN: A list of elements that have as property PROPERTY the value VALUE.
"
  (selectElements self (lambda (elem)
                         (let ((pval (getProperty elem property)))
                           (and pval (equal pval value)))))
  );;find-elements-with-property


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass PjbHashedSet (PjbSet)
  (
   (index
    :initform (lambda () (make-hash-table :test 'eq))
    :initarg :index
    :accessor index
    :type     hash-table
    :documentation "A hashtable used to index the elements in this set.")
   )
  (:documentation "This is a specialized kind of set that maintains a hashtable
index of its elements to be able to retrieve them rapidly.")
  );;PjbHashedSet


(defmethod* containsElement ((self PjbHashedSet) (anElement PjbElement))
  "
RETURN: Whether this set contains anElement.
"
  (gethash anElement (index self))
  );;containsElement


(defmethod* addElement ((self PjbHashedSet) (newElement PjbElement))
  "
PRE:    already_in   = (containsElement self newElement),
        old_cardinal = (cardinal self)
POST:   already_in       ==> (cardinal self) == old_cardinal
        (not already_in) ==> (cardinal self) == (1+ old_cardinal)
                             (containsElement self newElement)
"
  (call-next-method)
  (setf (gethash newElement (index self)) newElement)
  );;addElement



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass PjbEdge (PjbElement)
  ()
  (:documentation
   "An abstract  edge."
   )
  );;PjbEdge


(defmethod* description ((self PjbEdge))
  "
RETURN: A string describing this element.
"
  (let ((nodes (nodes self)))
    (format "<A %s between { %s and %s }>"
            (class-name (class-of self))
            (description (car  nodes))
            (description (cdr nodes))))
  );;description


(defgeneric copy ((self PjbEdge))
  "
RETURN: A copy of this edge.
        The copy has the same  nodes than this edge.
        Other attributes are normally copied.
"
  );;copy


(defgeneric nodes ((self PjbEdge))
  "
RETURN: A cons containing the two nodes of the edge, in no specific order.
        (Subclasses implementing directed edges should add specific methods
         to get the `from' and the `to' nodes).
"
  );;nodes


(defgeneric isBetweenNodes ((self PjbEdge)
                            (nodeA PjbElement)  (nodeB PjbElement))
  "
RETURN: Whether this edge is between `nodeA' and `nodeB'.
        If this edge is directed then `nodeA' is compared to the from node
                                  and `nodeB' is compared to the  to  node,
        otherwise, the node order is not important.
"
  );;isBetweenNodes


(defgeneric successor-of ((self PjbEdge) (node PjbElement))
  "
RETURN: If node is a node of the edge, then return its successor or nil.
        That is, for an undirected edge e,
             (and (eq (successor-of e (car (nodes e))) (cdr (nodes e)))
                  (eq (successor-of e (cdr (nodes e))) (car (nodes e))) )
        while for a directed edge d:
             (xor (eq (successor-of e (car (nodes e))) (cdr (nodes e)))
                  (eq (successor-of e (cdr (nodes e))) (car (nodes e))) )
"
  );;nodes


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass PjbWeightMixin ()
  (
   (weight
    :initform 1
    :initarg  :weight
    :accessor weight
    :type     number
    :documentation
    "The weight of the edge."
    )
   )
  (:documentation
   "This is a mixin for the subclasses of PjbEdge to add a weight to the edge."
   )
  );;PjbWeightMixin


(defmethod* setWeight ((self PjbWeightMixin) (newWeight integer))
  "
POST:   (equal (weight self) newWeight)
"
  (setf (slot-value self 'weight) newWeight)
  );;setWeight



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun node-cons-p (item)
  "
RETURN: Whether `item' is a cons of two objects kind of PjbElement.
"
  (and (consp item)
       (object-p (car item))
       (object-p (cdr item))
       (is-kind-of (car item) PjbElement)
       (is-kind-of (cdr item) PjbElement) )
  );;node-cons-p


(defclass PjbUndirectedEdge (PjbEdge)
  (
   (nodes
    :initarg :nodes
    :accessor nodes
    :type     (satisfies node-cons-p)
    :documentation
    "A cons containing the two unordered nodes of the edge." )
   )
  (:documentation
   "An undirected edge."
   )
  );;PjbUndirectedEdge


(defmethod* copy ((self PjbUndirectedEdge))
  "
RETURN: A copy of this edge (only with same nodes).
"
  (make-instance (class-of self)
                 :nodes  (nodes self))
  );;copy


(defun identical-nodes (nodes-cons-a nodes-cons-b)
  "
RETURN: Whether nodes-cons-a and nodes-cons-b contain the same nodes.
"
  (or (and (eq (car nodes-cons-a) (car nodes-cons-b))
           (eq (cdr nodes-cons-a) (cdr nodes-cons-b)))
      (and (eq (car nodes-cons-a) (cdr nodes-cons-b))
           (eq (cdr nodes-cons-a) (car nodes-cons-b))))
  );;identical-nodes


;; (defmethod* identicalTo ((self PjbUndirectedEdge) (other PjBElement))
;;   "
;; RETURN: Whether self and other are the same undirected edge.
;; NOTE:   We only compare the nodes linked by this edge, not any other attribute
;;         that could be added by a subclass...
;; "
;;   (and (is-kind-of other PjbUndirectedEdge)
;;        (identical-nodes (nodes self) (nodes other)))
;;   );;identicalTo

(defmethod* isBetweenNodes ((self PjbEdge)
                            (nodeA PjbElement)  (nodeB PjbElement))
  "
RETURN: Whether this edge is between `nodeA' and `nodeB'.
        The node order is not important.
"
  (identical-nodes (nodes self) (cons nodeA nodeB))
  );;isBetweenNodes


(defmethod* successor-of ((self PjbUndirectedEdge) (node PjbElement))
  "
RETURN: If node is a node of this edge, then the other node, else nil.
"
  (let ( (nodes (nodes self)) )
    (cond
     ((eq node (car nodes)) (cdr nodes))
     ((eq node (cdr nodes)) (car nodes))
     (t nil)))
  );;successor-of


(defmethod* setNodes ((self PjbUndirectedEdge)
                     (nodeA PjbElement) (nodeB PjbElement))
  "
DO:     set the nodes of this edge.
"
  (setf (slot-value self 'nodes) (cons nodeA nodeB))
  );;setNodes


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass PjbWeightedUndirectedEdge (PjbUndirectedEdge PjbWeightMixin)
  ()
  (:documentation
   "A weighted, undirected edge."
   )
  );;PjbWeightedUndirectedEdge


(defmethod* description ((self PjbWeightedUndirectedEdge))
  "
RETURN: A string describing this element.
"
  (let ((nodes (nodes self)))
    (format "<A %s between { %s and %s } weighting %S>"
            (class-name (class-of self))
            (description (car  nodes))
            (description (cdr nodes))
            (weight self)))
  );;description

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass PjbDirectedEdge (PjbEdge)
  (
   (from
    :initarg  :from
    :accessor  from
    :type      PjbElement
    :documentation
    "The `from' node of this edge." )
   (to
    :initarg  :to
    :accessor  to
    :type      PjbElement
    :documentation
    "The `to' node of this edge." )
   )
  (:documentation
   "An directed edge."
   )
  );;PjbDirectedEdge


(defmethod* copy ((self PjbDirectedEdge))
  "
RETURN: A copy of this edge (only with same nodes).
"
  (make-instance (class-of self)
                 :from   (from   self)
                 :to     (to     self))
  );;copy


(defmethod* nodes ((self PjbDirectedEdge))
  "
RETURN: A cons containing the two nodes of the edge in no particular order.
NOTE:   Use the accessor methods `from' and `to' to get the wanted node.
"
  (cons (from self) (to self))
  );;nodes


;; (defmethod* identicalTo ((self PjbDirectedEdge) (other PjBElement))
;;   "
;; RETURN: Whether self and other are the same directed edge.
;; NOTE:   We only compare the nodes linked by this edge, not any other attribute
;;         that could be added by a subclass...
;; "
;;   (and (is-kind-of other PjbDirectedEdge)
;;        (identicalTo (from self) (from other))
;;        (identicalTo (to   self) (to   other)) )
;;   );;identicalTo


(defmethod* isBetweenNodes ((self PjbEdge)
                           (nodeA PjbElement)  (nodeB PjbElement))
  "
RETURN: Whether this edge is between `nodeA' and `nodeB'.
        `nodeA' is compared to the from node
        and `nodeB' is compared to the  to  node.

"
  (and (eq (from self) nodeA) (eq (to self) nodeB))
  );;isBetweenNodes


(defmethod* successor-of ((self PjbDirectedEdge) (node PjbElement))
  "
RETURN: If node is the `from'  node of this edge, then the `to' node, else nil.
"
  (if (eq node (from self))  (to self)  nil)
  );;successor-of


(defmethod* setNodes ((self PjbDirectedEdge)
                     (newFrom PjbElement) (newTo PjbElement))
  "
DO:     set the nodes of this edge.
"
  (setf (slot-value self 'from) newFrom)
  (setf (slot-value self 'to)   newTo)
  );;setNodes


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass PjbWeightedDirectedEdge (PjbDirectedEdge PjbWeightMixin)
  ()
  (:documentation
   "A weighted, directed edge."
   )
  );;PjbWeightedDirectedEdge


(defmethod* description ((self PjbWeightedDirectedEdge))
  "
RETURN: A string describing this element.
"
  (let ((nodes (nodes self)))
    (format "<A %s between { %s and %s } weighting %S>"
            (class-name (class-of self))
            (description (car  nodes))
            (description (cdr nodes))
            (weight self)))
  );;description


(defmethod* copy ((self PjbWeightedDirectedEdge))
  "
RETURN: A copy of this edge (only with same nodes).
"
  (make-instance (class-of self)
                 :weight (weight self)
                 :from   (from   self)
                 :to     (to     self))
  );;copy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun subclass-of-edge-p (item)
  "
RETURN: Whether `item' is a subclass of PjbEdge (not PjbEdge itself).
"
  (and (class-p item)
       (is-subclass-of item PjbEdge) )
  );;subclass-of-edge-p


(defclass PjbGraph (PjbElement)
  (
   (nodes
    :initform (lambda () (make-instance 'PjbSet))
    :initarg  :nodes
    :accessor nodes
    :type     PjbSet
    :documentation
    "The set of nodes in this graph.")
   (edges
    :initform (lambda () (make-instance 'PjbSet))
    :initarg :edges
    :accessor edges
    :type     PjbSet
    :documentation
    "The set of edges in this graph.")
   (edge-class
    :initform 'PjbUndirectedEdge
    :initarg  :edge-class
    :accessor edge-class
    ;;:type     (satisfies 'subclass-of-edge-p)
    :documentation
    "The class used to make new edges in this graph.
Default is PjbUndirectedEdge.")
   )
  (:documentation
   "A graph of elements. By default, it's a undirected graph."
   )
  );;PjbGraph


(defmethod* description ((self PjbGraph))
  "
RETURN: A string describing this element.
"
  (format "<A %s with %d nodes and %d edges>"
          (class-name (class-of self))
          (cardinal (nodes self))
          (cardinal (edges self)))
  );;description


(defmethod* addNode ((self PjbGraph) (newNode PjbElement))
  "
DO:     Add newNode to the set of nodes of this graph.
"
  (addElement (nodes self) newNode)
  );;addNode


(defmethod* addNodes ((self PjbGraph) newNodeList)
  "
DO:     Add a list of new nodes to the set of nodes of this graph.
"
  (addElements (nodes self) newNodeList)
  );;addNodes



(defmethod* removeNode ((self PjbGraph) (oldNode PjbElement))
  "
DO:      Remove the oldNode from the graph.
         This implies removing all the edges adjacent to the node too.
"
  (when (containsElement (nodes self) oldNode)
    (removeEdges (selectElements (edges self)
                                 (lambda (edge)
                                   (or (eq oldNode (from edge))
                                       (eq oldNode (to   edge))))))
    );;when
  );;removeNode


(defmethod* removeNodes ((self PjbGraph) oldNodeList)
  "
DO:      Remove all the nodes of the oldNodeList from this graph.
"
  (dolist (node oldNodeList) (removeNode self node))
  );;removeNodes


(defmethod* addEdge ((self PjbGraph) (newEdge PjbEdge))
  "
PRE:    (and (containsElement (nodes self) (nth 0 (nodes newEdge)))
             (containsElement (nodes self) (nth 1 (nodes newEdge))))
DO:     Add a new edge to this graph.
"
  (when (and (containsElement (nodes self) (car (nodes newEdge)))
             (containsElement (nodes self) (cdr (nodes newEdge))))
    (addElement (edges self) newEdge))
  );;addNode


(defmethod* addEdgeBetweenNodes ((self PjbGraph)
                                (nodeA PjbElement) (nodeB PjbElement))
  "
DO:     Create a new edge (of class edge-class) between `nodeA' and `nodeB'.
        and add it to this graph.
        If the edge is directed,
        then `nodeA' is the `from' node and `nodeB' the `to' node.
"
  (let ((edge (make-instance (edge-class self))))
    (setNodes edge nodeA nodeB)
    (addEdge self edge))
  );;addEdgeBetweenNodes


(defmethod* removeEdge ((self PjbGraph) (oldEdge PjbEdge))
  "
DO:     Remove the `oldEdge' from this graph.
"
  (removeElement (edges self) oldEdge)
  );;removeEdge


(defmethod* removeEdges ((self PjbGraph) edge-list)
  "
DO:     Remove all the edges in edge-list from this graph.
"
  (dolist (edge edge-list)  (removeEdge self edge))
  );;removeEdges


(defmethod* removeEdgesBetweenNodes ((self PjbGraph)
                                    (nodeA PjbElement) (nodeB PjbElement))
  "
DO:     Remove all edges between `nodeA' and `nodeB'.
"
  (mapc (lambda (edge)
          (removeEdge self edge))
        (edgesBetweenNodes self nodeA nodeB))
  );;removeEdgesBetweenNodes


(defmethod* edgesBetweenNodes ((self PjbGraph)
                              (nodeA PjbElement) (nodeB PjbElement))
  "
RETURN: A list of edges existing between the `nodeA' and `nodeB'.
        If the graph is directed then `nodeA' corresponds to the from node
                                  and `nodeB' corresponds to the  to  node.
"
  (selectElements (edges self)
                  (lambda (edge)
                    (isBetweenNodes edge nodeA nodeB)))
  );;edgesBetweenNodes


(defmethod* directedEdgesBetweenNodes ((self PjbGraph)
                                      (fromNode PjbElement) (toNode PjbElement))
  "
RETURN: A list of edges existing from the `fromNode' and to the `toNode'.
"
  (selectElements (edges self)
                  (lambda (edge)
                    (eq (successor-of edge fromNode) toNode)))
  );;directedEdgesBetweenNodes


(defmethod* directedEdgesFromNode ((self PjbGraph) (fromNode PjbElement))
  "
PRE:    edge-class is-subclass-of PjbDirectedEdge
        or edge-class eq PjbDirectedEdge.
RETURN: A list of edges existing from the `fromNode'.
"
  (unless (or (eq (edge-class self) PjbDirectedEdge)
              (is-subclass-of (edge-class self) PjbDirectedEdge))
    (error
     "This graph is not a directed graph. Can't apply directedEdgesFromNode."))

  (selectElements (edges self)
                  (lambda (edge) (eq (from edge) fromNode)))
  );;directedEdgesBetweenNodes


(defmethod* successorNodes ((self PjbGraph) (node PjbElement))
  "
RETURN: The list of successors nodes of the given node in this graph.
NOTE:   For undirected graphs, it's the same as adjacentNodes.
"
  (let ((result nil))
    (performWithElements
     (edges self)
     (lambda (edge)
       (let ( (succ (successor-of edge node)) )
         (when succ
           (unless (memq succ result) (push succ result))))))
    result)
  );;successorNodes


(defmethod* adjacentNodes ((self PjbGraph) (node PjbElement))
  "
RETURN: The list of nodes adjacent to the given node in this graph.
NOTE:   For directed graphs, an adjacent node is either a predecessor
        or a successors of the node.
"
  (let ((result nil))
    (performWithElements
     (edges self)
     (lambda (edge)
       (let ((ns (nodes edge)))
         (cond
          ((eq node (car ns))
           (unless (memq (cdr ns) result) (push (cdr ns) result)))
          ((eq node (cdr ns))
           (unless (memq (car ns) result) (push (car ns) result)))
          ))))
    result)
  );;adjacentNodes


(defmethod* flowDistanceFromNode ((self PjbGraph)
                                 (startNode PjbElement) (prop-name symbol))
  "
DO:     Compute for each node in this graph the distance from the startNode,
        and store it as a property named prop-name.
NOTE:   If the graph is not connex, then some distances will be nil,
        meaning infinity.
"
  (performWithElements (nodes self) (lambda (node)
                                      (setProperty node prop-name nil)))
  (when (containsElement (nodes self) startNode)
    (setProperty startNode prop-name 0)
    (let ( (cur-nodes (list startNode))
           cur-node distance suc-nodes suc-dist )
      (while cur-nodes
        (setq cur-node (car cur-nodes))
        (setq cur-nodes (cdr cur-nodes))
        (setq distance (1+ (getProperty cur-node prop-name)))
        ;; (not (null distance))
        (setq suc-nodes (successorNodes self cur-node))
        (dolist (suc-node suc-nodes)
          (setq suc-dist (getProperty suc-node prop-name))
          (when (or (null suc-dist) (< distance suc-dist))
            (setProperty suc-node prop-name distance)
            (unless (memq suc-node cur-nodes) (push suc-node cur-nodes))
            );;when
          );;dolist
        );;while
      );;let
    );;when
  );;flowDistanceFromNode


(defmethod* walkFromNode ((self PjbGraph) (startNode PjbElement) lambda-body)
  "
DO:     Walk the graph starting form startNode, calling lambda-body
        with each node as argument.
"
  (let ((stamp (gensym "walked-")))

    (when (containsElement (nodes self) startNode)
      (performWithElements (nodes self)
                           (lambda (node) (setProperty node stamp nil)))


      (let ( (cur-nodes (list startNode))
             cur-node  suc-nodes  )
        (while cur-nodes
          (setq cur-node  (car cur-nodes)
                cur-nodes (cdr cur-nodes))

          (setProperty cur-node stamp t)
          (funcall lambda-body cur-node)

          (setq suc-nodes (successorNodes self cur-node))
          (dolist (suc-node suc-nodes)
            (unless (getProperty suc-node stamp)
              (push suc-node cur-nodes)) )
          );;while
        );;let

      (performWithElements (nodes self)
                           (lambda (node) (deleteProperty node stamp)))

      );;when
    );;let
  );;walkFromNode


(defmethod* walkEdgesFromNode ((self PjbGraph)
                              (startNode PjbElement) lambda-body)
  "
DO:     Walk the graph starting form startNode, calling lambda-body
        with each edges as argument. Since it's the edges that are passed
        to lambda-body, one node can be \"walked\" several times either as
        `from' or `to' node or different edges.
"
  (let ((stamp (gensym "walked-")))

    (when (containsElement (nodes self) startNode)

      (performWithElements (edges self)
                           (lambda (item) (setProperty item stamp nil)))
      (performWithElements (nodes self)
                           (lambda (item) (setProperty item stamp nil)))


      (setProperty startNode stamp t)
      (let ( (cur-nodes (list startNode))
             cur-node  suc-nodes  )
        (while cur-nodes
          (setq cur-node  (car cur-nodes)
                cur-nodes (cdr cur-nodes))
          (dolist (edge (directedEdgesFromNode self cur-node))
            (unless (getProperty edge stamp)
              (setProperty edge stamp t)
              (funcall lambda-body edge)
              (unless (getProperty (to edge) stamp)
                (setProperty (to edge) stamp t)
                (push (to edge) cur-nodes))
              );;unless edge alread walked
            );;do-list
          );;while
        );;let

      (performWithElements (edges self)
                           (lambda (item) (deleteProperty item stamp)))
      (performWithElements (nodes self)
                           (lambda (item) (deleteProperty item stamp)))

      );;when
    );;let
  );;walkEdgesFromNode



(defmethod* copy ((self PjbGraph) &rest keys)
  "
RETURN: A copy of this graph.
NOTE:   By default, the nodes are the same, but the edges are duplicated.

        The following keys are recognized:

            :copy-nodes       (==> :copy-edge)
            :no-copy-nodes    (default)
            :copy-edges       (default)
            :no-copy-edges    (==> :no-copy-node)

       The following combination are valid:

            :copy-nodes    :copy-edge    You get a deep copy of the graph,
                                         where you can change anything
                                         independtly from the orginal.

            :no-copy-nodes :copy-edge    You get a new graph with new edges,
                                         but the same nodes.

            :no-copy-nodes :no-copy-edge You get a new graph with the same
                                         edges and the same nodes. But you
                                         still can add or remove nodes or
                                         edges to make it different from the
                                         original graph.
"
  (let ((copy-nodes    (memq :copy-nodes    keys))
        (no-copy-nodes (memq :no-copy-nodes keys))
        (copy-edges    (memq :copy-edges    keys))
        (no-copy-edges (memq :no-copy-edges keys))
        new-nodes
        new-edges
        new-elements
        node-hash
        (copy-stamp    (gensym "copy-"))
        )
    (when (and copy-nodes no-copy-nodes)
      (error "Can't have both :copy-nodes and :no-copy-nodes."))
    (when (and copy-edges no-copy-edges)
      (error "Can't have both :copy-edges and :no-copy-edges."))
    (when (and copy-nodes no-copy-edges)
      (error "Can't have both :copy-nodes and :no-copy-edges."))
    (unless copy-nodes    (setq no-copy-nodes t))
    (unless no-copy-edges (setq copy-edges    t))
    (when   no-copy-edges (setq no-copy-nodes t))
    (when   copy-nodes    (setq copy-edges    t))
    (assert (or (and (not copy-nodes) no-copy-nodes)
                (and  copy-nodes (not no-copy-nodes))))
    (assert (or (and (not copy-edges) no-copy-edges)
                (and  copy-edges (not no-copy-edges))))

    (if copy-nodes
        (progn
          (setq node-hash (make-hash-table :test 'eq
                                           :size (cardinal (nodes self))))
          (setq new-elements
                (mapElements (nodes self)
                             (lambda (node)
                               (let ((new-node (copy node)))
                                 (setf (gethash node node-hash) new-node)
                                 new-node))) )
          (setq new-nodes (make-instance PjbSet :elements new-elements))
          );;progn
      (setq new-nodes (nodes self))
      );;copy-nodes

    (if copy-edges
        (progn
          (setq new-elements
                (mapElements (edges self)
                             (lambda (edge)
                               (let ((new-edge (copy edge))
                                     nodes)
                                 (when copy-nodes
                                   (setq nodes (nodes new-edge))
                                   (setNodes new-edge
                                             (gethash (car nodes) node-hash)
                                             (gethash (cdr nodes) node-hash))
                                   );;when copy-node
                                 new-edge
                                 ))))
          (setq new-edges (make-instance PjbSet :elements new-elements))
          );;progn
      (setq new-edges (edges self))
      );;copy-edges

    (make-instance (class-of self) :nodes new-nodes
                   :edges new-edges
                   :edge-class (edge-class self))
    );;let
  );;copy

(defmethod* find-nodes-with-property ((self PjbGraph) (property symbol) value)
  "
RETURN: A list of nodes that have as property PROPERTY the value VALUE.
"
  (find-elements-with-property (nodes self) property value)
  );;find-nodes-with-property


(defmethod* show-graph ((self PjbGraph))
  (printf "%s {\n" (description self))

  (performWithElements
   (nodes self)
   (lambda (node) (printf "   node %s\n"  (description node))) )

  (performWithElements
   (edges self)
   (lambda (edge) (printf "   edge %s\n" (description edge))) )

  (printf "}\n")
  );;show-graph

;;;; pjb-graph.el                     -- 2003-05-14 19:55:01 -- pascal   ;;;;
