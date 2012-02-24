;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               pjb-dot.el
;;;;LANGUAGE:           emacs-lisp
;;;;SYSTEM:             emacs-lisp
;;;;USER-INTERFACE:     emacs-lisp
;;;;DESCRIPTION
;;;;    
;;;;    Generate dot files from graphs (pjb-graph).
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon
;;;;MODIFICATIONS
;;;;    2003-05-14 <PJB> Extracted from pjb-cvs.
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
(require 'pjb-cl)
(require 'pjb-graph)
(provide 'pjb-dot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating dot files 

(defun dot-ident (ident)
  "
RETURN: A string containing the ident with the dash removed.
"
  (remove (character "-") (string* ident)))



(defmethod generate-dot ((self PjbElement))
  "
RETURN: A string containing the dot file data for this PjbElement node.
"
  (let ((style     (or (getProperty self :dot-style) "filled"))
        (color     (or (getProperty self :dot-color) "black"))
        (fillcolor (or (getProperty self :dot-fill-color) "LightYellow"))
        (label     (or (getProperty self :dot-label)
                       (unsplit-string
                        (mapcar
                         (lambda (prop-name)
                           (if (or 
                                (string-equal ;; for emacs: 
                                 "dot-" (subseq (symbol-name prop-name) 0 4))
                                (string-equal ;; for emacs: 
                                 ":dot-" (subseq (symbol-name prop-name) 0 5)))
                             ""
                             (format "%s = %s" prop-name
                                     (getProperty self prop-name))))
                         (property-names self))
                        "\n")
                       (dot-ident (ident self)))))
    (format "%S [ style=%s color=%s fillcolor=%s label=\"%s\" ];\n"
            (dot-ident (ident self)) style color fillcolor label)
    ));;generate-dot




(defmethod generate-dot ((self PjbDirectedEdge))
  "
RETURN: A string containing the dot file data for this edge.
"
  (format "%S -> %S ;\n"
          (dot-ident (ident (from self)))
          (dot-ident (ident (to   self))))
  );;generate-dot


(defmethod generate-dot ((self PjbWeightedDirectedEdge))
  "
RETURN: A string containing the dot file data for this edge.
"
  (format "%S -> %S [ weight=%d, style=%s, color=%s ];\n"
          (dot-ident (ident (from self)))
          (dot-ident (ident (to   self)))
          (weight self)
          (cond
           ((< (weight self) 3)  "dotted")
           ((< (weight self) 10) "dashed")
           ((< (weight self) 15) "solid")
           (t                    "bold"))
          "black"
          );;format
  );;generate-dot


;;; (description (car (element-list (nodes g))))
;;; (mapElements (nodes g) (lambda (elem) (ident elem)))
;;; (car (element-list (nodes g)))


(defmethod generate-dot ((self PjbGraph) (name string))
  "
RETURN: A string containing the dot file data for this graph.
NOTE:   dot graphs are directed.
"
  (apply 
   'concat
   (flatten
    (list
     (format "digraph %S\n" name)
     "{\n"
     ;; attributes of graph:
     "// page=\"8,11.4\"; // page size (NeXTprinter:A4).\n"
     "// size=\"30,8\";  // graph size (please edit to fit).\n"
     "// rotate=90;     // graph orientation (please edit to fit).\n"
     "// ratio=fill;\n // fill the size (or compress, auto, aspect/ratio).\n"
     "  nodesep=0.3;\n"
     "  ranksep=0.3;\n"
     "  center=1;\n"
     ;; common attributes of nodes:
     "  node [height=0.2 width=0.5 shape=box fontsize=8 fontname=Times];\n"
     (mapElements (nodes self) (lambda (node) (generate-dot node)))
     ;; common attributes of edges:
     "  edge [style=solid];\n" 
     (mapElements (edges self) (lambda (edge) (generate-dot edge)))
     "}\n")))
  );;generate-dot


;;;; pjb-dot.el                       --                     --          ;;;;
