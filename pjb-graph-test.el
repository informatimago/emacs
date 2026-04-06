;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-graph-test.el --- Tests pinning a slice of the public API of pjb-graph.el

(require 'ert)
(require 'cl) ;; pjb-graph.el still uses unprefixed `do' / `assert' etc.
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'pjb-graph)

;; --- Pure helper predicates ---------------------------------------------

(ert-deftest pjb-graph/identical-nodes-same-order ()
  (let ((a 'na) (b 'nb))
    (should (identical-nodes (cons a b) (cons a b)))))

(ert-deftest pjb-graph/identical-nodes-swapped ()
  (let ((a 'na) (b 'nb))
    (should (identical-nodes (cons a b) (cons b a)))))

(ert-deftest pjb-graph/identical-nodes-different ()
  (should-not (identical-nodes (cons 'a 'b) (cons 'a 'c))))

;; `subclass-of-edge-p' depends on EIEIO class introspection that
;; behaves differently in modern Emacs (PjbUndirectedEdge as a
;; *symbol-value* of the class object is no longer a class itself).
;; Phase 5 (Objective-style EIEIO retirement) will revisit.

;; --- PjbElement properties ----------------------------------------------

(ert-deftest pjb-graph/element-default-properties ()
  (let ((e (make-instance 'PjbElement)))
    (should (eq nil (properties e)))))

(ert-deftest pjb-graph/element-set-get-property ()
  (let ((e (make-instance 'PjbElement)))
    (setProperty e 'color 'red)
    (should (eq 'red (getProperty e 'color)))))

(ert-deftest pjb-graph/element-delete-property ()
  (let ((e (make-instance 'PjbElement)))
    (setProperty e 'color 'red)
    (deleteProperty e 'color)
    (should (eq nil (getProperty e 'color)))))

;; --- PjbSet basics ------------------------------------------------------

(ert-deftest pjb-graph/set-empty-cardinal ()
  (let ((s (make-instance 'PjbSet)))
    (should (= 0 (cardinal s)))))

(ert-deftest pjb-graph/set-add-and-contains ()
  (let ((s (make-instance 'PjbSet))
        (e (make-instance 'PjbElement)))
    (addElement s e)
    (should (= 1 (cardinal s)))
    (should (containsElement s e))))

(ert-deftest pjb-graph/set-add-duplicate-noop ()
  (let ((s (make-instance 'PjbSet))
        (e (make-instance 'PjbElement)))
    (addElement s e)
    (addElement s e)
    (should (= 1 (cardinal s)))))

(ert-deftest pjb-graph/set-remove-element ()
  (let ((s (make-instance 'PjbSet))
        (e (make-instance 'PjbElement)))
    (addElement s e)
    (removeElement s e)
    (should (= 0 (cardinal s)))
    (should-not (containsElement s e))))

(ert-deftest pjb-graph/set-add-elements-list ()
  (let ((s (make-instance 'PjbSet))
        (e1 (make-instance 'PjbElement))
        (e2 (make-instance 'PjbElement))
        (e3 (make-instance 'PjbElement)))
    (addElements s (list e1 e2 e3))
    (should (= 3 (cardinal s)))))

;; PjbUndirectedEdge construction with `:nodes` no longer initialises
;; the slot the way the original EIEIO did (modern EIEIO is stricter
;; about :initform vs :initarg).  Phase 5 will revisit; for now we
;; cover the rest of the API.

;;; pjb-graph-test.el ends here
