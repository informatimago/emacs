;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-graph-test.el --- Tests pinning a slice of the public API of pjb-graph.el

(require 'ert)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; pjb-graph.el uses the EIEIO `defgeneric' macro, which was renamed
;; to `cl-defgeneric' (and `defgeneric' removed) several Emacs versions
;; ago.  Loading the file therefore aborts with (void-function defgeneric).
;; Phase 2/3 should rewrite the EIEIO definitions with `cl-defgeneric'
;; / `cl-defmethod'.  Until then we pin the load failure here so the
;; fix is detectable.
(defvar pjb-graph-test--load-error
  (condition-case err (require 'pjb-graph) (error err)))

(ert-deftest pjb-graph/load-currently-fails ()
  "Pin the current load failure so the Phase 2/3 EIEIO fix is detected."
  (should pjb-graph-test--load-error))

(ert-deftest pjb-graph/api-when-loaded ()
  (skip-unless (null pjb-graph-test--load-error))
  (should (functionp 'identical-nodes))
  (should (fboundp  'subclass-of-edge-p))
  (should (boundp   'PjbElement)))

;;; pjb-graph-test.el ends here
