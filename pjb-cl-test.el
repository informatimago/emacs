;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-cl-test.el --- Tests for pjb-cl
(require 'ert)
(defvar pjb-cl-test--this-file (or load-file-name buffer-file-name)
  "Path to this test file, captured at load time so individual tests
can use it (load-file-name is unset once tests run).")
(add-to-list 'load-path (file-name-directory pjb-cl-test--this-file))
(require 'pjb-cl)

(ert-deftest pjb-cl-test/namestring-identity ()
  (should (string= "/tmp/foo.txt" (namestring "/tmp/foo.txt"))))

(ert-deftest pjb-cl-test/file-namestring ()
  (should (string= "foo.txt" (file-namestring "/tmp/foo.txt")))
  (should (string= "foo.txt" (file-namestring "foo.txt"))))

(ert-deftest pjb-cl-test/directory-namestring ()
  (should (string= "/tmp/" (directory-namestring "/tmp/foo.txt"))))

(ert-deftest pjb-cl-test/pathname-host-is-nil ()
  (should (null (pathname-host "/tmp/foo.txt"))))

(ert-deftest pjb-cl-test/pathname-directory ()
  (should (string= "/tmp/" (pathname-directory "/tmp/foo.txt"))))

(ert-deftest pjb-cl-test/pathname-name ()
  (should (string= "foo" (pathname-name "/tmp/foo.txt"))))

(ert-deftest pjb-cl-test/pathname-name-no-ext ()
  (should (eq :unspecific (pathname-name "/tmp/foo"))))

(ert-deftest pjb-cl-test/pathname-type ()
  (should (string= "txt" (pathname-type "/tmp/foo.txt"))))

(ert-deftest pjb-cl-test/pathname-type-none ()
  (should (eq :unspecific (pathname-type "/tmp/foo"))))

(ert-deftest pjb-cl-test/probe-file-existing ()
  (should (equal pjb-cl-test--this-file (probe-file pjb-cl-test--this-file))))

(ert-deftest pjb-cl-test/probe-file-missing ()
  (should (null (probe-file "/no/such/file/here/at/all.xyz"))))

(ert-deftest pjb-cl-test/truename-existing ()
  (should (stringp (truename pjb-cl-test--this-file))))

(ert-deftest pjb-cl-test/file-write-date-existing ()
  (should (numberp (file-write-date pjb-cl-test--this-file))))

(ert-deftest pjb-cl-test/get-universal-time ()
  ;; Pinned reality: pjb-cl returns a float, not an integer.
  (should (numberp (get-universal-time))))

(ert-deftest pjb-cl-test/get-decoded-time-shape ()
  (let ((dt (multiple-value-list (get-decoded-time))))
    (should (listp dt))
    (should (>= (length dt) 6))))

(ert-deftest pjb-cl-test/lisp-implementation-type ()
  (should (stringp (lisp-implementation-type))))

(ert-deftest pjb-cl-test/machine-instance ()
  (should (stringp (machine-instance))))

(ert-deftest pjb-cl-test/software-type ()
  (should (stringp (software-type))))

(ert-deftest pjb-cl-test/short-site-name ()
  (should (stringp (short-site-name))))

(ert-deftest pjb-cl-test/long-site-name ()
  (should (stringp (long-site-name))))

(ert-deftest pjb-cl-test/alpha-char-p-letters ()
  (should (alpha-char-p ?a))
  (should (alpha-char-p ?Z)))

(ert-deftest pjb-cl-test/alpha-char-p-digit ()
  (should-not (alpha-char-p ?5)))

(ert-deftest pjb-cl-test/upper-case-p ()
  (should (upper-case-p ?A))
  (should-not (upper-case-p ?a))
  (should-not (upper-case-p ?5)))

(ert-deftest pjb-cl-test/char= ()
  (should (char= ?a ?a))
  (should-not (char= ?a ?b)))

(ert-deftest pjb-cl-test/char< ()
  (should (char< ?a ?b))
  (should-not (char< ?b ?a)))

(ert-deftest pjb-cl-test/array-dimension-1d ()
  (should (= 5 (array-dimension (make-vector 5 0) 0))))

(provide 'pjb-cl-test)
