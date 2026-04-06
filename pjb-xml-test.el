;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-xml-test.el --- Tests for pjb-xml
(require 'ert)
(require 'cl) ;; pjb-xml.el uses `defun*' from cl
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'pjb-xml)

(ert-deftest pjb-xml-test/make-doc-without-dtd ()
  (should (equal (make-doc :root '(html nil "x"))
                 '((html nil "x")))))

(ert-deftest pjb-xml-test/make-doc-with-dtd ()
  (should (equal (make-doc :dtd '(dtd "html") :root '(html nil))
                 '((dtd "html") (html nil)))))

(ert-deftest pjb-xml-test/doc-has-dtd-true ()
  (should (doc-has-dtd '((dtd "html") (html nil)))))

(ert-deftest pjb-xml-test/doc-has-dtd-false ()
  (should-not (doc-has-dtd '((html nil)))))

(ert-deftest pjb-xml-test/doc-dtd ()
  (should (equal (doc-dtd '((dtd "html") (html nil)))
                 '(dtd "html")))
  (should (null (doc-dtd '((html nil))))))

(ert-deftest pjb-xml-test/doc-root-with-dtd ()
  (should (equal (doc-root '((dtd "html") (html nil "body")))
                 '(html nil "body"))))

(ert-deftest pjb-xml-test/doc-root-without-dtd ()
  (should (equal (doc-root '((html nil "body")))
                 '(html nil "body"))))

(ert-deftest pjb-xml-test/doc-root-leading-nil ()
  (should (equal (doc-root '(nil (html nil "body")))
                 '(html nil "body"))))

(ert-deftest pjb-xml-test/make-xml-node ()
  (should (equal (make-xml-node :name 'p :attributes '((class . "x")) :children '("hi"))
                 '(p ((class . "x")) "hi"))))

(ert-deftest pjb-xml-test/xml-node-p-true ()
  (should (xml-node-p '(p nil "hello"))))

(ert-deftest pjb-xml-test/xml-node-p-false-on-string ()
  (should-not (xml-node-p "hello")))

(ert-deftest pjb-xml-test/attribute-helpers ()
  (let ((a (make-xml-attribute 'class "foo")))
    (should (eq 'class (xml-attribute-name a)))
    (should (string= "foo" (xml-attribute-value a)))))

(ert-deftest pjb-xml-test/xml-quote-attribute-value-plain ()
  (should (string= "1.0" (xml-quote-attribute-value "1.0"))))

(ert-deftest pjb-xml-test/xml-quote-attribute-value-quotes ()
  (should (string= "There is no \\\"spoon\\\"!"
                   (xml-quote-attribute-value "There is no \"spoon\"!"))))

(ert-deftest pjb-xml-test/xml-quote-attribute-value-backslash ()
  (should (string= "A\\\\nB" (xml-quote-attribute-value "A\\nB"))))

(provide 'pjb-xml-test)
