;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-html-test.el --- Tests for pjb-html
(require 'ert)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'pjb-html)

(ert-deftest pjb-html-test/open-tag-p-true ()
  (should (open-tag-p "<div>")))

(ert-deftest pjb-html-test/open-tag-p-false-on-close ()
  (should-not (open-tag-p "</div>")))

(ert-deftest pjb-html-test/close-tag-p-explicit ()
  (should (close-tag-p "</div>")))

(ert-deftest pjb-html-test/close-tag-p-self-closing ()
  (should (close-tag-p "<br />")))

(ert-deftest pjb-html-test/close-tag-p-auto-close-tag ()
  (should (close-tag-p "<br>"))
  (should (close-tag-p "<img src=\"x\">")))

(ert-deftest pjb-html-test/tag-name-open ()
  (should (string= "div" (tag-name "<div>"))))

(ert-deftest pjb-html-test/tag-name-close ()
  (should (string= "div" (tag-name "</div>"))))

(ert-deftest pjb-html-test/tag-name-with-attrs ()
  (should (string= "a" (tag-name "<a href=\"x\">"))))

(ert-deftest pjb-html-test/element-helpers ()
  (let ((e (make-element 'p '((class . "x")) '("hi"))))
    (should (eq 'p (element-name e)))
    (should (equal '((class . "x")) (element-attributes e)))
    (should (equal '("hi") (element-children e)))))

(ert-deftest pjb-html-test/element-name-on-atom ()
  (should-not (element-name "string")))

(ert-deftest pjb-html-test/get-first-child ()
  (should (equal "hi" (get-first-child (make-element 'p nil '("hi" "bye"))))))

(ert-deftest pjb-html-test/single-string-child-p-true ()
  (should (single-string-child-p (make-element 'p nil '("hi")))))

(ert-deftest pjb-html-test/single-string-child-p-false ()
  (should-not (single-string-child-p (make-element 'p nil '("hi" "bye")))))

(ert-deftest pjb-html-test/value-to-boolean ()
  (should (value-to-boolean "true"))
  (should-not (value-to-boolean "false"))
  (should-not (value-to-boolean "yes")))

(ert-deftest pjb-html-test/entity-name-equal-p ()
  (should (entity-name-equal-p "div" "div"))
  (should (entity-name-equal-p '("div" "ns") "div"))
  (should-not (entity-name-equal-p "div" "span")))

(ert-deftest pjb-html-test/pjb-find-html-tag ()
  (let ((tree '(html nil (body nil (p nil "hi")))))
    (should (equal '(p nil "hi") (pjb-find-html-tag 'p tree)))
    (should (null (pjb-find-html-tag 'foo tree)))))

(provide 'pjb-html-test)
