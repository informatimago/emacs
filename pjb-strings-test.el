;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;; pjb-strings-test.el --- Tests for pjb-strings
(require 'ert)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'pjb-strings)

(ert-deftest pjb-strings/ensure-string-string ()
  (should (equal "hello" (ensure-string "hello"))))

(ert-deftest pjb-strings/ensure-string-symbol ()
  (should (equal "foo" (ensure-string 'foo))))

(ert-deftest pjb-strings/list-to-string ()
  (should (equal "abc" (list-to-string '(?a ?b ?c)))))

(ert-deftest pjb-strings/cut-string-exact-multiple ()
  ;; Pin reality: cut-string only works when the input length is an exact
  ;; multiple of LENGTH; on a remainder it errors with args-out-of-range
  ;; because `(subseq s i (+ i length))' doesn't clamp.  Phase 2 should fix.
  (should (equal '("abc" "def") (cut-string "abcdef" 3)))
  (should-error (cut-string "abcdefg" 3) :type 'args-out-of-range))

(ert-deftest pjb-strings/is-digit ()
  (should (is-digit ?5))
  (should (is-digit "7"))
  (should-not (is-digit ?a))
  (should-not (is-digit nil)))

(ert-deftest pjb-strings/is-letter ()
  (should (is-letter ?a))
  (should (is-letter ?Z))
  (should-not (is-letter ?5)))

(ert-deftest pjb-strings/string-position ()
  (should (= 2 (string-position "abcdef" "cd")))
  (should (null (string-position "abcdef" "xy"))))

(ert-deftest pjb-strings/unsplit-string ()
  (should (equal "a b c" (unsplit-string '("a" "b" "c"))))
  (should (equal "a-b-c" (unsplit-string '("a" "b" "c") "-"))))

(ert-deftest pjb-strings/string-repeat ()
  (should (equal "" (string-repeat 0 "ab")))
  (should (equal "abab" (string-repeat 2 "ab")))
  (should (equal "ababab" (string-repeat 3 "ab"))))

(ert-deftest pjb-strings/string-has-prefix ()
  (should (string-has-prefix "hello world" "hello"))
  (should-not (string-has-prefix "hello world" "world")))

(ert-deftest pjb-strings/string-has-suffix ()
  (should (string-has-suffix "hello world" "world"))
  (should-not (string-has-suffix "hello world" "hello")))

(ert-deftest pjb-strings/chop-spaces ()
  (should (equal "hello" (chop-spaces "  hello  ")))
  (should (equal "a b" (chop-spaces "  a b  "))))

(ert-deftest pjb-strings/chop-prefix ()
  (should (equal " world" (chop-prefix "hello world" "hello")))
  (should (null (chop-prefix "hello" "xyz"))))

(ert-deftest pjb-strings/first-last-char ()
  (should (equal ?h (first-char "hello")))
  (should (equal ?o (last-char "hello")))
  (should (null (first-char ""))))

(ert-deftest pjb-strings/butlast-butfirst-char ()
  (should (equal "hell" (butlast-char "hello")))
  (should (equal "ello" (butfirst-char "hello"))))

(ert-deftest pjb-strings/prefixp-suffixp ()
  (should (prefixp "hel" "hello"))
  (should (suffixp "llo" "hello"))
  (should-not (prefixp "xyz" "hello")))

;;;; pjb-strings-test.el                  --                     --          ;;;;
