;;;; -*- mode:emacs-lisp;coding:utf-8;lexical-binding:t -*-
;;;;***
;;;;FILE:               pjb-echo-keys-test.el
;;;;DESCRIPTION
;;;;    Unit tests for pjb-echo-keys.el
;;;;LEGAL
;;;;    AGPL3
;;;;    Copyright Pascal Bourguignon
;;;;***
(require 'pjb-echo-keys)
(require 'ert)

(defgroup pjb-echo-keys-tests nil
  "Test suite for pjb-echo-keys")

(ert-deftest test/pjb-echo-keys--variables-initialized ()
  "Test that variables are initialized."
  ;; Variables should exist and have initial values
  (assert (boundp '*echo-keys-last*))
  (assert (boundp '*echo-keys-count*))
  (assert (boundp '*echo-key-width*))
  (assert (eq *echo-key-password-disable* nil))
  (assert (eq *echo-key-coallesce-repeats* nil))
  :success)

(ert-deftest test/pjb-echo-keys--echo-key-width-default ()
  "Test default width of echo-keys window."
  (assert (eq *echo-key-width* 40))
  :success)

(ert-deftest test/pjb-echo-keys--echo-keys-count-initial ()
  "Test initial count is 0."
  (assert (eq *echo-keys-count* 0))
  :success)

(ert-deftest test/pjb-echo-keys--toggle-echo-keys-structure ()
  "Test toggle-echo-keys function exists and is callable."
  ;; This tests that the function exists (cannot fully test without window)
  (assert (fboundp 'toggle-echo-keys))
  :success)

(ert-deftest test/pjb-echo-keys--echo-keys-function-exists ()
  "Test echo-keys function exists."
  (assert (fboundp 'echo-keys))
  :success)

(ert-deftest test/pjb-echo-keys--echo-key-password-disable-toggle ()
  "Test that password disable variable can be toggled."
  (let ((original *echo-key-password-disable*))
    (setf *echo-key-password-disable* t)
    (assert *echo-key-password-disable*)
    (setf *echo-key-password-disable* nil)
    (assert (not *echo-key-password-disable*))
    (setf *echo-key-password-disable* original))
  :success)

(ert-deftest test/pjb-echo-keys--coalesce-repeats-toggle ()
  "Test that coalesce-repeats variable can be toggled."
  (let ((original *echo-key-coallesce-repeats*))
    (setf *echo-key-coallesce-repeats* t)
    (assert *echo-key-coallesce-repeats*)
    (setf *echo-key-coallesce-repeats* nil)
    (assert (not *echo-key-coallesce-repeats*))
    (setf *echo-key-coallesce-repeats* original))
  :success)

(provide 'pjb-echo-keys-tests)
;;;; THE END ;;;;
