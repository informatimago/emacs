;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;***
;;;;FILE:               pjb-constants-test.el
;;;;DESCRIPTION
;;;;    Unit tests for pjb-constants.el
;;;;LEGAL
;;;;    GPL
;;;;    Copyright Pascal Bourguignon
;;;;***
(require 'pjb-constants)
(require 'ert)

(defgroup pjb-constants-tests nil
  "Test suite for pjb-constants")

(ert-deftest test/pjb-constants--physical-constant-p-valid ()
  "Test physical-constant-p returns true for defined constants."
  (assert (physical-constant-p 'c))
  (assert (physical-constant-p 'R))
  (assert (physical-constant-p 'earth-mass))
  (assert (physical-constant-p 'G))
  :success)

(ert-deftest test/pjb-constants--physical-constant-p-invalid ()
  "Test physical-constant-p returns nil for undefined names."
  (assert (not (physical-constant-p 'not-a-constant)))
  (assert (not (physical-constant-p 'random-symbol)))
  :success)

(ert-deftest test/pjb-constants--unit-valid ()
  "Test unit returns correct unit string for valid constants."
  (assert (equal (unit 'c) '(* m (^ s -1))))
  (assert (equal (unit 'R) '(* J (^ K -1) (^ mol -1))))
  (assert (equal (unit 'G) '(* (^ kg -1) (^ m 3) (^ s -2))))
  :success)

(ert-deftest test/pjb-constants--unit-invalid ()
  "Test unit signals error for invalid constant names."
  (should-error (unit 'not-a-constant))
  :success)

(ert-deftest test-pjb-constants--known-physics-constants-exist ()
  "Test that known physics constants have expected values."
  ;; Speed of light should be 299792458.0
  (assert (= c 299792458.0))
  ;; Boltzmann constant should be 1.38066e-23
  (assert (= k 1.38066e-23))
  ;; Gravitational constant G
  (assert (= G 6.672e-11))
  ;; Planck constant h
  (assert (= h 6.626176e-34))
  :success)

(ert-deftest test-pjb-constants--earth-constants-exist ()
  "Test that earth physical constants are defined."
  ;; Earth mass
  (assert (> earth-mass 0))
  ;; Earth radius
  (assert (> earth-radius-equatorial 0))
  ;; Earth density
  (assert (> earth-density 0))
  :success)

(ert-deftest test-pjb-constants--sun-constants-exist ()
  "Test that sun physical constants are defined."
  ;; Sun mass should be very large
  (assert (= sun-mass 1.989e+30))
  ;; Sun radius
  (assert (= sun-equatorial-radius 695e6))
  :success)

(ert-deftest test-pjb-constants--def-phys-const-registering ()
  "Test that def-phys-const registers constants in *constantes*."
  (let ((count (length *constantes*)))
    ;; Should have at least one constant defined (e.g., c, R, etc.)
    (assert (> count 0)))
  :success)

(provide 'pjb-constants-tests)
;;;; THE END ;;;;
