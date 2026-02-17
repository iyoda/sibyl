;;;; suite.lisp — Test suite definition for Sibyl

(defpackage #:sibyl.tests
  (:use #:cl #:fiveam)
  (:export #:sibyl-tests #:run-sibyl-tests))

(in-package #:sibyl.tests)

(def-suite sibyl-tests
  :description "Top-level test suite for Sibyl.")

(in-suite sibyl-tests)

;;; Placeholder test to verify the suite loads
(test sanity-check
  "Basic sanity check."
  (is (= 1 1))
  (is (string= "sibyl" (string-downcase "SIBYL"))))

(defun run-sibyl-tests ()
  "Run the full test suite with self-assess nested execution prevention.
Binds *self-assess-running* to T so that any self-assess calls during
the test run skip re-executing the full suite and use cached results instead."
  (let ((sibyl.tools:*self-assess-running* t))
    (fiveam:run! 'sibyl-tests)))
