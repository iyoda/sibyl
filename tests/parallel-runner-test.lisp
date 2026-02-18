;;;; parallel-runner-test.lisp — Tests for the parallel test runner

(in-package #:sibyl.tests)

(in-suite sibyl-tests)

;;; ============================================================
;;; Cross-package suite initialization
;;;
;;; suite.lisp is loaded FIRST (before sibyl.agent.tests and
;;; sibyl.evolution.tests packages exist), so those cross-package
;;; symbols cannot be included in the *safe-suites* defparameter.
;;; This file is loaded LAST, so all packages are available here.
;;; Cross-package suites are resolved at runtime by %safe-suites-resolved.
;;; ============================================================

(def-suite parallel-runner-tests
  :description "Tests for the parallel test runner (run-tests-parallel)."
  :in sibyl-tests)

(in-suite parallel-runner-tests)

(test parallel-runner-exists
  "run-tests-parallel function exists and is callable."
  (is (fboundp 'sibyl.tests:run-tests-parallel)))

(test parallel-runner-returns-results
  "run-tests-parallel parameter variables are defined correctly."
  ;; Note: We test the variables rather than calling run-tests-parallel
  ;; to avoid re-entering the full suite recursively.
  (is (not (null (boundp 'sibyl.tests::*safe-suites*))))
  (is (not (null sibyl.tests::*safe-suites*)))
  (is (> (length sibyl.tests::*safe-suites*) 0)))

(test test-parallel-command-registered
  "/test-parallel command is registered in *repl-commands*."
  (is (not (null (assoc "/test-parallel" sibyl.repl::*repl-commands* :test #'string=)))))

(test parallel-safe-suites-defined
  "The *safe-suites* list contains expected suites."
  ;; Note: these suites live in cross-package namespaces (sibyl.evolution.tests,
  ;; sibyl.agent.tests) — check with the correct package-qualified symbols.
  (is (member 'sibyl.tests::core-tests sibyl.tests::*safe-suites*))
  (is (member 'sibyl.tests::message-tests sibyl.tests::*safe-suites*))
  ;; Cross-package suites are resolved at runtime, not in *safe-suites* directly
  (is (member 'sibyl.evolution.tests::evolution-state-tests (sibyl.tests::%safe-suites-resolved)))
  (is (member 'sibyl.evolution.tests::evolution-report-tests (sibyl.tests::%safe-suites-resolved)))
  (is (member 'sibyl.tests::read-sexp-tests sibyl.tests::*safe-suites*)))

(test parallel-unsafe-suites-defined
  "The *unsafe-suites* list contains expected suites."
  (is (member 'sibyl.tests::planning-tests sibyl.tests::*unsafe-suites*))
  (is (member 'sibyl.tests::self-assess-tests sibyl.tests::*unsafe-suites*))
  (is (member 'sibyl.tests::write-test-tests sibyl.tests::*unsafe-suites*)))
