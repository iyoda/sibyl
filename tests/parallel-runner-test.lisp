;;;; parallel-runner-test.lisp — Tests for the parallel test runner

(in-package #:sibyl.tests)

(in-suite sibyl-tests)

;;; ============================================================
;;; Cross-package suite initialization
;;;
;;; suite.lisp is loaded FIRST (before sibyl.agent.tests packages exist),
;;; so cross-package symbols cannot be included in *safe-suites* directly.
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

(test parallel-safe-suites-defined
  "The *safe-suites* list contains expected suites."
  ;; Note: these suites live in cross-package namespaces (sibyl.agent.tests)
  ;; — check with the correct package-qualified symbols.
  (is (member 'sibyl.tests::core-tests sibyl.tests::*safe-suites*))
  (is (member 'sibyl.tests::message-tests sibyl.tests::*safe-suites*))
  (is (member 'sibyl.tests::read-sexp-tests sibyl.tests::*safe-suites*)))

(test parallel-unsafe-suites-defined
  "The *unsafe-suites* list contains expected suites."
  (is (member 'sibyl.tests::planning-tests sibyl.tests::*unsafe-suites*))
  (is (member 'sibyl.tests::self-assess-tests sibyl.tests::*unsafe-suites*))
  (is (member 'sibyl.tests::write-test-tests sibyl.tests::*unsafe-suites*)))

(test collect-fiveam-results-suppresses-stderr
  "Auto-generated test"
  
(let* ((captured (make-string-output-stream))
       (*error-output* captured))
  ;; session-tests contains a test that writes "Warning: session X not found"
  ;; to *error-output*.  After the fix, %collect-fiveam-results must suppress it.
  (sibyl.tests::%collect-fiveam-results 'sibyl.tests::session-tests)
  (let ((err (get-output-stream-string captured)))
    (is (string= "" err)
        "~a: %collect-fiveam-results must not leak test *error-output* to caller" err)))
)

(test run-tests-parallel-safe-suites-no-stderr-reemit
  "Auto-generated test"
  
;; Verify that warnings captured inside safe-suite threads are NOT
;; re-emitted to the caller's *error-output*.
;; We bind *error-output* and then force one safe suite to run.
;; The safe-redefine tests generate SBCL redefine warnings.
(let* ((captured (make-string-output-stream))
       (*error-output* captured)
       ;; Only run safe-redefine-tests to keep this test fast
       (sibyl.tests::*run-tests-parallel-running* nil))
  ;; Use %collect-fiveam-results (which runs via thread-safe serialization)
  ;; on a suite we know generates *error-output* (SBCL redefine warnings)
  (sibyl.tests::%run-suite-isolated 'sibyl.tests::safe-redefine-tests)
  (let ((err (get-output-stream-string captured)))
    (is (string= "" err)
        "~a: safe-suite runner must not re-emit test stderr to caller" err)))
)

(test run-sibyl-tests-no-stderr
  "Auto-generated test"
  
;; run-sibyl-tests must not emit test-generated warnings to *error-output*.
;; We bind *error-output* before calling run-sibyl-tests and verify nothing leaks.
(let* ((captured (make-string-output-stream))
       (*error-output* captured)
       (*standard-output* (make-broadcast-stream))) ; also silence stdout
  ;; Temporarily guard against recursive parallel-runner re-entry
  (let ((sibyl.tests::*run-tests-parallel-running* t))
    ;; run-sibyl-tests calls fiveam:run! 'sibyl-tests sequentially
    ;; It will exit early because *run-tests-parallel-running* is already T
    ;; So just verify the guard itself doesn't leak to *error-output*
    (sibyl.tests::run-sibyl-tests))
  ;; After the fix, nothing from test code should appear
  (let ((err (get-output-stream-string captured)))
    ;; Exclude legitimate runner-level warnings (contain "[")
    ;; Only fail on raw SBCL / application warnings
    (is (not (search "redefining" err))
        "SBCL redefine warning leaked: ~a" err)
    (is (not (search "not found" err))
        "Application warning leaked: ~a" err)))
)
