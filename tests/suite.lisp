;;;; suite.lisp — Test suite definition for Sibyl

(defpackage #:sibyl.tests
  (:use #:cl #:fiveam)
  (:export #:sibyl-tests #:run-sibyl-tests #:run-tests-parallel))

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
  "Run the full test suite with self-assess nested execution prevention
and codebase-map caching for faster repeated scans."
  (sibyl.tools:with-codebase-map-cache ()
    (let ((sibyl.tools:*self-assess-running* t))
      (fiveam:run! 'sibyl-tests))))

;;; ============================================================
;;; Suite classification for parallel execution
;;; ============================================================

(defparameter *safe-suites*
  '(read-sexp-tests
    describe-symbol-tests
    macroexpand-form-tests
    package-symbols-tests
    codebase-map-tests
    sync-to-file-tests
    evolve-tests
    agent-tests
    tdd-orchestration-tests
    run-hook-tests
    evolution-state-tests
    evolution-report-tests)
  "Test suites safe for parallel execution (no shared file I/O, no global state mutation).")

(defparameter *unsafe-suites*
  '(suggest-improvements-tests
    self-assess-tests
    improvement-plan-tests
    suggest-improvements-enhanced-tests
    safe-redefine-tests
    write-test-tests
    creation-integration-tests
    asdf-registration-tests
    register-command-tests
    asdf-protection-tests
    eval-form-tests
    who-calls-tests)
  "Test suites that must run sequentially (file I/O, global state, or FiveAM side effects).")

;;; Lock for serializing FiveAM calls (FiveAM is not thread-safe)
(defvar *fiveam-run-lock* (bt:make-lock "fiveam-run-lock")
  "Lock to serialize FiveAM run calls across threads.")

(defun %collect-fiveam-results (suite)
  "Run SUITE collecting results with thread-safe serialization.
   Uses a lock to prevent concurrent FiveAM global state mutation."
  (bt:with-lock-held (*fiveam-run-lock*)
    (fiveam:run suite)))

(defun run-tests-parallel ()
  "Run the full test suite with parallel execution of safe suites.

Execution strategy:
  1. SAFE suites: run in parallel threads (serialized via lock due to FiveAM not being thread-safe)
  2. UNSAFE suites: run sequentially (shared state, file I/O)

Uses with-codebase-map-cache and *self-assess-running* guard for optimal performance.

Returns a list of all test results (same format as fiveam:run)."
  (sibyl.tools:with-codebase-map-cache ()
    (let* ((sibyl.tools:*self-assess-running* t)
           ;; Parallel phase: run SAFE suites in threads
           (n-safe (length *safe-suites*))
           (safe-results (make-array n-safe :initial-element nil))
           (threads
             (loop for suite in *safe-suites*
                   for i from 0
                   collect
                   (let ((suite-capture suite)
                         (i-capture i))
                     (bt:make-thread
                      (lambda ()
                        (handler-case
                            (setf (aref safe-results i-capture)
                                  (%collect-fiveam-results suite-capture))
                          (error (e)
                            (format *error-output* "~%[parallel-runner] Error in ~a: ~a~%"
                                    suite-capture e)
                            (setf (aref safe-results i-capture) nil))))
                      :name (format nil "test-~a" suite-capture))))))

      ;; Wait for all parallel threads to complete
      (dolist (thread threads)
        (bt:join-thread thread))

      ;; Sequential phase: run UNSAFE suites
      (let ((unsafe-results
              (loop for suite in *unsafe-suites*
                    nconc
                    (handler-case
                        (%collect-fiveam-results suite)
                      (error (e)
                        (format *error-output* "~%[parallel-runner] Error in ~a: ~a~%"
                                suite e)
                        nil)))))

        ;; Merge all results
        (let ((all-results
                (append (loop for i from 0 below n-safe
                              nconc (or (aref safe-results i) nil))
                        unsafe-results)))

          ;; Print summary (like fiveam:run! does)
          ;; Uses find-symbol+find-class to avoid SBCL package-lock on IT.BESE.FIVEAM
          (let* ((fiveam-pkg (find-package "IT.BESE.FIVEAM"))
                 (passed-class  (find-class (find-symbol "TEST-PASSED"             fiveam-pkg) nil))
                 (failed-class  (find-class (find-symbol "TEST-FAILED"             fiveam-pkg) nil))
                 (skipped-class (find-class (find-symbol "TEST-SKIPPED"            fiveam-pkg) nil))
                 (error-class   (find-class (find-symbol "UNEXPECTED-TEST-FAILURE" fiveam-pkg) nil))
                 (total (length all-results))
                 (pass (count-if (lambda (r) (and passed-class  (typep r passed-class)))  all-results))
                 (fail (count-if (lambda (r) (and failed-class  (typep r failed-class)))  all-results))
                 (skip (count-if (lambda (r) (and skipped-class (typep r skipped-class))) all-results))
                 (err  (count-if (lambda (r) (and error-class   (typep r error-class)))   all-results)))
            (format t "~% Did ~a checks.~%" total)
            (format t "    Pass: ~a (~a%)~%" pass (if (> total 0) (round (* 100 (/ pass total))) 0))
            (format t "    Skip: ~a~%" skip)
            (format t "    Fail: ~a~%" (+ fail err)))

          all-results)))))
