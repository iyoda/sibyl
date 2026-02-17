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

;;; Helper to resolve a suite symbol at runtime.
;;; FiveAM uses the full package-qualified symbol for suite lookup,
;;; so cross-package suites must be resolved from their defining package.
;;; Lock order: tool-registry (1) -> evolution-state (2) -> modified-files (3) -> command-handlers (4)
(defun %resolve-suite (name package)
  "Return the suite symbol for NAME in PACKAGE (a package designator), or NIL if not found."
  (let ((pkg (find-package package)))
    (when pkg (find-symbol (symbol-name name) pkg))))

(defparameter *safe-suites*
  ;; Suites defined in sibyl.tests package (same package as this file)
  '(read-sexp-tests
    describe-symbol-tests
    macroexpand-form-tests
    package-symbols-tests
    codebase-map-tests
    sync-to-file-tests
    evolve-tests)
  "Test suites in sibyl.tests package safe for parallel execution.
Cross-package suites are resolved at runtime via %safe-suites-resolved.")

(defun %safe-suites-resolved ()
  "Return the full list of SAFE suites with cross-package symbols resolved at runtime.
Called at run-tests-parallel invocation time, after all packages are loaded."
  (append
   *safe-suites*
   (remove nil
           (list
            (%resolve-suite 'agent-tests             '#:sibyl.agent.tests)
            (%resolve-suite 'tdd-orchestration-tests '#:sibyl.agent.tests)
            (%resolve-suite 'run-hook-tests          '#:sibyl.agent.tests)
             (%resolve-suite 'memory-compact-tests    '#:sibyl.agent.tests)
             (%resolve-suite 'evolution-state-tests   '#:sibyl.evolution.tests)
             (%resolve-suite 'evolution-report-tests  '#:sibyl.evolution.tests)))))

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
    who-calls-tests
    run-tests-tests
    add-definition-tests
    add-export-tests
    create-module-tests
    parallel-runner-tests)
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
  3. Full sibyl-tests suite: run to capture top-level tests not in any named sub-suite
     (e.g. sanity-check, tool-registration from tools-test.lisp). The full suite result
     is used as the authoritative result set when it has >= checks than named sub-suites.

Uses with-codebase-map-cache and *self-assess-running* guard for optimal performance.

Returns a list of all test results (same format as fiveam:run)."
  (sibyl.tools:with-codebase-map-cache ()
    (let* ((sibyl.tools:*self-assess-running* t)
           ;; Resolve cross-package suite symbols at runtime
           (safe-suites (%safe-suites-resolved))
           ;; Parallel phase: run SAFE suites in threads
           (n-safe (length safe-suites))
           (safe-results (make-array n-safe :initial-element nil))
           (threads
             (loop for suite in safe-suites
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

        ;; Full suite phase: run sibyl-tests to capture top-level tests not in named sub-suites.
        ;; fiveam:run 'sibyl-tests recurses into all sub-suites, so its result count >= named suites.
        ;; We use the full suite results as the authoritative set.
        (let* ((full-results (%collect-fiveam-results 'sibyl-tests))
               (named-count (+ (loop for i from 0 below n-safe
                                     sum (length (or (aref safe-results i) nil)))
                               (length unsafe-results)))
               ;; Use full results (which include everything) as the final answer
               (final-results (if (>= (length full-results) named-count)
                                  full-results
                                  (append (loop for i from 0 below n-safe
                                                nconc (or (aref safe-results i) nil))
                                          unsafe-results))))

          ;; Print summary (like fiveam:run! does)
          ;; Uses find-symbol+find-class to avoid SBCL package-lock on IT.BESE.FIVEAM
          (let* ((fiveam-pkg (find-package "IT.BESE.FIVEAM"))
                 (passed-class  (find-class (find-symbol "TEST-PASSED"             fiveam-pkg) nil))
                 (failed-class  (find-class (find-symbol "TEST-FAILED"             fiveam-pkg) nil))
                 (skipped-class (find-class (find-symbol "TEST-SKIPPED"            fiveam-pkg) nil))
                 (error-class   (find-class (find-symbol "UNEXPECTED-TEST-FAILURE" fiveam-pkg) nil))
                 (total (length final-results))
                 (pass (count-if (lambda (r) (and passed-class  (typep r passed-class)))  final-results))
                 (fail (count-if (lambda (r) (and failed-class  (typep r failed-class)))  final-results))
                 (skip (count-if (lambda (r) (and skipped-class (typep r skipped-class))) final-results))
                 (err  (count-if (lambda (r) (and error-class   (typep r error-class)))   final-results)))
            (format t "~% Did ~a checks.~%" total)
            (format t "    Pass: ~a (~a%)~%" pass (if (> total 0) (round (* 100 (/ pass total))) 0))
            (format t "    Skip: ~a~%" skip)
            (format t "    Fail: ~a~%" (+ fail err)))

          final-results)))))
