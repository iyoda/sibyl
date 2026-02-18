;;;; suite.lisp — Test suite definition for Sibyl

(defpackage #:sibyl.tests
  (:use #:cl #:fiveam)
  (:export #:sibyl-tests #:run-sibyl-tests #:run-tests-parallel))

(in-package #:sibyl.tests)

(def-suite sibyl-tests
  :description "Top-level test suite for Sibyl.")


(def-suite core-tests
  :description "Core sanity tests for the test harness."
  :in sibyl-tests)

(in-suite core-tests)

;;; Placeholder test to verify the suite loads
(test sanity-check
  "Basic sanity check."
  (is (= 1 1))
  (is (string= "sibyl" (string-downcase "SIBYL"))))

(in-suite sibyl-tests)

;;; ============================================================
;;; Suite classification for parallel execution
;;; ============================================================

;;; LLM GUIDANCE:
;;; - Always define a per-file test suite and add it to *safe-suites* or *unsafe-suites*.
;;; - SAFE: pure logic, no file I/O, no global state mutation, no external processes.
;;; - UNSAFE: file I/O, global registries/state, random timing, network, or tool execution.
;;; - If you add a new suite and forget to classify it, run-tests-parallel will miss it.

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
  '(core-tests
    tools-tests
    message-tests
    client-tests
    repl-tests
    read-sexp-tests
    describe-symbol-tests
    macroexpand-form-tests
    package-symbols-tests
    codebase-map-tests
    sync-to-file-tests
    provisioning-tests
    evolve-tests
    ollama-tests)
  "Test suites in sibyl.tests package safe for parallel execution.
Cross-package suites are resolved at runtime via %safe-suites-resolved.")

(defun %safe-suites-resolved ()
  "Return the full list of SAFE suites with cross-package symbols resolved at runtime.
Called at run-tests-parallel invocation time, after all packages are loaded."
  (remove-duplicates
   (append
    *safe-suites*
    (remove nil
            (list
             (%resolve-suite 'agent-tests             '#:sibyl.agent.tests)
             (%resolve-suite 'tdd-orchestration-tests '#:sibyl.agent.tests)
             (%resolve-suite 'run-hook-tests          '#:sibyl.agent.tests)
              (%resolve-suite 'memory-compact-tests    '#:sibyl.agent.tests)
              (%resolve-suite 'memory-sanitize-tests   '#:sibyl.agent.tests)
              (%resolve-suite 'evolution-state-tests   '#:sibyl.evolution.tests)
              (%resolve-suite 'evolution-report-tests  '#:sibyl.evolution.tests))))))

(defparameter *unsafe-suites*
  '(planning-tests
    suggest-improvements-tests
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

;;; Recursion guard — prevents tests from re-entering run-tests-parallel
(defvar *run-tests-parallel-running* nil
  "T while run-tests-parallel is executing. Prevents recursive re-entry.")

(defun run-sibyl-tests ()
  "Run the full test suite with self-assess nested execution prevention
and codebase-map caching for faster repeated scans."
  (sibyl.tools:with-codebase-map-cache ()
    (let ((sibyl.tools:*self-assess-running* t)
          (*run-tests-parallel-running* t))
      (fiveam:run! 'sibyl-tests))))

(defun %collect-fiveam-results (suite)
  "Run SUITE collecting results with thread-safe serialization.
    Uses a lock to prevent concurrent FiveAM global state mutation."
  (bt:with-lock-held (*fiveam-run-lock*)
    (fiveam:run suite)))

(defun %time-call (thunk)
  "Call THUNK, returning (values result elapsed-seconds).
THUNK is a zero-argument function. elapsed-seconds is a float."
  (let ((start (get-internal-real-time)))
    (let ((result (funcall thunk)))
      (values result
              (/ (float (- (get-internal-real-time) start))
                 internal-time-units-per-second)))))

(defun %validate-suites (suites label)
  "Check SUITES against FiveAM registry. Returns (values valid-suites skipped-names).
LABEL is a string like \"SAFE\" or \"UNSAFE\" for warning messages."
  (let (valid skipped)
    (dolist (suite suites)
      (if (fiveam:get-test suite)
          (push suite valid)
          (progn
            (format *error-output* "~%[parallel-runner] WARNING: ~a suite ~a not found in FiveAM registry, skipping~%"
                    label suite)
            (push suite skipped))))
    (values (nreverse valid) (nreverse skipped))))

(defun run-tests-parallel ()
  "Run the full test suite with parallel execution of safe suites.

Execution strategy:
  1. SAFE suites: run in parallel threads (serialized via lock due to FiveAM not being thread-safe)
  2. UNSAFE suites: run sequentially (shared state, file I/O)

Uses with-codebase-map-cache and *self-assess-running* guard for optimal performance.

Returns a list of all test results (same format as fiveam:run)."
  (when *run-tests-parallel-running*
    (format *error-output* "~%[parallel-runner] WARNING: recursive re-entry detected, skipping~%")
    (return-from run-tests-parallel nil))
  (let ((*run-tests-parallel-running* t))
  (sibyl.tools:with-codebase-map-cache ()
    (let* ((sibyl.tools:*self-assess-running* t)
           ;; Resolve cross-package suite symbols at runtime
           (safe-suites (nth-value 0 (%validate-suites (%safe-suites-resolved) "SAFE")))
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
                            (multiple-value-bind (results elapsed)
                                (%time-call (lambda () (%collect-fiveam-results suite-capture)))
                              (setf (aref safe-results i-capture) (cons results elapsed)))
                          (error (e)
                            (format *error-output* "~%[parallel-runner] Error in ~a: ~a~%"
                                    suite-capture e)
                            (setf (aref safe-results i-capture) (cons nil 0.0)))))
                      :name (format nil "test-~a" suite-capture))))))

       ;; Wait for all parallel threads to complete
       (dolist (thread threads)
         (bt:join-thread thread))

       ;; Sequential phase: run UNSAFE suites
       (let* ((unsafe-suites (nth-value 0 (%validate-suites *unsafe-suites* "UNSAFE")))
              ;; Collect (results . elapsed) cons for each unsafe suite
              (unsafe-timed
                (loop for suite in unsafe-suites
                      collect
                      (handler-case
                          (multiple-value-bind (results elapsed)
                              (%time-call (lambda () (%collect-fiveam-results suite)))
                            (cons results elapsed))
                        (error (e)
                          (format *error-output* "~%[parallel-runner] Error in ~a: ~a~%"
                                  suite e)
                          (cons nil 0.0)))))
              (unsafe-results (loop for cell in unsafe-timed nconc (car cell)))
              (final-results (append (loop for i from 0 below n-safe
                                           nconc (car (aref safe-results i)))
                                     unsafe-results)))

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
               (err  (count-if (lambda (r) (and error-class   (typep r error-class)))   final-results))
               ;; Wall-clock totals
               (safe-wall   (loop for i from 0 below n-safe
                                  sum (or (cdr (aref safe-results i)) 0.0)))
               (unsafe-wall (loop for cell in unsafe-timed sum (or (cdr cell) 0.0)))
               (total-wall  (+ safe-wall unsafe-wall)))
          (format t "~% Did ~a checks.~%" total)
          (format t "    Pass: ~a (~a%)~%" pass (if (> total 0) (round (* 100 (/ pass total))) 0))
          (format t "    Skip: ~a~%" skip)
          (format t "    Fail: ~a~%" (+ fail err))
          ;; Per-suite timing: safe
          (format t "~% Per-suite timing (safe):~%")
          (loop for suite in safe-suites
                for i from 0
                do (let* ((cell    (aref safe-results i))
                          (results (car cell))
                          (elapsed (or (cdr cell) 0.0))
                          (checks  (length results)))
                     (format t "  ~a~30t~6,3fs (~a checks)~%"
                             suite elapsed checks)))
          ;; Per-suite timing: unsafe
          (format t "~% Per-suite timing (unsafe):~%")
          (loop for suite in unsafe-suites
                for cell in unsafe-timed
                do (let* ((results (car cell))
                          (elapsed (or (cdr cell) 0.0))
                          (checks  (length results)))
                     (format t "  ~a~30t~6,3fs (~a checks)~%"
                             suite elapsed checks)))
          ;; Total wall-clock
          (format t "~% Total wall-clock: ~6,3fs (safe: ~6,3fs, unsafe: ~6,3fs)~%"
                  total-wall safe-wall unsafe-wall))

        final-results)))))
