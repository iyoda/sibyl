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
;;; Lock order: tool-registry (1) -> modified-files (2) -> command-handlers (3)
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
    ollama-tests
    cache-key-tests
    cache-adapter-tests
    ;; Task 1: Newly classified safe suites (pure logic, no I/O, no global state)
    tokens-tests                    ;; Token tracking: pure logic, no I/O, no sleep
    token-tracking-suite            ;; Token usage tracking: pure logic, hash table operations
    memory-compaction-suite         ;; Memory compaction: pure logic, no I/O, no sleep
    model-registry-tests            ;; Model registry and pricing: pure logic, no I/O
    mcp-tests                       ;; MCP protocol: JSON-RPC, schema conversion (pure logic, no I/O)
    openai-usage-tests              ;; OpenAI usage extraction: pure logic, no I/O
    openai-pricing-tests            ;; OpenAI pricing table: pure logic, no I/O
    openai-streaming-usage-tests    ;; OpenAI streaming usage extraction: pure logic, no I/O
    openai-cache-telemetry-tests    ;; OpenAI cache telemetry integration: pure logic, no I/O
    openai-cache-integration-tests  ;; OpenAI cache integration with usage tracking: pure logic, no I/O
    anthropic-thinking-config-tests ;; Anthropic thinking config and capabilities: pure logic, no I/O
    anthropic-streaming-cache-tests ;; Anthropic streaming cache token extraction: pure logic, no I/O
    anthropic-thinking-serialization-tests ;; Anthropic thinking block serialization: pure logic, no I/O
    anthropic-thinking-params-tests ;; Anthropic thinking params helper: pure logic, no I/O
    repl-display-tests              ;; REPL display formatting: pure logic, no I/O, no global state
    turn-footer-tests               ;; Turn footer display: pure logic, no I/O, no global state
    session-summary-tests           ;; Session summary display: pure logic, no I/O, no global state
    interrupt-handler-tests         ;; Ctrl+C interrupt handler decision logic: pure logic, no I/O
    logging-tests)                  ;; Logging context propagation + component filtering: pure logic, no I/O
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
               (%resolve-suite 'token-tracker-enhancement-tests '#:sibyl.llm))))))

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
    parallel-runner-tests
    cache-lru-tests
    cache-telemetry-tests
    cache-integration-tests
    ;; Task 1: Newly classified unsafe suites (sleep, threads, timing-sensitive)
    rich-repl-tests               ;; REPL features: spinner threads, sleep calls
    session-tests)                ;; Session persistence: file I/O, timer threads
  "Test suites in sibyl.tests package that must run sequentially (file I/O, global state, or FiveAM side effects).
Cross-package suites are resolved at runtime via %unsafe-suites-resolved.")

(defun %unsafe-suites-resolved ()
  "Return the full list of UNSAFE suites with cross-package symbols resolved at runtime.
Called at run-tests-parallel invocation time, after all packages are loaded."
  (remove-duplicates
   (append
    *unsafe-suites*
    (remove nil
             (list
              ;; Task 1: Newly classified cross-package unsafe suites (sleep, threads)
              (%resolve-suite 'parallel-tool-execution-tests '#:sibyl.agent.tests)  ;; Parallel tool execution: sleep calls (lines 440, 441, 460)
              (%resolve-suite 'parallel-agent-tests         '#:sibyl.parallel.tests)  ;; Parallel agent tasks: sleep calls, threads
              (%resolve-suite 'tool-timing-tests            '#:sibyl.tests.tool-timing))))))  ;; Tool timing: sleep calls for timing verification

;;; Lock for serializing FiveAM calls (FiveAM is not thread-safe)
(defvar *fiveam-run-lock* (bt:make-lock "fiveam-run-lock")
  "Lock to serialize FiveAM run calls across threads.")

;;; Recursion guard — prevents tests from re-entering run-tests-parallel
(defvar *run-tests-parallel-running* nil
  "T while run-tests-parallel is executing. Prevents recursive re-entry.")

(defun run-sibyl-tests ()
  "Run the full test suite with self-assess nested execution prevention
and codebase-map caching for faster repeated scans."
  (sibyl.tools:with-codebase-map-cache nil
    (let ((sibyl.tools:*self-assess-running* t)
          (*run-tests-parallel-running* t)
          (it.bese.fiveam:*test-dribble* (make-broadcast-stream)))
      (it.bese.fiveam:run! 'sibyl-tests))))

(defun %reset-suite-statuses (suite)
  "Reset all test statuses in SUITE to :unknown for re-execution."
  (let* ((fiveam-pkg (find-package "IT.BESE.FIVEAM"))
         (get-test-fn (find-symbol "GET-TEST" fiveam-pkg))
         (suite-obj (funcall get-test-fn suite)))
    (when suite-obj
      (labels ((safe-slot-value (obj slot)
                 (when slot (ignore-errors (slot-value obj slot))))
               (bundle-tests (bundle)
                  (let ((bundle-tests-slot (find-symbol "TESTS" fiveam-pkg)))
                    (safe-slot-value bundle bundle-tests-slot)))
               (suite-tests (suite-object)
                 (let* ((tests-slot (find-symbol "TESTS" fiveam-pkg))
                        (tests-bundle (safe-slot-value suite-object tests-slot)))
                   (cond
                     ((hash-table-p tests-bundle) tests-bundle)
                     (tests-bundle (bundle-tests tests-bundle))
                     (t nil))))
               (reset-testable (test-obj)
                  (let* ((status-slot (find-symbol "STATUS" fiveam-pkg))
                         (suite-class (find-class (find-symbol "TEST-SUITE" fiveam-pkg) nil))
                         (resolved (cond
                                     ((symbolp test-obj) (funcall get-test-fn test-obj))
                                     (t test-obj))))
                    (when resolved
                      (setf (slot-value resolved status-slot) :unknown)
                      (when (and suite-class (typep resolved suite-class))
                        (let ((tests (suite-tests resolved)))
                          (when tests
                            (maphash (lambda (name nested-obj)
                                       (declare (ignore name))
                                       (reset-testable nested-obj))
                                     tests))))))))
        (reset-testable suite-obj)))))

(defun %reset-all-statuses ()
  "Reset all test statuses in the FiveAM registry to :unknown."
  (let* ((fiveam-pkg (find-package "IT.BESE.FIVEAM"))
         (test-names-fn (find-symbol "TEST-NAMES" fiveam-pkg))
         (get-test-fn (find-symbol "GET-TEST" fiveam-pkg))
         (status-slot (find-symbol "STATUS" fiveam-pkg)))
    (when (and test-names-fn get-test-fn status-slot)
      (dolist (test (funcall test-names-fn))
        (let ((obj (funcall get-test-fn test)))
          (when obj
            (ignore-errors (setf (slot-value obj status-slot) :unknown))))))))

(defvar *fiveam-reset-enabled* t
  "When T, reset suite statuses before running a suite in the parallel runner.")

(defun %run-suite-isolated (suite)
  "Run SUITE in isolation with thread-local FiveAM globals.
Uses *fiveam-reset-enabled* to control status reset before execution."
  (let* ((fiveam-pkg (find-package "IT.BESE.FIVEAM"))
         (test-dribble-sym (find-symbol "*TEST-DRIBBLE*" fiveam-pkg))
         (test-dribble-indent-sym (find-symbol "*TEST-DRIBBLE-INDENT*" fiveam-pkg))
         (print-names-sym (find-symbol "*PRINT-NAMES*" fiveam-pkg))
         (return-result-list-fn (find-symbol "RETURN-RESULT-LIST" fiveam-pkg))
         (%run-fn (find-symbol "%RUN" fiveam-pkg)))
    (progv (list test-dribble-sym test-dribble-indent-sym print-names-sym)
           (list (make-broadcast-stream)
                 (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)
                 nil)
      (when *fiveam-reset-enabled*
        (%reset-suite-statuses suite))
      (funcall return-result-list-fn
               (lambda () (funcall %run-fn suite))))))

(defun %collect-fiveam-results (suite)
  "Run SUITE collecting results with thread-safe serialization.
    Uses a lock to prevent concurrent FiveAM global state mutation."
  (bordeaux-threads:with-lock-held (*fiveam-run-lock*)
    (let* ((fiveam-pkg (find-package "IT.BESE.FIVEAM"))
           (test-dribble-sym (find-symbol "*TEST-DRIBBLE*" fiveam-pkg))
           (test-dribble-indent-sym (find-symbol "*TEST-DRIBBLE-INDENT*" fiveam-pkg))
           (print-names-sym (find-symbol "*PRINT-NAMES*" fiveam-pkg))
           (return-result-list-fn (find-symbol "RETURN-RESULT-LIST" fiveam-pkg))
           (%run-fn (find-symbol "%RUN" fiveam-pkg)))
      (progv (list test-dribble-sym test-dribble-indent-sym print-names-sym)
             (list (make-broadcast-stream)
                   (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)
                   nil)
        (when *fiveam-reset-enabled*
          (%reset-suite-statuses suite))
        (funcall return-result-list-fn
                 (lambda () (funcall %run-fn suite)))))))

(defun %suite-run-order (suite)
  "Return the immediate test/suite names for SUITE, preserving duplicates." 
  (let* ((fiveam-pkg (find-package "IT.BESE.FIVEAM"))
         (get-test-fn (find-symbol "GET-TEST" fiveam-pkg))
         (suite-obj (and get-test-fn (funcall get-test-fn suite))))
    (when suite-obj
      (labels ((safe-slot-value (obj slot)
                 (when slot (ignore-errors (slot-value obj slot)))))
        (let* ((tests-slot (find-symbol "TESTS" fiveam-pkg))
               (names-slot (find-symbol "NAMES" fiveam-pkg))
               (tests-bundle (safe-slot-value suite-obj tests-slot))
               (names (safe-slot-value tests-bundle names-slot)))
          (when names
            (reverse (copy-list names))))))))

(defun %dedupe-suites (suites)
  "Return SUITES with duplicates removed, preserving order."
  (let ((seen (make-hash-table :test 'eq))
        (result nil))
    (dolist (suite suites (nreverse result))
      (unless (gethash suite seen)
        (setf (gethash suite seen) t)
        (push suite result)))))

(defun %suite-test-cases (suite)
  "Return a list of test-case symbols reachable from SUITE."
  (let* ((fiveam-pkg (find-package "IT.BESE.FIVEAM"))
         (get-test-fn (find-symbol "GET-TEST" fiveam-pkg))
         (suite-class (find-class (find-symbol "TEST-SUITE" fiveam-pkg) nil))
         (test-case-class (find-class (find-symbol "TEST-CASE" fiveam-pkg) nil))
         (name-slot (find-symbol "NAME" fiveam-pkg))
         (tests-slot (find-symbol "TESTS" fiveam-pkg))
         (bundle-tests-slot (find-symbol "TESTS" fiveam-pkg))
         (acc (make-hash-table :test 'eq)))
    (labels ((safe-slot-value (obj slot)
               (when slot (ignore-errors (slot-value obj slot))))
             (bundle-tests (bundle)
               (safe-slot-value bundle bundle-tests-slot))
             (suite-tests (suite-object)
               (let ((tests-bundle (safe-slot-value suite-object tests-slot)))
                 (cond
                   ((hash-table-p tests-bundle) tests-bundle)
                   (tests-bundle (bundle-tests tests-bundle))
                   (t nil))))
             (add-test (test-obj)
               (let ((resolved (cond
                                 ((symbolp test-obj)
                                  (and get-test-fn (funcall get-test-fn test-obj)))
                                 (t test-obj))))
                 (when resolved
                   (cond
                     ((and test-case-class (typep resolved test-case-class))
                      (let ((name (safe-slot-value resolved name-slot)))
                        (when name
                          (setf (gethash name acc) t))))
                     ((and suite-class (typep resolved suite-class))
                      (let ((tests (suite-tests resolved)))
                        (when tests
                          (maphash (lambda (name nested-obj)
                                     (declare (ignore name))
                                     (add-test nested-obj))
                                   tests)))))))))
      (when (and get-test-fn suite)
        (add-test (funcall get-test-fn suite))))
    (loop for key being the hash-keys of acc collect key)))

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

(defun %record-timing-history (safe-suites safe-results unsafe-suites unsafe-timed
                               total-wall safe-wall unsafe-wall)
  "Record per-suite timing data to tests/timing-history.json."
  (let* ((history-path (merge-pathnames "tests/timing-history.json"
                                        (asdf:system-source-directory :sibyl)))
         (timestamp (local-time:format-timestring nil (local-time:now)
                                                  :format local-time:+iso-8601-format+))
         (suites-data (make-hash-table :test 'equal)))
    ;; Collect safe suite timings
    (loop for suite in safe-suites
          for i from 0
          do (let* ((cell (aref safe-results i))
                    (results (car cell))
                    (elapsed (or (cdr cell) 0.0))
                    (checks (length results))
                    (suite-ht (make-hash-table :test 'equal)))
               (setf (gethash "elapsed" suite-ht) elapsed)
               (setf (gethash "checks" suite-ht) checks)
               (setf (gethash (string-downcase (symbol-name suite)) suites-data) suite-ht)))
    ;; Collect unsafe suite timings
    (loop for suite in unsafe-suites
          for cell in unsafe-timed
          do (let* ((results (car cell))
                    (elapsed (or (cdr cell) 0.0))
                    (checks (length results))
                    (suite-ht (make-hash-table :test 'equal)))
               (setf (gethash "elapsed" suite-ht) elapsed)
               (setf (gethash "checks" suite-ht) checks)
               (setf (gethash (string-downcase (symbol-name suite)) suites-data) suite-ht)))
    ;; Build new entry (use hash table for yason compatibility)
    (let ((new-entry (let ((ht (make-hash-table :test 'equal)))
                       (setf (gethash "timestamp" ht) timestamp)
                       (setf (gethash "total-wall" ht) total-wall)
                       (setf (gethash "safe-wall" ht) safe-wall)
                       (setf (gethash "unsafe-wall" ht) unsafe-wall)
                       (setf (gethash "suites" ht) suites-data)
                       ht)))
      ;; Read existing history
      (let ((history (if (probe-file history-path)
                         (handler-case
                              (with-open-file (stream history-path :direction :input)
                                (let ((parsed (yason:parse stream)))
                                  (if (vectorp parsed)
                                      (coerce parsed 'list)
                                      parsed)))
                           (error (e)
                             (format *error-output* "~%[timing-history] WARNING: Failed to parse ~a: ~a~%"
                                     history-path e)
                             (format *error-output* "  Starting fresh history~%")
                             nil))
                         nil)))
        ;; Append new entry
        (setf history (append history (list new-entry)))
        ;; Keep only last 50 entries
        (when (> (length history) 50)
          (setf history (subseq history (- (length history) 50))))
        ;; Write back
        (with-open-file (stream history-path :direction :output :if-exists :supersede)
          (yason:encode (coerce history 'vector) stream))))))

(defun %detect-regression ()
  "Detect timing regressions by comparing against recent history."
  (let* ((history-path (merge-pathnames "tests/timing-history.json"
                                        (asdf:system-source-directory :sibyl))))
    (when (probe-file history-path)
      (handler-case
           (let* ((parsed (with-open-file (stream history-path :direction :input)
                           (yason:parse stream)))
                  (history (if (vectorp parsed) (coerce parsed 'list) parsed)))
            (when (and history (>= (length history) 5))
              (let* ((recent (subseq history (max 0 (- (length history) 6)) (1- (length history))))
                     (current (nth (1- (length history)) history))
                     (current-total (gethash "total-wall" current))
                     (recent-totals (mapcar (lambda (e) (gethash "total-wall" e)) recent))
                     (median-total (nth (floor (length recent-totals) 2)
                                       (sort (copy-seq recent-totals) #'<))))
                ;; Check total wall-clock regression
                (when (> current-total (* 1.5 median-total))
                  (format *error-output* "~%[timing-regression] WARNING: Total wall-clock regression detected!~%")
                  (format *error-output* "  Current: ~6,3fs, Median of last 5: ~6,3fs (threshold: ~6,3fs)~%"
                          current-total median-total (* 1.5 median-total)))
                ;; Check per-suite regressions
                (let ((current-suites (gethash "suites" current)))
                  (maphash (lambda (suite-name suite-data)
                             (let* ((current-elapsed (gethash "elapsed" suite-data))
                                    (recent-elapsed (remove nil
                                                           (mapcar
                                                                (lambda (e)
                                                                  (let ((s (gethash "suites" e)))
                                                                    (when s
                                                                      (let ((suite-ht (gethash suite-name s)))
                                                                        (when suite-ht
                                                                          (gethash "elapsed" suite-ht))))))
                                                                recent))))
                               (when (>= (length recent-elapsed) 3)
                                 (let ((median-elapsed (nth (floor (length recent-elapsed) 2)
                                                           (sort (copy-seq recent-elapsed) #'<))))
                                   (when (and (> median-elapsed 0.01)  ; Skip very fast suites
                                              (> current-elapsed (* 2.0 median-elapsed)))
                                     (format *error-output* "~%[timing-regression] WARNING: Suite ~a regression detected!~%"
                                             suite-name)
                                     (format *error-output* "  Current: ~6,3fs, Median: ~6,3fs (threshold: ~6,3fs)~%"
                                             current-elapsed median-elapsed (* 2.0 median-elapsed)))))))
                           current-suites)))))
        (error (e)
          (format *error-output* "~%[timing-regression] WARNING: Failed to detect regressions: ~a~%" e))))))

(defun %validate-suite-classification ()
   "Warn about unclassified test suites in FiveAM registry.
  Enumerates all suites in the FiveAM registry and warns about any that are not
  in *safe-suites* or *unsafe-suites*, excluding the parent suite 'sibyl-tests'.
  Also warns if any SAFE suite has cross-suite depends-on relationships."
   (let* ((fiveam-pkg (find-package "IT.BESE.FIVEAM"))
          (*test*-sym (find-symbol "*TEST*" fiveam-pkg))
          (test-suite-class (find-class (find-symbol "TEST-SUITE" fiveam-pkg)))
          (all-safe (%safe-suites-resolved))
          (all-unsafe (%unsafe-suites-resolved))
          (all-classified (append all-safe all-unsafe))
         (registry-hash (slot-value (symbol-value *test*-sym)
                                     (find-symbol "TESTS" fiveam-pkg)))
         (unclassified nil)
         (cross-suite-deps nil))
     ;; Enumerate all suites in the FiveAM registry
     (maphash (lambda (name obj)
                (when (typep obj test-suite-class)
                  ;; Skip parent suite and NIL
                  (unless (or (eq name 'sibyl-tests) (null name))
                    ;; Check if unclassified
                    (unless (member name all-classified)
                      (push name unclassified))
                     ;; Check for cross-suite depends-on in SAFE suites only
                     (when (member name all-safe)
                       (let ((depends-on (slot-value obj (find-symbol "DEPENDS-ON" fiveam-pkg))))
                         (when depends-on
                           (dolist (dep depends-on)
                             (unless (eq name dep)
                               (push (cons name dep) cross-suite-deps)))))))))
               registry-hash)
     ;; Warn about unclassified suites
     (when unclassified
       (format *error-output* "~%[parallel-runner] WARNING: Unclassified suites: ~{~a~^, ~}~%"
               (nreverse unclassified))
       (format *error-output* "  Add them to *safe-suites* or *unsafe-suites* in tests/suite.lisp~%"))
     ;; Warn about cross-suite dependencies in SAFE suites
     (when cross-suite-deps
       (format *error-output* "~%[parallel-runner] WARNING: Cross-suite dependencies in SAFE suites detected:~%")
       (dolist (dep (nreverse cross-suite-deps))
         (format *error-output* "  ~a depends-on ~a~%" (car dep) (cdr dep)))
       (format *error-output* "  Cross-suite depends-on may cause parallel execution issues~%"))))

 (defun run-tests-parallel ()
   "Run the full test suite with parallel execution of safe suites.

 Execution strategy:
    1. SAFE suites: run in parallel threads with isolated FiveAM globals (no lock)
    2. UNSAFE suites: run sequentially (shared state, file I/O)

 Uses with-codebase-map-cache and *self-assess-running* guard for optimal performance.

 Returns a list of all test results (same format as fiveam:run)."
   (when *run-tests-parallel-running*
     (format *error-output* "~%[parallel-runner] WARNING: recursive re-entry detected, skipping~%")
     (return-from run-tests-parallel nil))
   (let ((*run-tests-parallel-running* t))
     ;; Validate suite classification before running tests
     (%validate-suite-classification)
   (sibyl.tools:with-codebase-map-cache ()
    (let* ((sibyl.tools:*self-assess-running* t)
            ;; Resolve cross-package suite symbols at runtime
            (safe-suites-all (nth-value 0 (%validate-suites (%safe-suites-resolved) "SAFE")))
            (unsafe-suites-all (nth-value 0 (%validate-suites (%unsafe-suites-resolved) "UNSAFE")))
            (suite-run-order (%suite-run-order 'sibyl-tests))
            (safe-suites (%dedupe-suites
                          (if suite-run-order
                              (loop for name in suite-run-order
                                    when (member name safe-suites-all) collect name)
                              safe-suites-all)))
            (unsafe-suites (%dedupe-suites
                            (if suite-run-order
                                (loop for name in suite-run-order
                                      when (member name unsafe-suites-all) collect name)
                                unsafe-suites-all)))
            ;; Parallel phase: run first occurrences of SAFE suites in threads
            (safe-run-vector (coerce safe-suites 'vector))
            (n-safe (length safe-run-vector))
            (safe-results (make-array n-safe :initial-element nil))
            (safe-error-output (make-array n-safe :initial-element ""))
            (threads nil)
            (safe-wall-clock 0.0)
            (unsafe-wall-clock 0.0)
            (unsafe-timed nil))
        ;; Reset all test statuses once, like fiveam:run
        (%reset-all-statuses)
        (let* ((*fiveam-reset-enabled* t)
               (suite-test-cache (make-hash-table :test 'eq)))
          (labels ((suite-test-set (suite)
                     (or (gethash suite suite-test-cache)
                         (let ((set (make-hash-table :test 'eq)))
                           (dolist (name (%suite-test-cases suite))
                             (setf (gethash name set) t))
                           (setf (gethash suite suite-test-cache) set))))
                   (suite-overlaps-p (suite-set batch-set)
                     (let ((overlap nil))
                       (when suite-set
                         (maphash (lambda (name present)
                                    (declare (ignore present))
                                    (when (gethash name batch-set)
                                      (setf overlap t)))
                                  suite-set))
                       overlap))
                   (merge-suite-set (suite-set batch-set)
                     (when suite-set
                       (maphash (lambda (name present)
                                  (declare (ignore present))
                                  (setf (gethash name batch-set) t))
                                suite-set))))
            (setf safe-wall-clock
                  (nth-value
                   1
                   (%time-call
                    (lambda ()
                      (let ((i 0))
                        (loop while (< i n-safe)
                              do (let ((batch nil)
                                       (batch-set (make-hash-table :test 'eq)))
                                   (loop while (< i n-safe)
                                         for suite = (aref safe-run-vector i)
                                         for suite-set = (suite-test-set suite)
                                         do (if (and batch (suite-overlaps-p suite-set batch-set))
                                                (return)
                                                (progn
                                                  (push i batch)
                                                  (merge-suite-set suite-set batch-set)
                                                  (incf i))))
                                   (setf threads
                                         (loop for i in (nreverse batch)
                                               for suite = (aref safe-run-vector i)
                                               collect
                                               (let ((suite-capture suite)
                                                     (i-capture i))
                                                 (bt:make-thread
                                                  (lambda ()
                                                    (let ((error-stream (make-string-output-stream)))
                                                      (unwind-protect
                                                           (let ((*error-output* error-stream)
                                                                 (sibyl.tools:*codebase-map-cache*
                                                                   (when sibyl.tools:*codebase-map-cache*
                                                                     (make-hash-table :test 'equal))))
                                                             (handler-case
                                                                 (multiple-value-bind (results elapsed)
                                                                     (%time-call (lambda () (%run-suite-isolated suite-capture)))
                                                                   (setf (aref safe-results i-capture) (cons results elapsed)))
                                                               (error (e)
                                                                 (format *error-output* "~%[parallel-runner] Error in ~a: ~a~%"
                                                                         suite-capture e)
                                                                 (setf (aref safe-results i-capture) (cons nil 0.0)))))
                                                        (setf (aref safe-error-output i-capture)
                                                              (get-output-stream-string error-stream)))))
                                                  :name (format nil "test-~a" suite-capture))))
                                    )

                                    (dolist (thread threads)
                                      (bt:join-thread thread)))

                        (loop for i from 0 below n-safe
                              for output = (aref safe-error-output i)
                              unless (or (null output) (string= output ""))
                                do (write-string output *error-output*)
                                   (finish-output *error-output*))))))))

          ;; Sequential phase: run UNSAFE suites
          (multiple-value-bind (timed elapsed)
              (%time-call
               (lambda ()
                 ;; Collect (results . elapsed) cons for each unsafe suite
                 (loop for suite in unsafe-suites
                       collect
                       (handler-case
                           (multiple-value-bind (results elapsed)
                               (%time-call (lambda () (%collect-fiveam-results suite)))
                             (cons results elapsed))
                         (error (e)
                           (format *error-output* "~%[parallel-runner] Error in ~a: ~a~%"
                                   suite e)
                           (cons nil 0.0))))))
            (setf unsafe-timed timed)
            (setf unsafe-wall-clock elapsed)))

        (let* ((unsafe-results (loop for cell in unsafe-timed append (car cell)))
               (final-results (append (loop for i from 0 below n-safe
                                            append (car (aref safe-results i)))
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
                (safe-wall safe-wall-clock)
                (unsafe-wall unsafe-wall-clock)
                (total-wall (+ safe-wall-clock unsafe-wall-clock)))
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
                  total-wall safe-wall unsafe-wall)
          
          ;; Record timing history and detect regressions
          (handler-case
              (progn
                (%record-timing-history safe-suites safe-results unsafe-suites unsafe-timed
                                       total-wall safe-wall unsafe-wall)
                (%detect-regression))
            (error (e)
              (format *error-output* "~%[parallel-runner] WARNING: Timing history error: ~a~%" e))))

        final-results))))))
