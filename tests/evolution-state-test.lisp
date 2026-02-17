;;;; evolution-state-test.lisp — Tests for *evolution-state* and evolution-status tool

(defpackage #:sibyl.evolution.tests
  (:use #:cl #:fiveam)
  (:import-from #:sibyl.tools
                #:execute-tool
                #:evolution-report-cycle-start
                #:evolution-report-improvement-start
                #:evolution-report-step
                #:evolution-report-improvement-result
                #:evolution-report-cycle-summary
                #:evolution-report-final-summary)
  (:export #:evolution-state-tests
           #:evolution-report-tests))

(in-package #:sibyl.evolution.tests)

(def-suite evolution-state-tests
  :description "Tests for *evolution-state* variable and evolution-status tool"
  :in sibyl.tests:sibyl-tests)

(in-suite evolution-state-tests)

;;; ============================================================
;;; Helper: access *evolution-state* via symbol-value
;;; ============================================================

(defun get-evolution-state ()
  "Access *evolution-state* via symbol-value to avoid undefined variable warnings."
  (let ((sym (find-symbol "*EVOLUTION-STATE*" :sibyl.tools)))
    (when (and sym (boundp sym))
      (symbol-value sym))))

(defun set-evolution-state (value)
  "Set *evolution-state* via symbol-value."
  (let ((sym (find-symbol "*EVOLUTION-STATE*" :sibyl.tools)))
    (when sym
      (setf (symbol-value sym) value))))

(defun call-evolution-state-init ()
  "Call evolution-state-init."
  (let ((fn (find-symbol "EVOLUTION-STATE-INIT" :sibyl.tools)))
    (when (and fn (fboundp fn))
      (funcall (symbol-function fn)))))

(defun call-evolution-state-record-attempt (description result)
  "Call evolution-state-record-attempt."
  (let ((fn (find-symbol "EVOLUTION-STATE-RECORD-ATTEMPT" :sibyl.tools)))
    (when (and fn (fboundp fn))
      (funcall (symbol-function fn) description result))))

(defun call-evolution-state-save (path)
  "Call evolution-state-save."
  (let ((fn (find-symbol "EVOLUTION-STATE-SAVE" :sibyl.tools)))
    (when (and fn (fboundp fn))
      (funcall (symbol-function fn) path))))

(defun call-evolution-state-load (path)
  "Call evolution-state-load."
  (let ((fn (find-symbol "EVOLUTION-STATE-LOAD" :sibyl.tools)))
    (when (and fn (fboundp fn))
      (funcall (symbol-function fn) path))))

;;; ============================================================
;;; Test 1: State initialization
;;; ============================================================

(test evolution-state-init-creates-state
  "evolution-state-init should create a hash-table with required keys."
  (call-evolution-state-init)
  (let ((state (get-evolution-state)))
    (is (not (null state))
        "*evolution-state* should be non-nil after init")
    (is (hash-table-p state)
        "*evolution-state* should be a hash-table")
    (is (= 0 (gethash "cycle-number" state 0))
        "cycle-number should be initialized to 0")
    (is (multiple-value-bind (v present) (gethash "attempted-improvements" state)
          (declare (ignore v))
          present)
        "attempted-improvements key should exist in hash-table")
    (is (multiple-value-bind (v present) (gethash "results" state)
          (declare (ignore v))
          present)
        "results key should exist in hash-table")
    (is (integerp (gethash "baseline-test-count" state 0))
        "baseline-test-count should be an integer")
    (is (integerp (gethash "baseline-tool-count" state 0))
        "baseline-tool-count should be an integer")
    (is (multiple-value-bind (v present) (gethash "modified-files" state)
          (declare (ignore v))
          present)
        "modified-files key should exist in hash-table")))

(test evolution-state-variable-is-bound
  "*evolution-state* should be a bound special variable in sibyl.tools."
  (let ((sym (find-symbol "*EVOLUTION-STATE*" :sibyl.tools)))
    (is (not (null sym))
        "*EVOLUTION-STATE* symbol should exist in sibyl.tools package")
    ;; After init, it should be bound
    (call-evolution-state-init)
    (is (boundp sym)
        "*evolution-state* should be bound after init")))

;;; ============================================================
;;; Test 2: Recording an attempt
;;; ============================================================

(test evolution-state-record-attempt-adds-to-list
  "evolution-state-record-attempt should add description to attempted-improvements."
  (call-evolution-state-init)
  (call-evolution-state-record-attempt "Add docstrings to foo" "success")
  (let* ((state (get-evolution-state))
         (attempted (gethash "attempted-improvements" state)))
    (is (not (null attempted))
        "attempted-improvements should be non-nil after recording")
    (is (member "Add docstrings to foo" attempted :test #'string=)
        "attempted-improvements should contain the recorded description")))

(test evolution-state-record-attempt-adds-to-results
  "evolution-state-record-attempt should add (description . result) to results."
  (call-evolution-state-init)
  (call-evolution-state-record-attempt "Refactor bar" "failed")
  (let* ((state (get-evolution-state))
         (results (gethash "results" state)))
    (is (not (null results))
        "results should be non-nil after recording")
    (let ((found (find "Refactor bar" results
                       :test (lambda (desc pair)
                               (string= desc (car pair))))))
      (is (not (null found))
          "results should contain a pair with the description")
      (is (string= "failed" (cdr found))
          "results pair should have the correct result"))))

(test evolution-state-record-multiple-attempts
  "Multiple calls to record-attempt should accumulate entries."
  (call-evolution-state-init)
  (call-evolution-state-record-attempt "Improvement A" "success")
  (call-evolution-state-record-attempt "Improvement B" "failed")
  (call-evolution-state-record-attempt "Improvement C" "success")
  (let* ((state (get-evolution-state))
         (attempted (gethash "attempted-improvements" state)))
    (is (>= (length attempted) 3)
        "Should have at least 3 attempted improvements")))

;;; ============================================================
;;; Test 3: JSON save/load roundtrip
;;; ============================================================

(test evolution-state-save-creates-file
  "evolution-state-save should create a JSON file at the given path."
  (call-evolution-state-init)
  (call-evolution-state-record-attempt "Test improvement" "success")
  (let ((path (uiop:tmpize-pathname
               (uiop:merge-pathnames* "evolution-test-save.json"
                                      (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (call-evolution-state-save path)
           (is (probe-file path)
               "JSON file should exist after save"))
      (when (probe-file path)
        (delete-file path)))))

(test evolution-state-save-load-roundtrip
  "evolution-state-save and evolution-state-load should roundtrip state."
  (call-evolution-state-init)
  (call-evolution-state-record-attempt "Roundtrip test" "success")
  (let ((path (uiop:tmpize-pathname
               (uiop:merge-pathnames* "evolution-test-roundtrip.json"
                                      (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           ;; Save current state
           (call-evolution-state-save path)
           ;; Reset state
           (set-evolution-state nil)
           ;; Load from file
           (call-evolution-state-load path)
           ;; Verify loaded state
           (let* ((state (get-evolution-state))
                  (attempted (when state (gethash "attempted-improvements" state))))
             (is (not (null state))
                 "State should be non-nil after load")
             (is (hash-table-p state)
                 "Loaded state should be a hash-table")
             (is (member "Roundtrip test" attempted :test #'string=)
                 "Loaded state should contain previously recorded attempt")))
      (when (probe-file path)
        (delete-file path)))))

(test evolution-state-load-nonexistent-file-graceful
  "evolution-state-load with nonexistent file should not error."
  (let ((path "/tmp/nonexistent-evolution-state-xyz-12345.json"))
    (is (not (probe-file path))
        "Test file should not exist")
    ;; Should not signal an error
    (finishes
      (call-evolution-state-load path))))

;;; ============================================================
;;; Test 4: evolution-status tool returns formatted output
;;; ============================================================

(test evolution-status-tool-exists
  "evolution-status tool should be registered."
  (let ((tool (sibyl.tools:find-tool "evolution-status")))
    (is (not (null tool))
        "evolution-status tool should be registered in the tool registry")))

(test evolution-status-tool-returns-string
  "evolution-status tool should return a string."
  (call-evolution-state-init)
  (let ((result (execute-tool "evolution-status" '())))
    (is (stringp result)
        "evolution-status should return a string")))

(test evolution-status-tool-shows-cycle-number
  "evolution-status tool output should include cycle number."
  (call-evolution-state-init)
  (let ((result (execute-tool "evolution-status" '())))
    (is (search "cycle" (string-downcase result))
        "evolution-status output should mention cycle")))

(test evolution-status-tool-shows-attempted-improvements
  "evolution-status tool output should include attempted improvements."
  (call-evolution-state-init)
  (call-evolution-state-record-attempt "Status test improvement" "success")
  (let ((result (execute-tool "evolution-status" '())))
    (is (or (search "attempted" (string-downcase result))
            (search "improvement" (string-downcase result)))
        "evolution-status output should mention attempted improvements")))

(test evolution-status-tool-graceful-when-uninitialized
  "evolution-status tool should handle nil *evolution-state* gracefully."
  ;; Temporarily set state to nil
  (let ((original (get-evolution-state)))
    (unwind-protect
         (progn
           (set-evolution-state nil)
           (let ((result (execute-tool "evolution-status" '())))
             (is (stringp result)
                 "evolution-status should return a string even when state is nil")))
      ;; Restore original state
      (set-evolution-state original))))

;;; ============================================================
;;; Test Suite: Evolution Progress Reporting
;;; ============================================================

(def-suite evolution-report-tests
  :description "Tests for evolution progress reporting functions"
  :in sibyl.tests:sibyl-tests)

(in-suite evolution-report-tests)

;;; ============================================================
;;; Test 1: evolution-report-cycle-start
;;; ============================================================

(test evolution-report-cycle-start-format
  "evolution-report-cycle-start should print correct format."
  (let ((output (with-output-to-string (stream)
                  (let ((*standard-output* stream))
                    (evolution-report-cycle-start 1 10)))))
    (is (search "=== Evolution Cycle 1/10 ===" output)
        "Output should contain cycle header with correct numbers")))

(test evolution-report-cycle-start-multiple-cycles
  "evolution-report-cycle-start should handle different cycle numbers."
  (let ((output (with-output-to-string (stream)
                  (let ((*standard-output* stream))
                    (evolution-report-cycle-start 5 20)))))
    (is (search "5/20" output)
        "Output should contain correct cycle/max numbers")))

;;; ============================================================
;;; Test 2: evolution-report-improvement-start
;;; ============================================================

(test evolution-report-improvement-start-format
  "evolution-report-improvement-start should print correct format."
  (let ((output (with-output-to-string (stream)
                  (let ((*standard-output* stream))
                    (evolution-report-improvement-start 1 3 "Add test for agent-run function")))))
    (is (search "[1/3]" output)
        "Output should contain improvement index/total")
    (is (search "Improving:" output)
        "Output should contain 'Improving:' label")
    (is (search "Add test for agent-run function" output)
        "Output should contain improvement name")))

(test evolution-report-improvement-start-multiple-improvements
  "evolution-report-improvement-start should handle different indices."
  (let ((output (with-output-to-string (stream)
                  (let ((*standard-output* stream))
                    (evolution-report-improvement-start 2 5 "Fix docstring")))))
    (is (search "[2/5]" output)
        "Output should contain correct improvement index/total")))

;;; ============================================================
;;; Test 3: evolution-report-step
;;; ============================================================

(test evolution-report-step-format
  "evolution-report-step should print step name with ellipsis."
  (let ((output (with-output-to-string (stream)
                  (let ((*standard-output* stream))
                    (evolution-report-step "RED")))))
    (is (search "Step:" output)
        "Output should contain 'Step:' label")
    (is (search "RED" output)
        "Output should contain step name")
    (is (search "..." output)
        "Output should contain ellipsis")))

(test evolution-report-step-multiple-steps
  "evolution-report-step should handle different step names."
  (let ((output (with-output-to-string (stream)
                  (let ((*standard-output* stream))
                    (evolution-report-step "GREEN")))))
    (is (search "GREEN" output)
        "Output should contain step name")))

;;; ============================================================
;;; Test 4: evolution-report-improvement-result
;;; ============================================================

(test evolution-report-improvement-result-success
  "evolution-report-improvement-result should print checkmark on success."
  (let ((output (with-output-to-string (stream)
                  (let ((*standard-output* stream))
                    (evolution-report-improvement-result t)))))
    (is (search "✓" output)
        "Output should contain checkmark for success")))

(test evolution-report-improvement-result-failure
  "evolution-report-improvement-result should print X and reason on failure."
  (let ((output (with-output-to-string (stream)
                  (let ((*standard-output* stream))
                    (evolution-report-improvement-result nil)))))
    (is (search "✗" output)
        "Output should contain X for failure")
    (is (search "skipped" output)
        "Output should mention skipped")))

;;; ============================================================
;;; Test 5: evolution-report-cycle-summary
;;; ============================================================

(test evolution-report-cycle-summary-format
  "evolution-report-cycle-summary should print correct format."
  (let ((output (with-output-to-string (stream)
                  (let ((*standard-output* stream))
                    (evolution-report-cycle-summary 2 1 716 724 20 20)))))
    (is (search "Cycle complete:" output)
        "Output should contain 'Cycle complete:' label")
    (is (search "2 succeeded" output)
        "Output should show succeeded count")
    (is (search "1 skipped" output)
        "Output should show skipped count")
    (is (search "716 → 724" output)
        "Output should show test count transition")
    (is (search "+8" output)
        "Output should show test delta with sign")))

(test evolution-report-cycle-summary-negative-delta
  "evolution-report-cycle-summary should handle negative test deltas."
  (let ((output (with-output-to-string (stream)
                  (let ((*standard-output* stream))
                    (evolution-report-cycle-summary 1 0 100 95 10 10)))))
    (is (search "100 → 95" output)
        "Output should show test count transition")
    (is (search "-5" output)
        "Output should show negative delta")))

(test evolution-report-cycle-summary-zero-delta
  "evolution-report-cycle-summary should handle zero test deltas."
  (let ((output (with-output-to-string (stream)
                  (let ((*standard-output* stream))
                    (evolution-report-cycle-summary 1 0 50 50 5 5)))))
    (is (search "50 → 50" output)
        "Output should show unchanged test count")
    (is (search "+0" output)
        "Output should show +0 delta")))

;;; ============================================================
;;; Test 6: evolution-report-final-summary
;;; ============================================================

(test evolution-report-final-summary-format
  "evolution-report-final-summary should print correct format."
  (let ((output (with-output-to-string (stream)
                  (let ((*standard-output* stream))
                    (evolution-report-final-summary 2 1 2 1 716 724)))))
    (is (search "=== Evolution Summary ===" output)
        "Output should contain summary header")
    (is (search "Cycles: 2" output)
        "Output should show total cycles")
    (is (search "1 productive" output)
        "Output should show productive cycles")
    (is (search "1 empty" output)
        "Output should show empty cycles")
    (is (search "2 succeeded" output)
        "Output should show total succeeded")
    (is (search "1 skipped" output)
        "Output should show total skipped")
    (is (search "716 → 724" output)
        "Output should show overall test transition")
    (is (search "+8" output)
        "Output should show overall test delta")))

(test evolution-report-final-summary-all-productive
  "evolution-report-final-summary should handle all productive cycles."
  (let ((output (with-output-to-string (stream)
                  (let ((*standard-output* stream))
                    (evolution-report-final-summary 3 3 5 0 100 110)))))
    (is (search "Cycles: 3" output)
        "Output should show total cycles")
    (is (search "3 productive" output)
        "Output should show all productive")
    (is (search "0 empty" output)
        "Output should show zero empty")))

(test evolution-report-final-summary-no-improvements
  "evolution-report-final-summary should handle no improvements."
  (let ((output (with-output-to-string (stream)
                  (let ((*standard-output* stream))
                    (evolution-report-final-summary 1 0 0 0 50 50)))))
    (is (search "Cycles: 1" output)
        "Output should show total cycles")
    (is (search "0 productive" output)
        "Output should show zero productive")
    (is (search "1 empty" output)
        "Output should show one empty")))
