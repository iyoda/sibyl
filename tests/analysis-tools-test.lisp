;;;; analysis-tools-test.lisp — Tests for analysis tools
;;;; (suggest-improvements, self-assess, improvement-plan, safe-redefine, sync-to-file, run-tests, write-test)

(in-package #:sibyl.tests)

(def-suite suggest-improvements-tests
  :description "Tests for suggest-improvements tool."
  :in sibyl-tests)

(in-suite suggest-improvements-tests)

(defun parse-suggest-improvements-result (json)
  "Parse JSON result from suggest-improvements tool."
  (let ((parsed (yason:parse json :object-as :hash-table)))
    (ensure-list (gethash "suggestions" parsed))))

(defun suggestion-priority-rank (priority)
  (cond
    ((string= priority "high") 0)
    ((string= priority "medium") 1)
    (t 2)))

(test suggest-improvements-generates-suggestions
  "suggest-improvements returns at least one suggestion."
  (let* ((learnings-path (asdf:system-relative-pathname
                          :sibyl
                          ".sisyphus/notepads/self-development-roadmap/learnings.md"))
         (original (when (probe-file learnings-path)
                     (uiop:read-file-string learnings-path))))
    (unwind-protect
          (let* ((result (sibyl.tools:execute-tool
                          "suggest-improvements"
                          '(("scope" . "tools"))))
                  (suggestions (parse-suggest-improvements-result result)))
             (is (listp suggestions))
             (is (> (length suggestions) 0))
             (is (<= (length suggestions) 10)))
      (when original
        (with-open-file (stream learnings-path :direction :output
                                :if-exists :supersede)
          (write-string original stream))))))

(test suggest-improvements-includes-required-fields
  "suggest-improvements includes description, rationale, and priority."
  (let* ((learnings-path (asdf:system-relative-pathname
                          :sibyl
                          ".sisyphus/notepads/self-development-roadmap/learnings.md"))
         (original (when (probe-file learnings-path)
                     (uiop:read-file-string learnings-path))))
    (unwind-protect
          (let* ((result (sibyl.tools:execute-tool
                          "suggest-improvements"
                          '(("scope" . "tools"))))
                  (suggestions (parse-suggest-improvements-result result)))
            (dolist (suggestion suggestions)
              (is (integerp (gethash "id" suggestion)))
              (is (stringp (gethash "description" suggestion)))
              (is (stringp (gethash "rationale" suggestion)))
              (is (stringp (gethash "priority" suggestion)))
              (is (stringp (gethash "category" suggestion)))
              (is (stringp (gethash "file" suggestion)))
              (is (integerp (gethash "line" suggestion)))))
      (when original
        (with-open-file (stream learnings-path :direction :output
                                :if-exists :supersede)
          (write-string original stream))))))

(test suggest-improvements-priorities-valid-and-ordered
  "suggest-improvements returns valid priority values in sorted order."
  (let* ((learnings-path (asdf:system-relative-pathname
                          :sibyl
                          ".sisyphus/notepads/self-development-roadmap/learnings.md"))
         (original (when (probe-file learnings-path)
                     (uiop:read-file-string learnings-path))))
    (unwind-protect
         (let* ((result (sibyl.tools:execute-tool
                         "suggest-improvements"
                         '(("scope" . "tools"))))
                 (suggestions (parse-suggest-improvements-result result))
                 (ranks (mapcar (lambda (suggestion)
                                  (let ((priority (gethash "priority" suggestion)))
                                    (is (member priority '("high" "medium" "low")
                                                :test #'string=))
                                    (suggestion-priority-rank priority)))
                                suggestions)))
           (is (loop for (a b) on ranks while b always (<= a b))))
      (when original
        (with-open-file (stream learnings-path :direction :output
                                :if-exists :supersede)
          (write-string original stream))))))

(def-suite self-assess-tests
  :description "Tests for self-assess tool."
  :in sibyl-tests)

(in-suite self-assess-tests)

(defun parse-self-assess-result (json)
  "Parse JSON result from self-assess tool."
  (yason:parse json :object-as :hash-table))

;;; Helper: bind *self-assess-running* with mock test results so that
;;; self-assess skips the full (run-tests) call, preventing recursive
;;; re-execution of the entire test suite.
(defun %mock-test-results ()
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "passed" h) 100
          (gethash "failed" h) 0
          (gethash "total" h) 100)
    h))

(defmacro with-self-assess-guard (&body body)
  `(let ((sibyl.tools::*self-assess-running* t)
         (sibyl.tools::*self-assess-last-test-results* (%mock-test-results)))
     ,@body))

(test self-assess-generates-report
  "self-assess returns a non-empty JSON report."
  (with-self-assess-guard
    (let ((result (sibyl.tools:execute-tool "self-assess" '())))
      (is (stringp result))
      (is (> (length result) 0)))))

(test self-assess-has-required-sections
  "self-assess includes toolset, codebase, test_coverage, and limitations."
  (with-self-assess-guard
    (let* ((result (sibyl.tools:execute-tool "self-assess" '()))
           (parsed (parse-self-assess-result result)))
      (is (gethash "toolset" parsed))
      (is (gethash "codebase" parsed))
      (is (gethash "test_coverage" parsed))
      (is (gethash "limitations" parsed)))))

(test self-assess-metrics-reasonable
  "self-assess returns reasonable non-zero metrics."
  (with-self-assess-guard
    (let* ((result (sibyl.tools:execute-tool "self-assess" '()))
           (parsed (parse-self-assess-result result))
           (toolset (gethash "toolset" parsed))
           (codebase (gethash "codebase" parsed))
           (test-coverage (gethash "test_coverage" parsed))
           (total-tools (gethash "total_tools" toolset))
           (total-lines (gethash "total_lines" codebase))
           (total-functions (gethash "total_functions" codebase))
           (total-modules (gethash "total_modules" codebase))
           (total-tests (gethash "total_tests" test-coverage))
           (coverage (gethash "coverage_estimate" test-coverage)))
      (is (integerp total-tools))
      (is (> total-tools 0))
      (is (integerp total-lines))
      (is (> total-lines 0))
      (is (integerp total-functions))
      (is (> total-functions 0))
      (is (integerp total-modules))
      (is (> total-modules 0))
      (is (integerp total-tests))
      (is (> total-tests 0))
      (is (numberp coverage))
      (is (>= coverage 0.0))
      (is (<= coverage 1.0)))))

(test self-assess-does-not-rerun-full-suite
  "*self-assess-running* が t の時、self-assess は run-tests をスキップしキャッシュを返すべき。"
  (with-self-assess-guard
    (let ((result (sibyl.tools:execute-tool "self-assess" nil)))
      (is (stringp result))
      (is (search "test_coverage" result)))))

(def-suite improvement-plan-tests
  :description "Tests for improvement-plan tool."
  :in sibyl-tests)

(in-suite improvement-plan-tests)

(defun parse-improvement-plan-result (json)
  "Parse JSON result from improvement-plan tool."
  (yason:parse json :object-as :hash-table))

(defun improvement-plan-improvements (parsed)
  (ensure-list (gethash "improvements" parsed)))

(test improvement-plan-generates-plan
  "improvement-plan returns a non-empty plan with improvements."
  (with-self-assess-guard
    (let* ((result (sibyl.tools:execute-tool "improvement-plan" '()))
           (parsed (parse-improvement-plan-result result))
           (improvements (improvement-plan-improvements parsed)))
      (is (stringp result))
      (is (> (length result) 0))
      (is (listp improvements))
      (is (> (length improvements) 0)))))

(test improvement-plan-has-required-fields
  "improvement-plan includes required fields and improvement structure."
  (with-self-assess-guard
    (let* ((result (sibyl.tools:execute-tool "improvement-plan" '()))
           (parsed (parse-improvement-plan-result result))
           (improvements (improvement-plan-improvements parsed))
           (summary (gethash "summary" parsed)))
      (is (stringp (gethash "plan_id" parsed)))
      (is (stringp (gethash "based_on_assessment" parsed)))
      (is (hash-table-p summary))
      (is (integerp (gethash "total_improvements" summary)))
      (dolist (improvement improvements)
        (is (integerp (gethash "id" improvement)))
        (is (stringp (gethash "title" improvement)))
        (is (stringp (gethash "description" improvement)))
        (is (stringp (gethash "category" improvement)))
        (is (stringp (gethash "priority" improvement)))
        (is (stringp (gethash "risk" improvement)))
        (is (stringp (gethash "effect" improvement)))
        (is (stringp (gethash "timeframe" improvement)))
        (is (stringp (gethash "rationale" improvement)))
        (is (stringp (gethash "estimated_effort" improvement)))))))

(test improvement-plan-priorities-valid
  "improvement-plan uses valid priority, risk, effect, and timeframe values."
  (with-self-assess-guard
    (let* ((result (sibyl.tools:execute-tool "improvement-plan" '()))
           (parsed (parse-improvement-plan-result result))
           (improvements (improvement-plan-improvements parsed)))
      (dolist (improvement improvements)
        (let ((priority (gethash "priority" improvement))
              (risk (gethash "risk" improvement))
              (effect (gethash "effect" improvement))
              (timeframe (gethash "timeframe" improvement)))
          (is (member priority '("high" "medium" "low") :test #'string=))
          (is (member risk '("high" "medium" "low") :test #'string=))
          (is (member effect '("high" "medium" "low") :test #'string=))
          (is (member timeframe '("short" "medium" "long") :test #'string=)))))))

(def-suite safe-redefine-tests
  :description "Tests for safe-redefine tool."
  :in sibyl-tests)

(in-suite safe-redefine-tests)

(test safe-redefine-successful
  "safe-redefine updates a function and compiles it."
  (eval '(defun sibyl.tests::safe-redefine-test-fn-001 () "original"))
  (compile 'sibyl.tests::safe-redefine-test-fn-001)
  (unwind-protect
       (let ((result (sibyl.tools:execute-tool
                      "safe-redefine"
                      '(("name" . "sibyl.tests::safe-redefine-test-fn-001")
                        ("new-definition" . "(defun sibyl.tests::safe-redefine-test-fn-001 () \"modified\")")))))
         (is (search "redefined" (string-downcase result)))
         (is (string= "modified" (sibyl.tests::safe-redefine-test-fn-001)))
         (is (compiled-function-p #'sibyl.tests::safe-redefine-test-fn-001)))
    (when (fboundp 'sibyl.tests::safe-redefine-test-fn-001)
      (fmakunbound 'sibyl.tests::safe-redefine-test-fn-001))))

(test safe-redefine-rollback
  "safe-redefine rolls back on error, preserving original function."
  (eval '(defun sibyl.tests::safe-redefine-test-fn-002 () "original"))
  (compile 'sibyl.tests::safe-redefine-test-fn-002)
  (unwind-protect
       (progn
         ;; Attempt redefinition with invalid code - should fail
         (handler-case
             (sibyl.tools:execute-tool
              "safe-redefine"
              '(("name" . "sibyl.tests::safe-redefine-test-fn-002")
                ("new-definition" . "invalid-lisp-code-here")))
           (sibyl.conditions:tool-execution-error (e)
             ;; Error expected - this is good
             (declare (ignore e))))
         ;; After error, function should still return original value (rollback worked)
         (is (string= "original" (sibyl.tests::safe-redefine-test-fn-002))))
    (when (fboundp 'sibyl.tests::safe-redefine-test-fn-002)
      (fmakunbound 'sibyl.tests::safe-redefine-test-fn-002))))

(test safe-redefine-rejects-non-sibyl
  "safe-redefine blocks redefinition outside Sibyl packages."
  (signals sibyl.conditions:tool-execution-error
    (sibyl.tools:execute-tool
     "safe-redefine"
     '(("name" . "cl:car")
       ("new-definition" . "(defun car (x) nil)")))))

(def-suite sync-to-file-tests
  :description "Tests for sync-to-file tool."
  :in sibyl-tests)

(in-suite sync-to-file-tests)

(defun %sync-to-file-join-lines (lines)
  (with-output-to-string (stream)
    (loop for line in lines
          for idx from 0
          do (write-string line stream)
             (when (< idx (1- (length lines)))
               (write-char #\Newline stream)))))

(defun %sync-to-file-temp-path ()
  (let* ((dir (uiop:temporary-directory))
         (name (format nil "sibyl-sync-to-file-~a-~a.lisp"
                       (get-universal-time)
                       (random 1000000))))
    (namestring (merge-pathnames name dir))))

(test sync-to-file-replaces-definition
  "sync-to-file replaces only the target definition and preserves comments."
  (let* ((path (%sync-to-file-temp-path))
         (original (%sync-to-file-join-lines
                    '(";; header comment"
                      ""
                      "(defun first ()"
                      "  ;; first comment"
                      "  1)"
                      ""
                      ";; middle comment before"
                      "(defun middle ()"
                      "  ;; inside middle"
                      "  2)"
                      ""
                      ";; trailing comment"
                      "(defun last ()"
                      "  3)"
                      "")))
         (new-source (%sync-to-file-join-lines
                      '("(defun middle ()"
                        "  ;; inside middle updated"
                        "  42)"
                        "")))
         (expected (%sync-to-file-join-lines
                    '(";; header comment"
                      ""
                      "(defun first ()"
                      "  ;; first comment"
                      "  1)"
                      ""
                      ";; middle comment before"
                      "(defun middle ()"
                      "  ;; inside middle updated"
                      "  42)"
                      ""
                      ";; trailing comment"
                      "(defun last ()"
                      "  3)"
                      ""))))
    (unwind-protect
         (progn
           (with-open-file (stream path :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-string original stream))
           (sibyl.tools:execute-tool
            "sync-to-file"
            (list (cons "name" "middle")
                  (cons "file" path)
                  (cons "new-source" new-source)))
           (let ((updated (uiop:read-file-string path)))
             (is (string= expected updated))
             (is (search "header comment" updated))
             (is (search "trailing comment" updated))
             (is (not (search "  2)" updated)))
             (is (search "  42)" updated))))
      (when (probe-file path)
        (delete-file path)))))

(test sync-to-file-missing-definition
  "sync-to-file errors when the target definition is missing."
  (let* ((path (%sync-to-file-temp-path))
         (content (%sync-to-file-join-lines
                   '("(defun alpha () 1)"
                     ""))))
    (unwind-protect
         (progn
           (with-open-file (stream path :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-string content stream))
           (signals sibyl.conditions:tool-execution-error
             (sibyl.tools:execute-tool
              "sync-to-file"
              (list (cons "name" "missing")
                    (cons "file" path)
                    (cons "new-source" "(defun missing () 9)")))))
      (when (probe-file path)
        (delete-file path)))))

(def-suite run-tests-tests
  :description "Tests for run-tests tool."
  :in sibyl-tests)

(in-suite run-tests-tests)

(defun parse-run-tests-result (json)
  "Parse JSON result from run-tests tool."
  (yason:parse json :object-as :hash-table))

(test run-tests-all-tests
  "run-tests runs all tests and returns structured results."
  ;; Run a specific suite to avoid infinite recursion
  (let* ((result (sibyl.tools:execute-tool "run-tests" '(("suite" . "read-sexp-tests"))))
         (parsed (parse-run-tests-result result))
         (total (gethash "total" parsed))
         (passed (gethash "passed" parsed))
         (failed (gethash "failed" parsed))
         (failures (gethash "failures" parsed)))
    (is (integerp total))
    (is (integerp passed))
    (is (integerp failed))
    (is (> total 0))
    (is (= passed total))
    (is (= failed 0))
    (is (or (null failures) (vectorp failures) (listp failures)))))

(test run-tests-specific-suite
  "run-tests can run a specific test suite."
  (let* ((result (sibyl.tools:execute-tool
                  "run-tests"
                  '(("suite" . "describe-symbol-tests"))))
         (parsed (parse-run-tests-result result))
         (total (gethash "total" parsed)))
    (is (integerp total))
    (is (> total 0))))

(test run-tests-detects-failures
  "run-tests detects and reports test failures."
  ;; Define a failing test
  (eval '(fiveam:test run-tests-intentional-failure
           "This test is designed to fail."
           (fiveam:is (= 1 2))))
  
  (unwind-protect
       (let* ((result (sibyl.tools:execute-tool
                       "run-tests"
                       '(("test" . "run-tests-intentional-failure"))))
              (parsed (parse-run-tests-result result))
              (total (gethash "total" parsed))
              (passed (gethash "passed" parsed))
              (failed (gethash "failed" parsed))
              (failures (gethash "failures" parsed)))
         (is (= total 1))
         (is (= passed 0))
         (is (= failed 1))
         (is (or (vectorp failures) (listp failures)))
         (let ((failures-list (if (vectorp failures)
                                  (coerce failures 'list)
                                  failures)))
           (is (= (length failures-list) 1))))
    ;; Cleanup - remove the failing test
    (when (fiveam:get-test 'run-tests-intentional-failure)
      (fiveam:rem-test 'run-tests-intentional-failure))))

(def-suite write-test-tests
  :description "Tests for write-test tool."
  :in sibyl-tests)

(in-suite write-test-tests)

(test write-test-generates-and-registers-test
  "write-test generates a test and registers it in-memory."
  (let* ((test-name "write-test-auto-generated-001")
         (test-symbol (intern (string-upcase test-name) :sibyl.tests))
         (test-file (asdf:system-relative-pathname :sibyl "tests/sexp-tools-test.lisp"))
         (original-content (uiop:read-file-string test-file)))
    (unwind-protect
         (progn
           ;; Generate test using write-test tool
           (let ((result (sibyl.tools:execute-tool
                          "write-test"
                          `(("name" . ,test-name)
                            ("body" . "(is (equal 1 1))")))))
             ;; Should succeed
             (is (search "success" (string-downcase result)))
             (is (search test-name (string-downcase result)))
             
             ;; Test should be registered in-memory
             (is (not (null (fiveam:get-test test-symbol))))
             
             ;; Test should be executable
             (let ((test-result (fiveam:run test-symbol)))
               (is (not (null test-result))))))
      ;; Cleanup
      (when (fiveam:get-test test-symbol)
        (fiveam:rem-test test-symbol))
      ;; Restore file content to prevent test artifacts from persisting
      (with-open-file (stream test-file :direction :output :if-exists :supersede)
        (write-string original-content stream)))))

(test write-test-appends-to-file
  "write-test appends the generated test to the specified file."
  (let* ((test-name "write-test-persistence-check")
         (test-symbol (intern (string-upcase test-name) :sibyl.tests))
         (test-file (asdf:system-relative-pathname :sibyl "tests/sexp-tools-test.lisp")))
    (unwind-protect
         (progn
           ;; Read original file content
           (let ((original-content (uiop:read-file-string test-file)))
             ;; Generate test
             (sibyl.tools:execute-tool
              "write-test"
              `(("name" . ,test-name)
                ("body" . "(is (equal 2 2))")
                ("file" . ,(namestring test-file))))
             
             ;; File should contain the new test
             (let ((new-content (uiop:read-file-string test-file)))
               (is (search (format nil "(test ~a" test-name) new-content))
               
               ;; Restore original file (we don't want to persist test code in test file)
               (with-open-file (stream test-file :direction :output
                                       :if-exists :supersede)
                 (write-string original-content stream)))))
      ;; Cleanup
      (when (fiveam:get-test test-symbol)
        (fiveam:rem-test test-symbol)))))

(test write-test-rejects-duplicate-name
  "write-test rejects duplicate test names."
  (let* ((test-name "write-test-duplicate-check")
         (test-symbol (intern (string-upcase test-name) :sibyl.tests))
         (test-file (asdf:system-relative-pathname :sibyl "tests/sexp-tools-test.lisp"))
         (original-content (uiop:read-file-string test-file)))
    (unwind-protect
         (progn
            ;; Create first test
            (sibyl.tools:execute-tool
             "write-test"
             `(("name" . ,test-name)
               ("body" . "(is (eq t t))")))
            
            ;; Attempt to create duplicate should fail
            (signals sibyl.conditions:tool-execution-error
              (sibyl.tools:execute-tool
               "write-test"
               `(("name" . ,test-name)
                 ("body" . "(is (eq t t))")))))
      ;; Cleanup
      (when (fiveam:get-test test-symbol)
        (fiveam:rem-test test-symbol))
      ;; Restore file content to prevent test artifacts from persisting
      (with-open-file (stream test-file :direction :output :if-exists :supersede)
        (write-string original-content stream)))))

(test write-test-validates-required-parameters
  "write-test validates that required parameters are present."
  ;; Missing name - should trigger validation error
  (signals sibyl.conditions:tool-validation-error
    (sibyl.tools:execute-tool
     "write-test"
     '(("body" . "(is t)"))))
  
  ;; Missing body - should trigger validation error
  (signals sibyl.conditions:tool-validation-error
    (sibyl.tools:execute-tool
     "write-test"
     '(("name" . "test-no-body")))))

(test write-test-uses-default-suite
  "write-test defaults to sibyl-tests suite."
  (let* ((test-name "write-test-default-suite-check")
         (test-symbol (intern (string-upcase test-name) :sibyl.tests))
         (test-file (asdf:system-relative-pathname :sibyl "tests/sexp-tools-test.lisp"))
         (original-content (uiop:read-file-string test-file)))
    (unwind-protect
         (progn
           (sibyl.tools:execute-tool
            "write-test"
            `(("name" . ,test-name)
              ("body" . "(is (equal 3 3))")))
           
           ;; Test should exist
           (is (not (null (fiveam:get-test test-symbol))))
           
           ;; Test should be in sibyl-tests suite
           (let ((test-obj (fiveam:get-test test-symbol)))
             (is (not (null test-obj)))))
      ;; Cleanup
      (when (fiveam:get-test test-symbol)
        (fiveam:rem-test test-symbol))
      ;; Restore file content to prevent test artifacts from persisting
      (with-open-file (stream test-file :direction :output :if-exists :supersede)
        (write-string original-content stream)))))

(test write-test-generated-test-runs-successfully
  "write-test generated test can be run via run-tests tool."
  (let* ((test-name "write-test-runnable-check")
         (test-symbol (intern (string-upcase test-name) :sibyl.tests))
         (test-file (asdf:system-relative-pathname :sibyl "tests/sexp-tools-test.lisp"))
         (original-content (uiop:read-file-string test-file)))
    (unwind-protect
         (progn
           ;; Generate test
           (sibyl.tools:execute-tool
            "write-test"
            `(("name" . ,test-name)
              ("body" . "(is (equal 4 4))")))
           
           ;; Run via run-tests tool
           (let* ((result (sibyl.tools:execute-tool
                           "run-tests"
                           `(("test" . ,test-name))))
                  (parsed (parse-run-tests-result result))
                  (passed (gethash "passed" parsed))
                  (failed (gethash "failed" parsed)))
             (is (= passed 1))
             (is (= failed 0))))
      ;; Cleanup
      (when (fiveam:get-test test-symbol)
        (fiveam:rem-test test-symbol))
      ;; Restore file content to prevent test artifacts from persisting
      (with-open-file (stream test-file :direction :output :if-exists :supersede)
        (write-string original-content stream)))))



(test test-file-to-suite-mapping
  "Auto-generated test"
  (let ((mapping (sibyl.tools::%file-to-suite-mapping)))
  (is (hash-table-p mapping))
  (let ((lisp-suites (gethash "src/tools/lisp-tools.lisp" mapping)))
    (is (listp lisp-suites))
    (is (member "read-sexp-tests" lisp-suites :test #'string=)))
  (is (string= (gethash "src/agent/core.lisp" mapping) "agent-tests"))))

(test test-smart-suite-selection
  "Auto-generated test"
  ;; %suites-for-files が変更ファイルから関連スイートを返すことを確認
(let ((suites (sibyl.tools::%suites-for-files
               '("src/tools/lisp-tools.lisp" "src/agent/core.lisp"))))
  (is (listp suites))
  (is (member "read-sexp-tests" suites :test #'string=))
  (is (member "agent-tests" suites :test #'string=)))

;; 未知のファイルは無視される
(let ((suites (sibyl.tools::%suites-for-files '("src/unknown/file.lisp"))))
  (is (null suites))))

(test test-run-tests-with-files
  "Auto-generated test"
  ;; :files オプションで関連スイートのみ実行できることを確認
;; (実際のテスト実行は行わず、ツールが :files を受け付けることを確認)
(let ((tool (sibyl.tools:find-tool "run-tests")))
  (is (not (null tool)))
  (let ((params (getf (sibyl.tools::tool-spec tool) :parameters)))
    (is (some (lambda (p) (string= (getf p :name) "files")) params)))))

(test test-run-tests-files-option
  "Auto-generated test"
  ;; :files オプションで analysis-tools-test スイートが実行されることを確認
(let* ((result-json (sibyl.tools:execute-tool
                     "run-tests"
                     '(("files" . "src/tools/analysis-tools.lisp"))))
       (result (sibyl.tools::%tools-parse-json result-json)))
  (is (hash-table-p result))
  (is (integerp (gethash "total" result)))
  (is (> (gethash "total" result) 0))))

(test test-file-to-suite-mapping-multi-suite
  "Auto-generated test"
  ;; analysis-tools.lisp は複数スイートを返す
(let* ((suites (sibyl.tools::%suites-for-files
                '("src/tools/analysis-tools.lisp"))))
  (is (listp suites))
  (is (> (length suites) 1))
  (is (member "run-tests-tests" suites :test #'string=))
  (is (member "suggest-improvements-tests" suites :test #'string=))))
