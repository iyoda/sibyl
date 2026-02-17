;;;; sexp-tools-test.lisp — Tests for s-expression tools

(in-package #:sibyl.tests)

(def-suite read-sexp-tests
  :description "Tests for read-sexp tool."
  :in sibyl-tests)

(in-suite read-sexp-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((tools-file (asdf:system-relative-pathname :sibyl
                                                   "src/tools/lisp-tools.lisp")))
    (when (probe-file tools-file)
      (load tools-file))))

(defun parse-read-sexp-result (json)
  (let ((parsed (yason:parse json :object-as :hash-table)))
    (if (vectorp parsed)
        (coerce parsed 'list)
        parsed)))

(defun entry-name (entry)
  (gethash "name" entry))

(defun entry-type (entry)
  (gethash "type" entry))

(defun entry-start-line (entry)
  (gethash "start_line" entry))

(defun entry-end-line (entry)
  (gethash "end_line" entry))

(defun entry-form (entry)
  (gethash "form" entry))

(test read-sexp-collects-deftools
  "read-sexp finds all deftool forms in builtin.lisp."
  (let* ((result (sibyl.tools:execute-tool
                  "read-sexp"
                  '(("path" . "src/tools/builtin.lisp"))))
         (entries (parse-read-sexp-result result))
         (names (mapcar #'entry-name entries)))
    (is (= 6 (length entries)))
    (dolist (expected '("read-file" "write-file" "list-directory"
                        "shell" "grep" "file-info"))
      (is (find expected names :test #'string=)))
    (dolist (entry entries)
      (is (string= "deftool" (entry-type entry)))
      (is (integerp (entry-start-line entry)))
      (is (integerp (entry-end-line entry)))
      (is (<= (entry-start-line entry) (entry-end-line entry))))))

(test read-sexp-filter-by-name
  "read-sexp can filter by tool name."
  (let* ((result (sibyl.tools:execute-tool
                  "read-sexp"
                  '(("path" . "src/tools/builtin.lisp")
                    ("name" . "read-file"))))
         (entries (parse-read-sexp-result result)))
    (is (= 1 (length entries)))
    (let ((entry (first entries)))
      (is (string= "read-file" (entry-name entry)))
      (is (search "(deftool \"read-file\"" (entry-form entry))))))

(test read-sexp-filter-by-type
  "read-sexp can filter by type."
  (let* ((result (sibyl.tools:execute-tool
                  "read-sexp"
                  '(("path" . "src/tools/builtin.lisp")
                    ("type" . "deftool"))))
         (entries (parse-read-sexp-result result)))
    (is (= 6 (length entries)))))

(test read-sexp-missing-file
  "read-sexp signals tool errors for missing files."
  (signals sibyl.conditions:tool-execution-error
    (sibyl.tools:execute-tool
     "read-sexp"
     '(("path" . "/nonexistent.lisp")))))

(def-suite describe-symbol-tests
  :description "Tests for describe-symbol tool."
  :in sibyl-tests)

(in-suite describe-symbol-tests)

(defclass test-agent (sibyl.agent:agent) ())

(defmethod sibyl.agent:agent-step ((agent test-agent) user-input)
  (declare (ignore agent user-input))
  :test)

(test describe-symbol-generic-function
  "describe-symbol reports generic functions and methods."
  (let ((result (sibyl.tools:execute-tool
                 "describe-symbol"
                 '(:symbol "sibyl.agent:agent-step"))))
    (is (search "generic-function" result :test #'char-equal))
    (is (search "methods: 2" result :test #'char-equal))))

(test describe-symbol-special-variable
  "describe-symbol reports special variables."
  (let ((result (sibyl.tools:execute-tool
                 "describe-symbol"
                 '(:symbol "sibyl.tools:*tool-registry*"))))
    (is (search "special-variable" result :test #'char-equal))
    (is (search "special-variable: t" result :test #'char-equal))))

(test describe-symbol-missing
  "describe-symbol handles missing symbols."
  (let ((result (sibyl.tools:execute-tool
                 "describe-symbol"
                 '(:symbol "sibyl.agent:nonexistent-xyz"))))
    (is (search "not-found" result :test #'char-equal))))

(def-suite eval-form-tests
  :description "Tests for eval-form tool."
  :in sibyl-tests)

(in-suite eval-form-tests)

(test eval-form-simple
  "eval-form evaluates a simple expression."
  (let ((result (sibyl.tools:execute-tool "eval-form"
                                          '(:form "(+ 1 2)"))))
    (is (string= "3" result))))

(test eval-form-compiles-defun
  "eval-form compiles newly defined functions."
  (sibyl.tools:execute-tool "eval-form"
                            '(:form "(defun eval-form-test-fn () 42)"))
  (let* ((pkg (find-package :sibyl))
         (sym (and pkg (find-symbol "EVAL-FORM-TEST-FN" pkg))))
    (unwind-protect
         (progn
           (is (not (null sym)))
           (when sym
             (is (fboundp sym))
             (is (compiled-function-p (symbol-function sym)))))
      (when (and sym (fboundp sym))
        (fmakunbound sym)))))

(test eval-form-timeout
  "eval-form enforces timeouts for runaway evaluations."
  (let ((result (sibyl.tools:execute-tool "eval-form"
                                          '(:form "(loop)"
                                            :timeout 1))))
    (is (search "timeout" (string-downcase result)))))

(test eval-form-captures-output
  "eval-form captures standard output and error output."
  (let ((result (sibyl.tools:execute-tool "eval-form"
                                          '(:form "(progn (format t \"hello\")
                                                         (format *error-output* \"oops\")
                                                         7)"))))
    (is (search "hello" result))
    (is (search "oops" result))
    (is (search "7" result))))

(test eval-form-multiple-values
  "eval-form returns all values as a string."
  (let ((result (sibyl.tools:execute-tool "eval-form"
                                           '(:form "(values 1 2 3)"))))
    (is (string= "1 2 3" result))))

(def-suite macroexpand-form-tests
  :description "Tests for macroexpand-form tool."
  :in sibyl-tests)

(in-suite macroexpand-form-tests)

(test macroexpand-form-simple-macro
  "macroexpand-form expands a simple macro."
  (let ((result (sibyl.tools:execute-tool
                 "macroexpand-form"
                 '(("form" . "(when t (+ 1 2))")))))
    (is (stringp result))
    (is (search "IF" result :test #'char-equal))))

(test macroexpand-form-deftool-full-expansion
  "macroexpand-form fully expands deftool macro."
  (let ((result (sibyl.tools:execute-tool
                 "macroexpand-form"
                 '(("form" . "(deftool \"test-tool\" (:description \"Test\" :parameters ()) (+ 1 1))")
                   ("full" . "true")))))
    (is (stringp result))
    (is (search "MAKE-TOOL" result :test #'char-equal))
    (is (search "REGISTER-TOOL" result :test #'char-equal))))

(test macroexpand-form-deftool-single-step
  "macroexpand-form expands deftool one step."
  (let ((result (sibyl.tools:execute-tool
                 "macroexpand-form"
                 '(("form" . "(deftool \"test-tool\" (:description \"Test\" :parameters ()) (+ 1 1))")
                   ("full" . "false")))))
    (is (stringp result))
    (is (search "LET" result :test #'char-equal))))

(test macroexpand-form-non-macro
  "macroexpand-form handles non-macro forms."
  (let ((result (sibyl.tools:execute-tool
                 "macroexpand-form"
                 '(("form" . "(+ 1 2)")))))
    (is (stringp result))
    (is (search "+" result))))

(test macroexpand-form-invalid-form
  "macroexpand-form signals error for invalid forms."
  (signals sibyl.conditions:tool-execution-error
    (sibyl.tools:execute-tool
     "macroexpand-form"
     '(("form" . "(unmatched-paren")))))

(def-suite package-symbols-tests
  :description "Tests for package-symbols tool."
  :in sibyl-tests)

(in-suite package-symbols-tests)

(defun parse-package-symbols-result (json)
  "Parse JSON result from package-symbols tool."
  (let ((parsed (yason:parse json :object-as :hash-table)))
    (if (vectorp parsed)
        (coerce parsed 'list)
        parsed)))

(defun symbol-entry-name (entry)
  (gethash "name" entry))

(defun symbol-entry-type (entry)
  (gethash "type" entry))

(test package-symbols-lists-exported-symbols
  "package-symbols lists exported symbols from SIBYL.TOOLS by default."
  (let* ((result (sibyl.tools:execute-tool
                  "package-symbols"
                  '(("package" . "SIBYL.TOOLS"))))
         (entries (parse-package-symbols-result result))
         (names (mapcar #'symbol-entry-name entries)))
    (is (> (length entries) 0))
    (is (find "DEFTOOL" names :test #'string=))
    (is (find "EXECUTE-TOOL" names :test #'string=))
    (is (find "FIND-TOOL" names :test #'string=))))

(test package-symbols-includes-type-annotations
  "package-symbols includes type annotations for each symbol."
  (let* ((result (sibyl.tools:execute-tool
                  "package-symbols"
                  '(("package" . "SIBYL.TOOLS"))))
         (entries (parse-package-symbols-result result)))
    (dolist (entry entries)
      (is (gethash "name" entry))
      (is (gethash "type" entry))
      (let ((type (symbol-entry-type entry)))
        (is (member type '("function" "variable" "class" "macro")
                    :test #'string=))))))

(test package-symbols-external-only-default
  "package-symbols defaults to external-only=true."
  (let* ((result (sibyl.tools:execute-tool
                  "package-symbols"
                  '(("package" . "SIBYL.TOOLS"))))
         (entries (parse-package-symbols-result result)))
    ;; All entries should be exported symbols
    (dolist (entry entries)
      (let ((name (symbol-entry-name entry)))
        (is (not (null name)))))))

(test package-symbols-all-symbols-mode
  "package-symbols can list all symbols when external-only=false."
  (let* ((result-exported (sibyl.tools:execute-tool
                           "package-symbols"
                           '(("package" . "SIBYL.TOOLS")
                             ("external-only" . "true"))))
         (result-all (sibyl.tools:execute-tool
                      "package-symbols"
                      '(("package" . "SIBYL.TOOLS")
                        ("external-only" . "false"))))
         (exported-count (length (parse-package-symbols-result result-exported)))
         (all-count (length (parse-package-symbols-result result-all))))
    ;; All symbols should be >= exported symbols
    (is (>= all-count exported-count))))

(test package-symbols-sorted-alphabetically
  "package-symbols returns symbols sorted alphabetically."
  (let* ((result (sibyl.tools:execute-tool
                  "package-symbols"
                  '(("package" . "SIBYL.TOOLS"))))
         (entries (parse-package-symbols-result result))
         (names (mapcar #'symbol-entry-name entries)))
    (is (equal names (sort (copy-list names) #'string<)))))

(test package-symbols-invalid-package
  "package-symbols signals error for invalid package."
  (signals sibyl.conditions:tool-execution-error
    (sibyl.tools:execute-tool
     "package-symbols"
     '(("package" . "NONEXISTENT-PACKAGE-XYZ")))))

(def-suite codebase-map-tests
  :description "Tests for codebase-map tool."
  :in sibyl-tests)

(in-suite codebase-map-tests)

(defun parse-codebase-map-result (json)
  "Parse JSON result from codebase-map tool."
  (yason:parse json :object-as :hash-table))

(defun ensure-list (value)
  (cond
    ((vectorp value) (coerce value 'list))
    ((null value) nil)
    (t value)))

(defun codebase-map-modules (result)
  (ensure-list (gethash "modules" result)))

(defun module-name (module)
  (gethash "name" module))

(defun module-files (module)
  (ensure-list (gethash "files" module)))

(defun file-path (file)
  (gethash "path" file))

(defun file-definitions (file)
  (ensure-list (gethash "definitions" file)))

(defun definition-name (definition)
  (gethash "name" definition))

(test codebase-map-summary-modules
  "codebase-map summary includes expected modules."
  (let* ((result (sibyl.tools:execute-tool
                  "codebase-map"
                  '(("detail-level" . "summary"))))
         (parsed (parse-codebase-map-result result))
         (modules (codebase-map-modules parsed))
         (names (mapcar #'module-name modules)))
    (dolist (expected '("llm" "tools" "agent" "repl" "util" "conditions"))
      (is (find expected names :test #'string=)))))

(test codebase-map-finds-agent-step
  "codebase-map full detail includes agent-step definition."
  (let* ((result (sibyl.tools:execute-tool
                  "codebase-map"
                  '(("detail-level" . "full"))))
         (parsed (parse-codebase-map-result result))
         (modules (codebase-map-modules parsed))
         (agent-module (find "agent" modules :key #'module-name :test #'string=))
         (files (and agent-module (module-files agent-module)))
         (core-file (find "src/agent/core.lisp" files :key #'file-path :test #'string=))
         (definitions (and core-file (file-definitions core-file)))
         (names (mapcar #'definition-name definitions)))
    (is (not (null agent-module)))
    (is (not (null core-file)))
    (is (find "agent-step" names :test #'string=))))

(def-suite who-calls-tests
  :description "Tests for who-calls tool."
  :in sibyl-tests)

(in-suite who-calls-tests)

(test who-calls-finds-callers
  "who-calls identifies functions that call a target function."
  ;; Define and compile test functions in SIBYL.TESTS package
  (eval '(defun sibyl.tests::who-calls-test-target () 42))
  (compile 'sibyl.tests::who-calls-test-target)
  (eval '(defun sibyl.tests::who-calls-test-caller () (sibyl.tests::who-calls-test-target)))
  (compile 'sibyl.tests::who-calls-test-caller)
  
  (unwind-protect
       (let* ((result (sibyl.tools:execute-tool
                       "who-calls"
                       '(("function" . "sibyl.tests:who-calls-test-target")
                         ("direction" . "callers"))))
              (result-lower (string-downcase result)))
         (is (search "who-calls-test-caller" result-lower)))
    ;; Cleanup
    (when (fboundp 'sibyl.tests::who-calls-test-target)
      (fmakunbound 'sibyl.tests::who-calls-test-target))
    (when (fboundp 'sibyl.tests::who-calls-test-caller)
      (fmakunbound 'sibyl.tests::who-calls-test-caller))))

(test who-calls-finds-agent-step-calls-execute-tool-call
  "who-calls identifies agent-step as caller of execute-tool-call."
  (let* ((result (sibyl.tools:execute-tool
                  "who-calls"
                  '(("function" . "sibyl.tools:execute-tool-call")
                    ("direction" . "callers"))))
         (result-lower (string-downcase result)))
    (is (search "agent-step" result-lower))))

(test who-calls-handles-nonexistent-function
  "who-calls handles nonexistent functions gracefully."
  (let ((result (sibyl.tools:execute-tool
                 "who-calls"
                 '(("function" . "sibyl.tools:nonexistent-function-xyz")))))
    (is (stringp result))
    ;; Should not crash, should return empty or warning
    (is (or (search "not found" (string-downcase result))
            (search "no callers" (string-downcase result))
            (search "warning" (string-downcase result))))))

(test who-calls-handles-special-variable
  "who-calls recognizes special variables and reports appropriately."
  (let ((result (sibyl.tools:execute-tool
                 "who-calls"
                 '(("function" . "sibyl.tools:*tool-registry*")))))
    (is (stringp result))
    ;; Should indicate it's a variable, not a function
    (is (or (search "variable" (string-downcase result))
            (search "not a function" (string-downcase result))))))

(test who-calls-default-direction-is-callers
  "who-calls defaults to callers direction when not specified."
  (eval '(defun sibyl.tests::who-calls-test-default-target () 99))
  (compile 'sibyl.tests::who-calls-test-default-target)
  (eval '(defun sibyl.tests::who-calls-test-default-caller () (sibyl.tests::who-calls-test-default-target)))
  (compile 'sibyl.tests::who-calls-test-default-caller)
  
  (unwind-protect
       (let* ((result (sibyl.tools:execute-tool
                       "who-calls"
                       '(("function" . "sibyl.tests:who-calls-test-default-target"))))
              (result-lower (string-downcase result)))
         (is (search "who-calls-test-default-caller" result-lower)))
    ;; Cleanup
     (when (fboundp 'sibyl.tests::who-calls-test-default-target)
      (fmakunbound 'sibyl.tests::who-calls-test-default-target))
     (when (fboundp 'sibyl.tests::who-calls-test-default-caller)
       (fmakunbound 'sibyl.tests::who-calls-test-default-caller))))

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

(test self-assess-generates-report
  "self-assess returns a non-empty JSON report."
  (let ((result (sibyl.tools:execute-tool "self-assess" '())))
    (is (stringp result))
    (is (> (length result) 0))))

(test self-assess-has-required-sections
  "self-assess includes toolset, codebase, test_coverage, and limitations."
  (let* ((result (sibyl.tools:execute-tool "self-assess" '()))
         (parsed (parse-self-assess-result result)))
    (is (gethash "toolset" parsed))
    (is (gethash "codebase" parsed))
    (is (gethash "test_coverage" parsed))
    (is (gethash "limitations" parsed))))

(test self-assess-metrics-reasonable
  "self-assess returns reasonable non-zero metrics."
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
    (is (<= coverage 1.0))))

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
  (let* ((result (sibyl.tools:execute-tool "improvement-plan" '()))
         (parsed (parse-improvement-plan-result result))
         (improvements (improvement-plan-improvements parsed)))
    (is (stringp result))
    (is (> (length result) 0))
    (is (listp improvements))
    (is (> (length improvements) 0))))

(test improvement-plan-has-required-fields
  "improvement-plan includes required fields and improvement structure."
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
      (is (stringp (gethash "estimated_effort" improvement))))))

(test improvement-plan-priorities-valid
  "improvement-plan uses valid priority, risk, effect, and timeframe values."
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
        (is (member timeframe '("short" "medium" "long") :test #'string=))))))

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
         (test-symbol (intern (string-upcase test-name) :sibyl.tests)))
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
        (fiveam:rem-test test-symbol)))))

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
         (test-symbol (intern (string-upcase test-name) :sibyl.tests)))
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
        (fiveam:rem-test test-symbol)))))

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
         (test-symbol (intern (string-upcase test-name) :sibyl.tests)))
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
        (fiveam:rem-test test-symbol)))))

(test write-test-generated-test-runs-successfully
  "write-test generated test can be run via run-tests tool."
  (let* ((test-name "write-test-runnable-check")
         (test-symbol (intern (string-upcase test-name) :sibyl.tests)))
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
        (fiveam:rem-test test-symbol)))))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))

(test write-test-auto-generated-001
  "Auto-generated test"
  (is (equal 1 1)))

(test write-test-duplicate-check
  "Auto-generated test"
  (is (eq t t)))

(test write-test-default-suite-check
  "Auto-generated test"
  (is (equal 3 3)))

(test write-test-runnable-check
  "Auto-generated test"
  (is (equal 4 4)))
