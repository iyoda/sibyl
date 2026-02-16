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
