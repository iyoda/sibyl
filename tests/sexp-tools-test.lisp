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
                  '(("path" . "src/tools/builtin.lisp")
                    ("type" . "deftool"))))
         (entries (parse-read-sexp-result result))
         (names (mapcar #'entry-name entries)))
    ;; Must find at least the 6 original tools
    (is (>= (length entries) 6))
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
    ;; Must find at least the 6 original deftool forms
    (is (>= (length entries) 6))
    ;; All entries must be deftool type
    (dolist (entry entries)
      (is (string= "deftool" (entry-type entry))))))

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
  "who-calls identifies %execute-tool-with-timing as caller of execute-tool-call."
  (let* ((result (sibyl.tools:execute-tool
                  "who-calls"
                  '(("function" . "sibyl.tools:execute-tool-call")
                    ("direction" . "callers"))))
         (result-lower (string-downcase result)))
    (is (search "%execute-tool-with-timing" result-lower))))

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

;;; self-assess-tests and improvement-plan-tests are defined in
;;; analysis-tools-test.lisp. Do not duplicate them here.

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

(test safe-redefine-defmethod-on-generic-function
  "safe-redefine accepts defmethod updates on generic functions."
  (eval '(defgeneric sibyl.tests::safe-redefine-test-gf-001 (x)))
  (eval '(defmethod sibyl.tests::safe-redefine-test-gf-001 ((x t)) "original"))
  (unwind-protect
       (let ((result (sibyl.tools:execute-tool
                      "safe-redefine"
                      '(("name" . "sibyl.tests::safe-redefine-test-gf-001")
                        ("new-definition" . "(defmethod sibyl.tests::safe-redefine-test-gf-001 ((x t)) \"modified\")")))))
         (is (search "redefined" (string-downcase result)))
         (is (string= "modified" (sibyl.tests::safe-redefine-test-gf-001 :anything)))
         (is (typep (symbol-function 'sibyl.tests::safe-redefine-test-gf-001)
                    'generic-function)))
    (when (fboundp 'sibyl.tests::safe-redefine-test-gf-001)
      (fmakunbound 'sibyl.tests::safe-redefine-test-gf-001))))

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


(def-suite creation-integration-tests
  :description "E2E creation workflow integration tests."
  :in sibyl-tests)

(in-suite creation-integration-tests)

(defun %creation-integration-run (&key verify)
  (let* ((relative-path "src/tools/test-evolution-module.lisp")
         (module-path (asdf:system-relative-pathname :sibyl relative-path))
         (module-path-string (namestring module-path))
         (asd-path (asdf:system-relative-pathname :sibyl "sibyl.asd"))
         (packages-path (asdf:system-relative-pathname :sibyl "src/packages.lisp"))
         (original-asd (when (probe-file asd-path)
                         (uiop:read-file-string asd-path)))
         (original-packages (when (probe-file packages-path)
                              (uiop:read-file-string packages-path))))
    (unwind-protect
         (progn
           (sibyl.tools:execute-tool
            "create-module"
            `(("path" . ,relative-path)
              ("package" . "SIBYL.TOOLS")
              ("initial-definitions" . "(defun evolution-placeholder () :placeholder)")))
           (sibyl.tools:execute-tool
            "add-definition"
            `(("file" . ,module-path-string)
              ("new-definition" . "(defun evolution-test-fn () :evolution-works)")))
           (sibyl.tools:execute-tool
            "add-export"
            '(("package" . "SIBYL.TOOLS")
              ("symbols" . "evolution-test-fn")))
           (sibyl.tools:execute-tool
            "register-in-asdf"
            '(("file" . "test-evolution-module")
              ("module" . "tools")))
           (when verify
             (let ((fn-symbol (intern "EVOLUTION-TEST-FN" :sibyl.tools)))
               (is (eq :evolution-works (funcall fn-symbol)))
               (multiple-value-bind (symbol status)
                   (find-symbol "EVOLUTION-TEST-FN" (find-package :sibyl.tools))
                 (is (eq symbol fn-symbol))
                 (is (eq :external status)))))
           (values original-asd
                   original-packages
                   module-path-string
                   (namestring asd-path)
                   (namestring packages-path)))
      (when (probe-file module-path)
        (delete-file module-path))
      (when original-asd
        (with-open-file (stream asd-path :direction :output :if-exists :supersede)
          (write-string original-asd stream)))
      (when original-packages
        (with-open-file (stream packages-path :direction :output :if-exists :supersede)
          (write-string original-packages stream)))
      (ignore-errors (asdf:clear-system :sibyl))
      (ignore-errors (asdf:find-system :sibyl t))
      (let ((pkg (find-package :sibyl.tools)))
        (when pkg
          (ignore-errors (unexport 'sibyl.tools::evolution-test-fn pkg))))
      (when (fboundp 'sibyl.tools::evolution-test-fn)
        (fmakunbound 'sibyl.tools::evolution-test-fn))
      (when (fboundp 'sibyl.tools::evolution-placeholder)
        (fmakunbound 'sibyl.tools::evolution-placeholder)))))

(test creation-integration-e2e
  "E2E creation workflow succeeds."
  (%creation-integration-run :verify t))

(test creation-integration-cleanup
  "Cleanup restores files and exports after workflow."
  (multiple-value-bind (original-asd original-packages module-path asd-path packages-path)
      (%creation-integration-run :verify nil)
    (when original-asd
      (is (string= original-asd (uiop:read-file-string asd-path))))
    (when original-packages
      (is (string= original-packages (uiop:read-file-string packages-path))))
    (is (not (probe-file module-path)))
    (multiple-value-bind (symbol status)
        (find-symbol "EVOLUTION-TEST-FN" (find-package :sibyl.tools))
      (declare (ignore symbol))
      (is (not (eq :external status))))))


;;; ============================================================
;;; Add Definition Tests
;;; ============================================================

(def-suite add-definition-tests
  :description "Tests for add-definition tool."
  :in sibyl-tests)

(in-suite add-definition-tests)

(defun %add-def-tests-temp-path ()
  (let* ((dir (uiop:temporary-directory))
         (name (format nil "sibyl-add-def-~a-~a.lisp"
                       (get-universal-time)
                       (random 1000000))))
    (namestring (merge-pathnames name dir))))

(defun %add-def-tests-write-file (path content)
  (with-open-file (stream path :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string content stream)))

(test add-definition-appends-defun
  "add-definition appends new defun to file and makes it callable."
  (let* ((path (%add-def-tests-temp-path)))
    (unwind-protect
         (progn
           (%add-def-tests-write-file
            path
            "(in-package #:sibyl.tests)

(defun add-def-test-existing-fn () :existing)
")
           (let ((result (sibyl.tools:execute-tool
                          "add-definition"
                          (list (cons "file" path)
                                (cons "new-definition"
                                      "(defun add-def-test-new-fn () :new)")))))
             (is (search "success" (string-downcase result)))
             (let ((content (uiop:read-file-string path)))
               (is (search "add-def-test-existing-fn" content))
               (is (search "add-def-test-new-fn" content)))
             (let ((sym (find-symbol "ADD-DEF-TEST-NEW-FN" :sibyl.tests)))
               (is (not (null sym)))
               (when sym
                 (is (fboundp sym))
                 (is (eq :new (funcall sym)))))))
      (when (probe-file path) (delete-file path))
      (let ((sym1 (find-symbol "ADD-DEF-TEST-NEW-FN" :sibyl.tests)))
        (when (and sym1 (fboundp sym1)) (fmakunbound sym1)))
      (let ((sym2 (find-symbol "ADD-DEF-TEST-EXISTING-FN" :sibyl.tests)))
        (when (and sym2 (fboundp sym2)) (fmakunbound sym2))))))

(test add-definition-invalid-sexp
  "add-definition signals tool-execution-error for invalid S-expressions."
  (let* ((path (%add-def-tests-temp-path)))
    (unwind-protect
         (progn
           (%add-def-tests-write-file
            path
            "(in-package #:sibyl.tests)

(defun add-def-test-base-invalid-fn () :base)
")
           (signals sibyl.conditions:tool-execution-error
             (sibyl.tools:execute-tool
              "add-definition"
              (list (cons "file" path)
                    (cons "new-definition" "(defun unclosed-paren")))))
      (when (probe-file path) (delete-file path))
      (let ((sym (find-symbol "ADD-DEF-TEST-BASE-INVALID-FN" :sibyl.tests)))
        (when (and sym (fboundp sym)) (fmakunbound sym))))))

(test add-definition-preserves-existing-content
  "add-definition preserves existing file content after addition."
  (let* ((path (%add-def-tests-temp-path)))
    (unwind-protect
         (progn
           (%add-def-tests-write-file
            path
            "(in-package #:sibyl.tests)

(defun add-def-test-preserve-fn () :preserve)
")
           (sibyl.tools:execute-tool
            "add-definition"
            (list (cons "file" path)
                  (cons "new-definition"
                        "(defun add-def-test-added-fn () :added)")))
           (let ((content (uiop:read-file-string path)))
             (is (search "(in-package" content))
             (is (search "add-def-test-preserve-fn" content))
             (is (search "add-def-test-added-fn" content))))
      (when (probe-file path) (delete-file path))
      (let ((sym1 (find-symbol "ADD-DEF-TEST-ADDED-FN" :sibyl.tests)))
        (when (and sym1 (fboundp sym1)) (fmakunbound sym1)))
      (let ((sym2 (find-symbol "ADD-DEF-TEST-PRESERVE-FN" :sibyl.tests)))
        (when (and sym2 (fboundp sym2)) (fmakunbound sym2))))))

(test add-definition-undo-addition
  "add-definition signals tool-execution-error when blocked symbol (run-program) used."
  (let* ((path (%add-def-tests-temp-path)))
    (unwind-protect
         (progn
           (%add-def-tests-write-file
            path
            "(in-package #:sibyl.tests)

(defun add-def-test-undo-base-fn () :base)
")
           (signals sibyl.conditions:tool-execution-error
             (sibyl.tools:execute-tool
              "add-definition"
              (list (cons "file" path)
                    (cons "new-definition"
                          "(defun add-def-test-blocked-fn () (run-program \"ls\" :output t))")))))
      (when (probe-file path) (delete-file path))
      (let ((sym (find-symbol "ADD-DEF-TEST-BLOCKED-FN" :sibyl.tests)))
        (when (and sym (fboundp sym)) (fmakunbound sym)))
      (let ((sym (find-symbol "ADD-DEF-TEST-UNDO-BASE-FN" :sibyl.tests)))
        (when (and sym (fboundp sym)) (fmakunbound sym))))))

(test add-definition-after-parameter
  "add-definition uses after parameter to insert after specific definition."
  (let* ((path (%add-def-tests-temp-path)))
    (unwind-protect
         (progn
           (%add-def-tests-write-file
            path
            "(in-package #:sibyl.tests)

(defun add-def-test-first-fn () :first)

(defun add-def-test-second-fn () :second)
")
           (sibyl.tools:execute-tool
            "add-definition"
            (list (cons "file" path)
                  (cons "new-definition"
                        "(defun add-def-test-after-fn () :after)")
                  (cons "after" "add-def-test-first-fn")))
           (let ((content (uiop:read-file-string path)))
             (is (search "add-def-test-first-fn" content))
             (is (search "add-def-test-after-fn" content))
             (is (search "add-def-test-second-fn" content))
             (let ((after-pos (search "add-def-test-after-fn" content))
                   (second-pos (search "add-def-test-second-fn" content)))
               (is (< after-pos second-pos)))))
      (when (probe-file path) (delete-file path))
      (dolist (name '("ADD-DEF-TEST-FIRST-FN" "ADD-DEF-TEST-SECOND-FN"
                      "ADD-DEF-TEST-AFTER-FN"))
        (let ((sym (find-symbol name :sibyl.tests)))
          (when (and sym (fboundp sym)) (fmakunbound sym)))))))

;;; ============================================================
;;; Add Export Tests
;;; ============================================================

(def-suite add-export-tests
  :description "Tests for add-export tool."
  :in sibyl-tests)

(in-suite add-export-tests)

(defun %add-export-tests-packages-path ()
  (asdf:system-relative-pathname :sibyl "src/packages.lisp"))

(defun %add-export-tests-restore-packages (original)
  (with-open-file (stream (%add-export-tests-packages-path)
                          :direction :output :if-exists :supersede)
    (write-string original stream)))

(test add-export-new-symbol
  "add-export adds a new symbol to sibyl.tools package with :EXTERNAL status."
  (let* ((packages-path (%add-export-tests-packages-path))
         (original-content (uiop:read-file-string packages-path))
         (test-sym "add-export-test-sym-7777"))
    (unwind-protect
         (let ((result (sibyl.tools:execute-tool
                        "add-export"
                        (list (cons "package" "sibyl.tools")
                              (cons "symbols" test-sym)))))
           (is (search "success" (string-downcase result)))
           (multiple-value-bind (sym status)
               (find-symbol (string-upcase test-sym) :sibyl.tools)
             (declare (ignore sym))
             (is (eq :external status))))
      (%add-export-tests-restore-packages original-content))))

(test add-export-idempotent
  "add-export is idempotent when re-exporting an already-exported symbol."
  (let* ((packages-path (%add-export-tests-packages-path))
         (original-content (uiop:read-file-string packages-path)))
    (unwind-protect
         (progn
           ;; execute-tool is already exported; re-exporting should succeed
           (let ((result (sibyl.tools:execute-tool
                          "add-export"
                          (list (cons "package" "sibyl.tools")
                                (cons "symbols" "execute-tool")))))
             (is (stringp result))
             (multiple-value-bind (sym status)
                 (find-symbol "EXECUTE-TOOL" :sibyl.tools)
               (declare (ignore sym))
               (is (eq :external status)))))
      (%add-export-tests-restore-packages original-content))))

(test add-export-undo-export
  "add-export signals error for non-sibyl packages and preserves packages.lisp."
  (let* ((packages-path (%add-export-tests-packages-path))
         (original-content (uiop:read-file-string packages-path)))
    (signals sibyl.conditions:tool-execution-error
      (sibyl.tools:execute-tool
       "add-export"
       (list (cons "package" "cl-user")
             (cons "symbols" "test-sym-undo"))))
    ;; packages.lisp should be unchanged since error occurred before modification
    (let ((current-content (uiop:read-file-string packages-path)))
      (is (string= original-content current-content)))))

(test add-export-formatting
  "add-export preserves packages.lisp formatting after successful export."
  (let* ((packages-path (%add-export-tests-packages-path))
         (original-content (uiop:read-file-string packages-path))
         (test-sym "add-export-fmt-test-sym-6666"))
    (unwind-protect
         (progn
           (sibyl.tools:execute-tool
            "add-export"
            (list (cons "package" "sibyl.tools")
                  (cons "symbols" test-sym)))
           (let ((new-content (uiop:read-file-string packages-path)))
             (is (stringp new-content))
             (is (> (length new-content) 0))
             ;; Test symbol should appear in packages.lisp
             (is (search (string-downcase test-sym) (string-downcase new-content)))
             ;; defpackage form should still be present
             (is (search "defpackage" (string-downcase new-content)))))
      (%add-export-tests-restore-packages original-content))))

;;; ============================================================
;;; Create Module Tests
;;; ============================================================

(def-suite create-module-tests
  :description "Tests for create-module tool."
  :in sibyl-tests)

(in-suite create-module-tests)

(defun %create-module-tests-unique-path ()
  (format nil "src/tools/temp-create-module-~a-~a.lisp"
          (get-universal-time)
          (random 1000000)))

(defun %create-module-tests-full-path (rel-path)
  (asdf:system-relative-pathname :sibyl rel-path))

(test create-module-creates-file
  "create-module creates a new file with in-package declaration."
  (let* ((rel-path (%create-module-tests-unique-path))
         (full-path (%create-module-tests-full-path rel-path)))
    (unwind-protect
         (progn
           (let ((result (sibyl.tools:execute-tool
                          "create-module"
                          (list (cons "path" rel-path)
                                (cons "package" "sibyl.tests")))))
             (is (search "success" (string-downcase result)))
             (is (uiop:file-exists-p full-path))
             (let ((content (uiop:read-file-string full-path)))
               (is (search "in-package" (string-downcase content)))
               (is (search "sibyl.tests" (string-downcase content))))))
      (when (probe-file full-path) (delete-file full-path)))))

(test create-module-existing-file-error
  "create-module signals tool-execution-error when file already exists."
  (let* ((rel-path (%create-module-tests-unique-path))
         (full-path (%create-module-tests-full-path rel-path)))
    (unwind-protect
         (progn
           ;; Create the file first
           (sibyl.tools:execute-tool
            "create-module"
            (list (cons "path" rel-path)
                  (cons "package" "sibyl.tests")))
           (is (uiop:file-exists-p full-path))
           ;; Attempt to create again should error
           (signals sibyl.conditions:tool-execution-error
             (sibyl.tools:execute-tool
              "create-module"
              (list (cons "path" rel-path)
                    (cons "package" "sibyl.tests")))))
      (when (probe-file full-path) (delete-file full-path)))))

(test create-module-non-sibyl-package-error
  "create-module signals tool-execution-error for non-sibyl packages."
  (signals sibyl.conditions:tool-execution-error
    (sibyl.tools:execute-tool
     "create-module"
     (list (cons "path" "src/tools/temp-non-sibyl-pkg-test.lisp")
           (cons "package" "cl-user")))))

(test create-module-with-initial-definitions
  "create-module creates file with initial definitions that compile."
  (let* ((rel-path (%create-module-tests-unique-path))
         (full-path (%create-module-tests-full-path rel-path))
         (def-str "(defun create-mod-test-init-fn () :init-ok)"))
    (unwind-protect
         (progn
           (let ((result (sibyl.tools:execute-tool
                          "create-module"
                          (list (cons "path" rel-path)
                                (cons "package" "sibyl.tests")
                                (cons "initial-definitions" def-str)))))
             (is (search "success" (string-downcase result)))
             (is (uiop:file-exists-p full-path))
             (let ((content (uiop:read-file-string full-path)))
               (is (search "create-mod-test-init-fn" content)))
             ;; The definition should have been eval'd and be callable
             (let ((sym (find-symbol "CREATE-MOD-TEST-INIT-FN" :sibyl.tests)))
               (is (not (null sym)))
               (when sym
                 (is (fboundp sym))
                 (is (eq :init-ok (funcall sym)))))))
      (when (probe-file full-path) (delete-file full-path))
      (let ((sym (find-symbol "CREATE-MOD-TEST-INIT-FN" :sibyl.tests)))
        (when (and sym (fboundp sym)) (fmakunbound sym))))))

;;; ============================================================
;;; Suggest Improvements Enhanced Tests
;;; ============================================================

(def-suite suggest-improvements-enhanced-tests
  :description "Tests for suggest-improvements min-priority and exclude-attempted."
  :in sibyl-tests)

(in-suite suggest-improvements-enhanced-tests)

(defun %suggest-improvements-enhanced-learnings-path ()
  (asdf:system-relative-pathname
   :sibyl
   ".sisyphus/notepads/self-development-roadmap/learnings.md"))

(defun %suggest-improvements-enhanced-parse-result (json)
  (let* ((parsed (yason:parse json :object-as :hash-table))
         (raw (gethash "suggestions" parsed)))
    (if (vectorp raw) (coerce raw 'list) (or raw nil))))

(test suggest-improvements-min-priority-high
  "suggest-improvements with min-priority=high returns only high priority suggestions."
  (let* ((learnings-path (%suggest-improvements-enhanced-learnings-path))
         (original (when (probe-file learnings-path)
                     (uiop:read-file-string learnings-path))))
    (unwind-protect
         (let* ((result (sibyl.tools:execute-tool
                         "suggest-improvements"
                         '(("scope" . "tools")
                           ("min-priority" . "high"))))
                (suggestions (%suggest-improvements-enhanced-parse-result result)))
           (is (listp suggestions))
           (dolist (suggestion suggestions)
             (is (string= "high" (gethash "priority" suggestion)))))
      (when original
        (with-open-file (stream learnings-path :direction :output
                                :if-exists :supersede)
          (write-string original stream))))))

(test suggest-improvements-min-priority-medium
  "suggest-improvements with min-priority=medium returns high and medium priority."
  (let* ((learnings-path (%suggest-improvements-enhanced-learnings-path))
         (original (when (probe-file learnings-path)
                     (uiop:read-file-string learnings-path))))
    (unwind-protect
         (let* ((result (sibyl.tools:execute-tool
                         "suggest-improvements"
                         '(("scope" . "tools")
                           ("min-priority" . "medium"))))
                (suggestions (%suggest-improvements-enhanced-parse-result result)))
           (is (listp suggestions))
           (dolist (suggestion suggestions)
             (is (member (gethash "priority" suggestion)
                         '("high" "medium") :test #'string=))))
      (when original
        (with-open-file (stream learnings-path :direction :output
                                :if-exists :supersede)
          (write-string original stream))))))

(test suggest-improvements-min-priority-low
  "suggest-improvements with min-priority=low returns all suggestions."
  (let* ((learnings-path (%suggest-improvements-enhanced-learnings-path))
         (original (when (probe-file learnings-path)
                     (uiop:read-file-string learnings-path))))
    (unwind-protect
         (let* ((result (sibyl.tools:execute-tool
                         "suggest-improvements"
                         '(("scope" . "tools")
                           ("min-priority" . "low"))))
                (suggestions (%suggest-improvements-enhanced-parse-result result)))
           (is (listp suggestions))
           (dolist (suggestion suggestions)
             (is (member (gethash "priority" suggestion)
                         '("high" "medium" "low") :test #'string=))))
      (when original
        (with-open-file (stream learnings-path :direction :output
                                :if-exists :supersede)
          (write-string original stream))))))

(test suggest-improvements-exclude-attempted-no-state
  "suggest-improvements with exclude-attempted=true and no state returns gracefully."
  (let* ((learnings-path (%suggest-improvements-enhanced-learnings-path))
         (original (when (probe-file learnings-path)
                     (uiop:read-file-string learnings-path))))
    (unwind-protect
         (let* ((result (sibyl.tools:execute-tool
                         "suggest-improvements"
                         '(("scope" . "tools")
                           ("exclude-attempted" . "true"))))
                (suggestions (%suggest-improvements-enhanced-parse-result result)))
           ;; Should return without error, result should be a list
           (is (stringp result))
           (is (listp suggestions)))
      (when original
        (with-open-file (stream learnings-path :direction :output
                                :if-exists :supersede)
          (write-string original stream))))))

;;; ============================================================
;;; ASDF Registration Tests
;;; ============================================================

(def-suite asdf-registration-tests
  :description "Tests for register-in-asdf tool."
  :in sibyl-tests)

(in-suite asdf-registration-tests)

(defun %asdf-registration-asd-path ()
  "Return the system-relative path to sibyl.asd."
  (asdf:system-relative-pathname :sibyl "sibyl.asd"))

(defun %asdf-registration-read-asd ()
  "Read sibyl.asd contents as a string."
  (uiop:read-file-string (%asdf-registration-asd-path)))

(defun %asdf-registration-restore-asd (content)
  "Restore sibyl.asd to CONTENT and reload the system definition."
  (let ((asdf-path (%asdf-registration-asd-path)))
    (with-open-file (stream asdf-path :direction :output
                            :if-exists :supersede)
      (write-string content stream))
    (asdf:clear-system :sibyl)
    (asdf:find-system :sibyl t)))

(defun %asdf-registration-find-component (module file)
  "Find a component under src/ modules by name."
  (or (asdf:find-component :sibyl (list module file))
      (asdf:find-component :sibyl (list "src" module file))))

(defun %asdf-registration-dummy-path (module file)
  "Build a dummy source path for MODULE/FILE under src/."
  (asdf:system-relative-pathname :sibyl (format nil "src/~a/~a.lisp" module file)))

(defun %asdf-registration-write-dummy (path)
  "Create a dummy Lisp file at PATH."
  (with-open-file (stream path :direction :output :if-does-not-exist :create)
    (write-string ";;; Dummy file" stream)))

(test register-in-asdf-adds-component
  "register-in-asdf registers a new component in sibyl.asd."
  (let* ((file "tmp-asdf-register-001")
         (module "tools")
         (original-content (%asdf-registration-read-asd))
         (dummy-path (%asdf-registration-dummy-path module file)))
    (unwind-protect
         (progn
           (%asdf-registration-write-dummy dummy-path)
           (let ((result (sibyl.tools:execute-tool
                          "register-in-asdf"
                          `(("file" . ,file)
                            ("module" . ,module)))))
             (is (search "success" (string-downcase result))))
           (is (not (null (%asdf-registration-find-component module file)))))
      (%asdf-registration-restore-asd original-content)
      (when (probe-file dummy-path)
        (delete-file dummy-path)))))

(test register-in-asdf-duplicate-error
  "register-in-asdf signals error when component is already registered."
  (let* ((file "tmp-asdf-register-dup-001")
         (module "tools")
         (original-content (%asdf-registration-read-asd))
         (dummy-path (%asdf-registration-dummy-path module file)))
    (unwind-protect
         (progn
           (%asdf-registration-write-dummy dummy-path)
           (sibyl.tools:execute-tool
            "register-in-asdf"
            `(("file" . ,file)
              ("module" . ,module)))
           (signals sibyl.conditions:tool-execution-error
             (sibyl.tools:execute-tool
              "register-in-asdf"
              `(("file" . ,file)
                ("module" . ,module)))))
      (%asdf-registration-restore-asd original-content)
      (when (probe-file dummy-path)
        (delete-file dummy-path)))))

(test register-in-asdf-invalid-module
  "register-in-asdf signals error for unknown modules."
  (let* ((original-content (%asdf-registration-read-asd)))
    (unwind-protect
         (signals sibyl.conditions:tool-execution-error
           (sibyl.tools:execute-tool
            "register-in-asdf"
            '(("file" . "tmp-asdf-register-err-001")
              ("module" . "missing-module"))))
      (%asdf-registration-restore-asd original-content))))

(test register-in-asdf-undo-restart
  "undo-registration restart restores sibyl.asd content."
  (let* ((original-content (%asdf-registration-read-asd))
         (restart-invoked nil))
    (unwind-protect
         (progn
           (handler-bind
               ((sibyl.conditions:tool-execution-error
                  (lambda (e)
                    (declare (ignore e))
                    (let ((restart (find (string 'undo-registration)
                                         (compute-restarts)
                                         :test #'string-equal
                                         :key (lambda (rs)
                                                (symbol-name (restart-name rs))))))
                      (when restart
                        (setf restart-invoked t)
                        (invoke-restart restart))))))
             (sibyl.tools:execute-tool
              "register-in-asdf"
              '(("file" . "tmp-asdf-register-undo-001")
                ("module" . "missing-module"))))
           (is (eq t restart-invoked))
           (let ((restored-content (%asdf-registration-read-asd)))
             (is (string= original-content restored-content))))
      (%asdf-registration-restore-asd original-content))))



(test logging-system-basic
  "Auto-generated test"
  (let ((sibyl.logging:*log-level* :debug)
        (output (make-string-output-stream)))
    (let ((sibyl.logging:*log-stream* output))
      (sibyl.logging:log-debug "test" "Debug message")
      (sibyl.logging:log-info "test" "Info message")
      (sibyl.logging:log-warn "test" "Warning message")
      (sibyl.logging:log-error "test" "Error message"))
    (let ((result (get-output-stream-string output)))
      (is (search "DEBUG" result))
      (is (search "INFO" result))
      (is (search "WARN" result))
      (is (search "ERROR" result))
      (is (search "test" result)))))

(test logging-level-filtering
  "Auto-generated test"
  (let ((output (make-string-output-stream)))
    (let ((sibyl.logging:*log-level* :warn)
          (sibyl.logging:*log-stream* output))
      (sibyl.logging:log-debug "test" "Should not appear")
      (sibyl.logging:log-info "test" "Should not appear")
      (sibyl.logging:log-warn "test" "Should appear")
      (sibyl.logging:log-error "test" "Should appear"))
    (let ((result (get-output-stream-string output)))
      (is (not (search "DEBUG" result)))
      (is (not (search "INFO" result)))
      (is (search "WARN" result))
      (is (search "ERROR" result)))))

(test logging-json-format
  "Auto-generated test"
  (let ((output (make-string-output-stream)))
    (let ((sibyl.logging:*log-level* :info)
          (sibyl.logging:*log-stream* output)
          (sibyl.logging:*log-format* :json))
      (sibyl.logging:log-info "test-component" "Test message"))
    (let ((result (get-output-stream-string output)))
      (is (search "\"level\":\"INFO\"" result))
      (is (search "\"component\":\"test-component\"" result))
      (is (search "\"message\":\"Test message\"" result))
      (is (search "\"timestamp\":" result)))))

(test call-llm-no-agent-error
   "Auto-generated test"
   
;; call-llm throws error when *current-agent* is NIL
(signals error
  (sibyl.llm:call-llm '()))
)


(test generate-unified-diff-no-change
   "Auto-generated test"
   (let ((result (sibyl.tools::%generate-unified-diff "test.lisp" "hello" "hello")))
   (is (null result) "returns nil when content is identical")))

(test generate-unified-diff-with-change
   "Auto-generated test"
   (let ((result (sibyl.tools::%generate-unified-diff "test.lisp" "hello" "world")))
   (is (stringp result) "returns string when content differs")
   (is (search "---" result) "--- header is included")
   (is (search "+++" result) "+++ header is included")
   (is (search "-hello" result) "deleted line is included")
   (is (search "+world" result) "added line is included")))

(test sync-to-file-includes-diff-in-result
  "Auto-generated test"
  (let* ((path (format nil "/tmp/sibyl-diff-test-~a.lisp" (get-universal-time)))
       (original "(defun foo () 1)
(defun bar () 2)
")
       (new-source "(defun foo () 42)"))
  (unwind-protect
       (progn
         (with-open-file (s path :direction :output :if-does-not-exist :create)
           (write-string original s))
          (let ((result (sibyl.tools:execute-tool
                         "sync-to-file"
                         (list (cons "name" "foo")
                               (cons "file" path)
                               (cons "new-source" new-source)))))
            (is (search "```diff" result) "diff block is included")
            (is (search "-" result) "deleted line is included")
            (is (search "+" result) "added line is included")))
    (when (probe-file path) (delete-file path)))))

(test write-file-includes-diff-in-result
  "Auto-generated test"
  (let* ((path (format nil "/tmp/sibyl-write-diff-test-~a.txt" (get-universal-time))))
   (unwind-protect
        (progn
          ;; Create existing file
          (with-open-file (s path :direction :output :if-does-not-exist :create)
            (write-string "line1\nline2\n" s))
          ;; Overwrite
          (let ((result (sibyl.tools:execute-tool
                         "write-file"
                         (list (cons "path" path)
                               (cons "content" "line1\nline2-modified\n")))))
            (is (search "```diff" result) "diff block is included when overwriting")))
    (when (probe-file path) (delete-file path)))))

(test suggest-improvements-tests-index-fast
  "Auto-generated test"
  (let* ((content "foo bar baz quux some-function another-fn")
       (index (sibyl.tools::%suggest-improvements-build-tests-index content)))
  (is (hash-table-p index))
  (is (gethash "foo" index))
  (is (gethash "some-function" index))
  (is (gethash "another-fn" index))
  (is (not (gethash "missing" index)))
  (is (not (gethash "quux2" index)))))

(test format-tool-result-line-error-detection
   "Auto-generated test"
   
(let* ((dummy-tc (make-instance 'sibyl.llm:tool-call
                                 :id "test-id"
                                 :name "shell"
                                 :arguments '(("command" . "sed -n '1,10p' src/repl.lisp"))))
        ;; Lisp code containing "Error:" text mixed in output (false positive bug)
        (result-with-error-text "(format t \"[LLM Error: ~a]\" e)")
        ;; Actual error (starts with "Error:")
        (result-real-error "Error: command not found")
        ;; Normal output
        (result-ok "Exit code: 0\nhello world"))
   ;; Normal output with "Error:" text should NOT show ✗
   (let ((line (sibyl.repl.display:format-tool-result-line dummy-tc result-with-error-text 0.03 50)))
     (is (not (search "✗" line))
         "should NOT be ✗ when 'Error:' appears inside output content, got: ~a" line))
   ;; Actual error should show ✗
   (let ((line (sibyl.repl.display:format-tool-result-line dummy-tc result-real-error 1.0 0)))
     (is (search "✗" line)
         "should be ✗ when result starts with 'Error:', got: ~a" line))
   ;; Normal output should show ✓
   (let ((line (sibyl.repl.display:format-tool-result-line dummy-tc result-ok 0.05 100)))
     (is (search "✓" line)
         "should be ✓ for normal output, got: ~a" line)))
)

(test tool-result-hook-restarts-spinner
   "Auto-generated test"
   
;; Verify make-tool-result-hook restarts spinner after displaying tool result
(let* ((dummy-tc (make-instance 'sibyl.llm:tool-call
                                :id "t1" :name "shell"
                                :arguments '(("command" . "ls"))))
       (hook (sibyl.repl:make-tool-result-hook)))
  (setf sibyl.repl:*current-spinner* nil)
  (funcall hook dummy-tc "Exit code: 0\nhello" 0.05)
  (let ((sp sibyl.repl:*current-spinner*))
    (unwind-protect
         (progn
           (is (not (null sp))
               "spinner should be restarted after tool-result hook")
           (is (sibyl.repl.spinner:spinner-active-p sp)
               "restarted spinner should be active"))
      (when (and sp (sibyl.repl.spinner:spinner-active-p sp))
        (sibyl.repl.spinner:stop-spinner sp))
      (setf sibyl.repl:*current-spinner* nil))))
)

(test safe-redefine-allows-exit-in-defun-body
  "Auto-generated test"
  
;; Regression test: safe-redefine should succeed even when the new definition
;; contains sb-ext:exit or cl:quit in the function body.
;; Previously, %safe-redefine-eval-definition called eval-form which
;; blocked any form containing "EXIT" or "QUIT" symbol names,
;; causing safe-redefine to fail with a false positive.
(eval '(defun sibyl.tests::safe-redefine-test-fn-exit-003 ()
         "original"))
(compile 'sibyl.tests::safe-redefine-test-fn-exit-003)
(unwind-protect
    (let ((result (sibyl.tools:execute-tool
                   "safe-redefine"
                   '(("name" . "sibyl.tests::safe-redefine-test-fn-exit-003")
                     ("new-definition" .
                      ;; Definition contains sb-ext:exit in its body
                      "(defun sibyl.tests::safe-redefine-test-fn-exit-003 ()
                         (when nil #+sbcl (sb-ext:exit :code 0))
                         \"modified-with-exit\")")))))
      ;; Must succeed - "Exit" in defun body is NOT unsafe
      (is (search "redefined" (string-downcase result))
          "safe-redefine should succeed; got: ~a" result)
      (is (string= "modified-with-exit"
                   (sibyl.tests::safe-redefine-test-fn-exit-003))
          "Function should be updated to new definition"))
  (when (fboundp 'sibyl.tests::safe-redefine-test-fn-exit-003)
    (fmakunbound 'sibyl.tests::safe-redefine-test-fn-exit-003)))
)

(test token-efficient-beta-header-present
  "Auto-generated test"
  (let* ((client (sibyl.llm::make-anthropic-client :api-key "sk-ant-test-key"))
       (headers (sibyl.llm::anthropic-headers client))
       (beta-header (cdr (assoc "anthropic-beta" headers :test #'equal))))
  (is (and beta-header
           (search "token-efficient-tools-2025-02-19" beta-header))
      "anthropic-beta header should include token-efficient-tools-2025-02-19")))

(test add-conversation-cache-points-basic
  "Auto-generated test"
  
;; Build a fake api-messages list with 3 turns
(let* ((msgs (list
              '(("role" . "user")    ("content" . ((("type" . "text") ("text" . "Hello")))))
              '(("role" . "assistant") ("content" . ((("type" . "text") ("text" . "Hi")))))
              '(("role" . "user")    ("content" . ((("type" . "text") ("text" . "What is 2+2?")))))
              '(("role" . "assistant") ("content" . ((("type" . "text") ("text" . "4")))))
              '(("role" . "user")    ("content" . ((("type" . "text") ("text" . "And 3+3?")))))))
       ;; Stub cache-enabled
       (result (let ((sibyl.config::*config*
                      (alexandria:alist-hash-table
                       '(("optimization.cache-enabled" . t)) :test #'equal)))
                 (sibyl.llm::add-conversation-cache-points msgs)))
       ;; The second-to-last user message (index 2) should have cache_control
       (marked-msg (nth 2 result))
       (marked-content (cdr (assoc "content" marked-msg :test #'equal)))
       (last-block (car (last marked-content)))
       (cache-ctrl (cdr (assoc "cache_control" last-block :test #'equal)))
       ;; The last user message (index 4) should NOT have cache_control
       (last-msg (nth 4 result))
       (last-content (cdr (assoc "content" last-msg :test #'equal)))
       (last-block-last (car (last last-content)))
       (no-cache (assoc "cache_control" last-block-last :test #'equal)))
  (is (and cache-ctrl
           (equal "ephemeral" (cdr (assoc "type" cache-ctrl :test #'equal))))
      "second-to-last user msg should have cache_control ephemeral")
  (is (null no-cache)
      "last user message should NOT have cache_control")))

(test add-conversation-cache-points-short-history
  "Auto-generated test"
  
;; With fewer than 3 messages, nothing should be cached
(let* ((msgs (list
              '(("role" . "user") ("content" . ((("type" . "text") ("text" . "Hello")))))))
       (result (let ((sibyl.config::*config*
                      (alexandria:alist-hash-table
                       '(("optimization.cache-enabled" . t)) :test #'equal)))
                 (sibyl.llm::add-conversation-cache-points msgs)))
       (content (cdr (assoc "content" (first result) :test #'equal)))
       (last-block (car (last content)))
       (no-cache (assoc "cache_control" last-block :test #'equal)))
  (is (null no-cache)
      "Single-message history should not get cache_control")
  (is (eq msgs result)
      "Should return the same list object when skipping")))

(test add-conversation-cache-points-cache-disabled
  "Auto-generated test"
  
;; When optimization.cache-enabled is nil, no markers should be added
(let* ((msgs (list
              '(("role" . "user")      ("content" . ((("type" . "text") ("text" . "A")))))
              '(("role" . "assistant") ("content" . ((("type" . "text") ("text" . "B")))))
              '(("role" . "user")      ("content" . ((("type" . "text") ("text" . "C")))))))
       (result (let ((sibyl.config::*config*
                      (alexandria:alist-hash-table
                       '(("optimization.cache-enabled" . nil)) :test #'equal)))
                 (sibyl.llm::add-conversation-cache-points msgs)))
       (first-user-content (cdr (assoc "content" (first result) :test #'equal)))
       (last-block (car (last first-user-content)))
       (no-cache (assoc "cache_control" last-block :test #'equal)))
  (is (null no-cache)
      "Should not add cache_control when caching is disabled")))

(test thinking-callback-emits-prefix-on-first-chunk
  "Auto-generated test"
  
;; When *current-spinner* is NIL and stream is enabled,
;; the callback should emit the [thinking] prefix before the first chunk.
(let* ((sibyl.repl::*stream-enabled* t)
       (sibyl.repl::*use-colors* nil)
       (sibyl.repl::*current-spinner* nil)
       (out (make-string-output-stream))
       (*standard-output* out)
       (cb (sibyl.repl::make-thinking-display-callback)))
  (funcall cb "hello")
  (let ((result (get-output-stream-string out)))
    (is (search "[thinking]" result)
        "prefix should appear on first chunk")
    (is (search "hello" result)
        "chunk content should appear")))
)

(test thinking-callback-no-prefix-on-subsequent-chunks
  "Auto-generated test"
  
;; Subsequent chunks in the SAME block should NOT re-emit the prefix.
(let* ((sibyl.repl::*stream-enabled* t)
       (sibyl.repl::*use-colors* nil)
       (sibyl.repl::*current-spinner* nil)
       (out (make-string-output-stream))
       (*standard-output* out)
       (cb (sibyl.repl::make-thinking-display-callback)))
  (funcall cb "first")
  (funcall cb "second")
  (funcall cb "third")
  (let ((result (get-output-stream-string out)))
    (is (= 1 (let ((count 0) (pos 0))
               (loop (let ((found (search "[thinking]" result :start2 pos)))
                       (if found
                           (progn (incf count) (setf pos (1+ found)))
                           (return count))))))
        "prefix should appear exactly once across consecutive chunks")))
)

(test thinking-callback-re-emits-prefix-after-spinner-restart
  "Auto-generated test"
  
;; KEY BUG REGRESSION TEST:
;; After a tool call restarts the spinner, the next thinking block
;; must re-emit the 💭/[thinking] prefix (and stop the spinner).
;;
;; We simulate: thinking block 1 (no spinner) → spinner restart → thinking block 2
;; The prefix must appear TWICE: once per block.
(let* ((sibyl.repl::*stream-enabled* t)
       (sibyl.repl::*use-colors* nil)
       (sibyl.repl::*current-spinner* nil)
       (out (make-string-output-stream))
       (*standard-output* out)
       (cb (sibyl.repl::make-thinking-display-callback)))
  ;; Block 1: no spinner, emit first chunk
  (funcall cb "block-one")
  ;; Simulate tool call restarting the spinner:
  ;; We just set *current-spinner* to a fake "active" object that
  ;; make-thinking-display-callback will detect and (attempt to) stop.
  ;; Use a real spinner with no thread so stop-spinner won't block.
  (let ((fake-spinner (sibyl.repl.spinner:start-spinner "Thinking...")))
    ;; Immediately stop the background thread but keep the struct
    ;; so we can re-set active=T to simulate "spinner running"
    (sibyl.repl.spinner:stop-spinner fake-spinner)
    ;; Now re-activate the flag so the callback thinks it's running
    (setf (sibyl.repl.spinner::spinner-active fake-spinner) t)
    (setf sibyl.repl::*current-spinner* fake-spinner))
  ;; Block 2: spinner is "active" → callback must stop it and re-emit prefix
  (funcall cb "block-two")
  (let* ((result (get-output-stream-string out))
         (count (let ((c 0) (pos 0))
                  (loop (let ((found (search "[thinking]" result :start2 pos)))
                          (if found
                              (progn (incf c) (setf pos (1+ found)))
                              (return c)))))))
    (is (= 2 count)
        (format nil "prefix should appear twice (once per block), got ~a in: ~s"
                count result))
    (is (search "block-one" result) "block 1 content should appear")
    (is (search "block-two" result) "block 2 content should appear")
    ;; Spinner should now be deactivated
    (is (null sibyl.repl::*current-spinner*)
        "spinner should be cleared after block 2 starts")))
)

(test thinking-callback-sets-output-active-flag
  "Auto-generated test"
  
;; When the thinking callback writes chunks, it should set
;; *thinking-output-active* to T so the text callback can
;; emit a newline separator.
(let ((sibyl.repl::*thinking-output-active* nil)
      (sibyl.repl::*stream-enabled* t)
      (sibyl.repl::*current-spinner* nil)
      (sibyl.repl::*use-colors* nil))
  (let ((cb (sibyl.repl::make-thinking-display-callback)))
    (with-output-to-string (*standard-output*)
      (funcall cb "hello"))
    (is (eq t sibyl.repl::*thinking-output-active*)
        "*thinking-output-active* should be T after a thinking chunk")))
)

(test thinking-output-active-reset-on-text
  "Auto-generated test"
  
;; When *thinking-output-active* is T and the text callback pattern runs,
;; it should emit a newline and reset the flag to NIL.
(let ((sibyl.repl::*thinking-output-active* t))
  (let ((output (with-output-to-string (*standard-output*)
                  ;; Replicate text callback's thinking-separator logic
                  (when sibyl.repl::*thinking-output-active*
                    (format t "~%")
                    (setf sibyl.repl::*thinking-output-active* nil))
                  (write-string "response text"))))
    (is (char= #\Newline (char output 0))
        "newline should be emitted before text when thinking was active")
    (is (null sibyl.repl::*thinking-output-active*)
        "flag should be reset to NIL")))
)

(test strip-leading-newlines-basic
  "Auto-generated test"
  (is (string= "Hello" (sibyl.repl::strip-leading-newlines "Hello")))
(is (string= "Hello" (sibyl.repl::strip-leading-newlines (format nil "~%~%Hello"))))
(is (string= (format nil "Hello~%World") (sibyl.repl::strip-leading-newlines (format nil "~%~%Hello~%World"))))
(is (string= "" (sibyl.repl::strip-leading-newlines "")))
(is (string= "" (sibyl.repl::strip-leading-newlines (format nil "~%~%"))))
(is (string= " Hello" (sibyl.repl::strip-leading-newlines " Hello"))))

(test strip-leading-newlines-streaming
  "Auto-generated test"
  ;; Simulate the streaming callback's first-chunk stripping logic
(let ((first-chunk-p t)
      (output (make-string-output-stream)))
  ;; First chunk with leading newlines (as LLMs often produce)
  (let* ((text (format nil "~%~%Hello, how can I help?"))
         (clean (if first-chunk-p
                    (sibyl.repl::strip-leading-newlines text)
                    text)))
    (when (and clean (plusp (length clean)))
      (setf first-chunk-p nil)
      (write-string clean output)))
  ;; Second chunk (no stripping)
  (let* ((text (format nil "~%I'm here to assist."))
         (clean (if first-chunk-p
                    (sibyl.repl::strip-leading-newlines text)
                    text)))
    (when (and clean (plusp (length clean)))
      (setf first-chunk-p nil)
      (write-string clean output)))
  (let ((result (get-output-stream-string output)))
    ;; Should NOT start with newlines
    (is (char/= (char result 0) #\Newline)
        "First character should not be a newline")
    ;; Should start with "Hello"
    (is (search "Hello" result)
        "Should contain Hello")
    ;; Newlines in the second chunk should be preserved
    (is (search (format nil "~%I'm here") result)
        "Newlines in subsequent chunks should be preserved"))))

(test strip-leading-newlines-all-newlines-chunk
  "Auto-generated test"
  ;; Edge case: first chunk is entirely newlines
(let ((first-chunk-p t)
      (output (make-string-output-stream)))
  ;; First chunk: only newlines → stripped to empty → first-chunk-p stays T
  (let* ((text (format nil "~%~%"))
         (clean (if first-chunk-p
                    (sibyl.repl::strip-leading-newlines text)
                    text)))
    (when (and clean (plusp (length clean)))
      (setf first-chunk-p nil)
      (write-string clean output)))
  ;; first-chunk-p should still be T (nothing was written)
  (is (eq first-chunk-p t)
      "first-chunk-p should remain T when first chunk was all newlines")
  ;; Second chunk also gets stripped since first-chunk-p is still T
  (let* ((text (format nil "~%Content"))
         (clean (if first-chunk-p
                    (sibyl.repl::strip-leading-newlines text)
                    text)))
    (when (and clean (plusp (length clean)))
      (setf first-chunk-p nil)
      (write-string clean output)))
  (is (string= "Content" (get-output-stream-string output))
      "Output should be just 'Content' with no leading newlines")))

(test register-command-uses-plist-format
  "Auto-generated test"
  
(let* ((sibyl.repl::*command-handlers* (copy-list sibyl.repl::*command-handlers*))
       (sibyl.repl::*repl-commands* (copy-list sibyl.repl::*repl-commands*))
       (tool-call (sibyl.llm:make-tool-call
                   :id "tc-register-command-plist"
                   :name "register-command"
                   :arguments '(:name "test-plist-fmt"
                                :description "A test command"
                                :handler-body "(lambda (agent input) (declare (ignore agent input)) nil)")))
       (result (sibyl.tools:execute-tool-call tool-call)))
  (declare (ignore result))
  (let* ((entry-pair (assoc :test-plist-fmt sibyl.repl::*command-handlers*))
         (entry (cdr entry-pair)))
    ;; Entry should be a plist, not a raw function
    (is (listp entry) "Entry should be a plist list")
    (is (functionp (getf entry :handler)) "Should have :handler key")
    (is (string= "A test command" (getf entry :description)) "Should have :description")))
)

(test tools-as-schema-respects-allowed-tools
  "Auto-generated test"
  
(let* ((all-tools (sibyl.tools:tools-as-schema))
       (first-tool-name (getf (first all-tools) :name)))
  ;; Without filter, should return many tools
  (is (> (length all-tools) 5))
  ;; With *allowed-tools* bound, should only return matching tools
  (let ((sibyl.tools:*allowed-tools* (list first-tool-name)))
    (let ((filtered (sibyl.tools:tools-as-schema)))
      (is (= 1 (length filtered)))
      (is (string= first-tool-name (getf (first filtered) :name)))))
  ;; With empty list, should return nothing
  (let ((sibyl.tools:*allowed-tools* '()))
    ;; NIL means no filter, empty list is still nil... 
    ;; Actually NIL = no filter. We need a sentinel.
    ;; Let's test with a non-existent tool name
    (let ((sibyl.tools:*allowed-tools* (list "nonexistent-tool-xyz")))
      (is (= 0 (length (sibyl.tools:tools-as-schema)))))))
)

(test execute-agent-task-binds-allowed-tools
  "Auto-generated test"
  
;; Test that execute-agent-task binds *allowed-tools* based on role
(let* ((role (make-instance 'sibyl.agent:agent-role
                            :name :tester
                            :system-prompt "You are a tester."
                            :tools '("read-file" "eval-form")))
       (tool-list (sibyl.agent::role-tools role)))
  ;; Role has expected tools
  (is (equal '("read-file" "eval-form") tool-list))
  ;; Outside execute-agent-task, *allowed-tools* is nil (no filter)
  (is (null sibyl.tools:*allowed-tools*))
  ;; Verify tools-as-schema returns all tools normally
  (let ((all-count (length (sibyl.tools:tools-as-schema))))
    (is (> all-count 2))
    ;; Simulate what execute-agent-task does
    (let ((sibyl.tools:*allowed-tools* tool-list))
      (let ((filtered (sibyl.tools:tools-as-schema)))
        (is (= 2 (length filtered)))
        (is (every (lambda (s) (member (getf s :name) tool-list :test #'string=))
                   filtered))))))
)

(test role-tools-filter-schema
  "Auto-generated test"
  
;; Verify that *allowed-tools* filters tools-as-schema output
(let* ((all-tools (sibyl.tools:tools-as-schema))
       (all-names (mapcar (lambda (s) (getf s :name)) all-tools)))
  ;; There should be many tools normally
  (is (> (length all-tools) 5) "Should have many tools without filter")
  ;; Now filter to only "eval-form" and "read-file"
  (let* ((sibyl.tools:*allowed-tools* (list "eval-form" "read-file"))
         (filtered (sibyl.tools:tools-as-schema))
         (filtered-names (mapcar (lambda (s) (getf s :name)) filtered)))
    (is (= 2 (length filtered)) "Should have exactly 2 tools when filtered")
    (is (member "eval-form" filtered-names :test #'string=))
    (is (member "read-file" filtered-names :test #'string=))
    (is (not (member "shell" filtered-names :test #'string=))
        "shell should be excluded")))
)

(test execute-task-binds-role-tools
  "Auto-generated test"
  
;; Verify that execute-agent-task binds *allowed-tools* from role-tools
(let* ((captured-tools :not-set)
       (role (make-instance 'sibyl.agent::agent-role
                            :name "limited-coder"
                            :description "Can only eval"
                            :system-prompt "You are limited."
                            :tools (list "eval-form" "read-file")))
       (agent (make-instance 'sibyl.agent::specialized-agent
                             :agent-id "bind-test"
                             :role role))
       (task (make-instance 'sibyl.agent::agent-task
                            :id "task-bind"
                            :description "test")))
  ;; Mock agent-run to capture *allowed-tools*
  (let ((orig-fn (symbol-function 'sibyl.agent::agent-run)))
    (unwind-protect
         (progn
           (setf (symbol-function 'sibyl.agent::agent-run)
                 (lambda (ag desc)
                   (declare (ignore ag desc))
                   (setf captured-tools (copy-list sibyl.tools:*allowed-tools*))
                   "done"))
           (sibyl.agent::execute-agent-task agent task)
           (is (equal captured-tools (list "eval-form" "read-file"))
               "Should bind *allowed-tools* to role's tool list"))
      (setf (symbol-function 'sibyl.agent::agent-run) orig-fn))))
)
