;;;; tools-test.lisp — Tests for the tool system

(in-package #:sibyl.tests)

(in-suite sibyl-tests)

(test tool-registration
  "Tools can be registered and found."
  (let ((sibyl.tools:*tool-registry* (make-hash-table :test 'equal)))
    ;; Register a test tool
    (sibyl.tools:deftool "test-echo"
        (:description "Echo input back"
         :parameters ((:name "text" :type "string" :required t
                       :description "Text to echo")))
      (getf args :text))

    ;; Find it
    (let ((tool (sibyl.tools:find-tool "test-echo")))
      (is (not (null tool)))
      (is (string= "test-echo" (sibyl.tools:tool-name tool)))
      (is (string= "Echo input back" (sibyl.tools:tool-description tool))))))

(test tool-execution
  "Tools can be executed with arguments."
  (let ((sibyl.tools:*tool-registry* (make-hash-table :test 'equal)))
    (sibyl.tools:deftool "test-add"
        (:description "Add two numbers"
         :parameters ((:name "a" :type "number" :required t
                       :description "First number")
                      (:name "b" :type "number" :required t
                       :description "Second number")))
      (+ (getf args :a) (getf args :b)))

    (let ((result (sibyl.tools:execute-tool
                   "test-add"
                   '(:a 2 :b 3))))
      ;; Result is JSON-encoded (number becomes "5")
      (is (string= "5" result)))))

(test tool-not-found
  "Executing a missing tool signals tool-not-found-error."
  (let ((sibyl.tools:*tool-registry* (make-hash-table :test 'equal)))
    (signals sibyl.conditions:tool-not-found-error
      (sibyl.tools:execute-tool "nonexistent" nil))))

(test tool-schema-generation
  "Tools generate correct schema for LLM API."
  (let ((sibyl.tools:*tool-registry* (make-hash-table :test 'equal)))
    (sibyl.tools:deftool "test-schema"
        (:description "Test tool for schema"
         :parameters ((:name "path" :type "string" :required t
                       :description "A path")))
      (declare (ignore args))
      "ok")

    (let* ((schemas (sibyl.tools:tools-as-schema))
           (schema (first schemas)))
      (is (= 1 (length schemas)))
      (is (string= "test-schema" (getf schema :name)))
      (is (string= "Test tool for schema" (getf schema :description))))))
