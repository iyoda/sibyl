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

(test grep-exclude-dir-test
  "Grep excludes matching directories from results."
  (let* ((test-dir (pathname "/tmp/grep-test-dir/"))
         (git-dir (merge-pathnames ".git/" test-dir))
         (src-dir (merge-pathnames "src/" test-dir)))
    (unwind-protect
         (progn
           ;; Setup test directory structure
           (ensure-directories-exist git-dir)
           (ensure-directories-exist src-dir)

           ;; Create test files
           (with-open-file (s (merge-pathnames "config" git-dir)
                              :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
             (write-string "test-pattern-in-git" s))
           (with-open-file (s (merge-pathnames "main.lisp" src-dir)
                              :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
             (write-string "test-pattern-in-src" s))

           ;; Test without exclude-dir - should find both
           (let ((result-without-exclude
                   (sibyl.tools:execute-tool "grep"
                                            (list :pattern "test-pattern"
                                                  :path (namestring test-dir)))))
             (is (search ".git/config" result-without-exclude))
             (is (search "src/main.lisp" result-without-exclude)))

           ;; Test with exclude-dir - should exclude .git
           (let ((result-with-exclude
                   (sibyl.tools:execute-tool "grep"
                                            (list :pattern "test-pattern"
                                                  :path (namestring test-dir)
                                                  :exclude-dir ".git"))))
             (is (not (search ".git/config" result-with-exclude)))
             (is (search "src/main.lisp" result-with-exclude))))
      ;; Cleanup
      (when (uiop:directory-exists-p test-dir)
        (uiop:delete-directory-tree test-dir :validate t)))))
