;;;; creation-tools-test.lisp — Tests for creation tools
;;;; (creation-integration, add-definition, add-export, create-module, suggest-improvements-enhanced, asdf-registration)

(in-package #:sibyl.tests)

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

(test parallel-runner-no-double-execution
  "run-tests-parallel executes safe+unsafe phases only (no full-suite re-run)."
  ;; When run inside the full suite, *run-tests-parallel-running* is already T
  ;; and run-tests-parallel returns early to prevent recursive re-entry.
  ;; In that case, skip this test — it can only meaningfully run standalone.
  (if sibyl.tests::*run-tests-parallel-running*
      (pass "Skipped: already inside run-tests-parallel (recursion guard active)")
      ;; Standalone execution: verify %collect-fiveam-results is called
      ;; exactly (safe + unsafe) times, not (safe + unsafe + 1) for full suite.
      (let ((call-count 0)
            (original-fn (symbol-function 'sibyl.tests::%collect-fiveam-results)))
        (unwind-protect
             (progn
               (setf (symbol-function 'sibyl.tests::%collect-fiveam-results)
                     (lambda (suite)
                       (incf call-count)
                       (funcall original-fn suite)))
                (sibyl.tests::run-tests-parallel)
                ;; Only unsafe suites call %collect-fiveam-results
                ;; (safe suites use %run-suite-isolated in parallel threads)
                (let ((unsafe-count (length (sibyl.tests::%unsafe-suites-resolved))))
                  (is (= call-count unsafe-count)
                      (format nil "Expected ~a unsafe suite calls, got ~a. Full-suite re-run detected!"
                              unsafe-count call-count))))
          (setf (symbol-function 'sibyl.tests::%collect-fiveam-results) original-fn)))))
