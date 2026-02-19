;;;; analysis-tools.lisp — Self-analysis tools: suggest-improvements, self-assess,
;;;;                        improvement-plan, run-tests, write-test

(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package #:sibyl.tools))

;;; ============================================================
;;; Self-analysis: suggest-improvements
;;; ============================================================

(defun %suggest-improvements-normalize-scope (scope)
  "Normalize SCOPE string, defaulting to \"all\"."
  (let ((value (string-downcase (string-trim-whitespace (or scope "")))))
    (if (string= value "") "all" value)))

(defun %suggest-improvements-ensure-list (value)
  (%tools-ensure-list value))

(defun %suggest-improvements-parse-json (json)
  (%tools-parse-json json))

(defun %suggest-improvements-parse-sexp-result (json)
  (let ((parsed (%suggest-improvements-parse-json json)))
    (cond
      ((vectorp parsed) (coerce parsed 'list))
      ((hash-table-p parsed) (list parsed))
      (t parsed))))

(defun %suggest-improvements-map-modules (map)
  (%suggest-improvements-ensure-list (gethash "modules" map)))

(defun %suggest-improvements-module-name (module)
  (gethash "name" module))

(defun %suggest-improvements-module-files (module)
  (%suggest-improvements-ensure-list (gethash "files" module)))

(defun %suggest-improvements-file-path (file)
  (gethash "path" file))

(defun %suggest-improvements-resolve-path (path)
  (let ((pathname (pathname path)))
    (if (uiop:absolute-pathname-p pathname)
        (namestring pathname)
        (namestring (asdf:system-relative-pathname :sibyl path)))))

(defun %suggest-improvements-file-match-p (scope path)
  (let* ((scope-lower (string-downcase scope))
         (path-lower (string-downcase path))
         (absolute (ignore-errors (%suggest-improvements-resolve-path path)))
         (absolute-lower (and absolute (string-downcase absolute)))
         (file-name (string-downcase (file-namestring (pathname path)))))
    (or (string= scope-lower path-lower)
        (and absolute-lower (string= scope-lower absolute-lower))
        (string= scope-lower file-name)
        (and (> (length path-lower) (length scope-lower))
             (string= scope-lower
                      (subseq path-lower (- (length path-lower)
                                            (length scope-lower)))))
        (and absolute-lower
             (> (length absolute-lower) (length scope-lower))
             (string= scope-lower
                      (subseq absolute-lower (- (length absolute-lower)
                                                (length scope-lower))))))))

(defun %suggest-improvements-collect-scope-files (map scope)
  "Collect file paths from MAP based on SCOPE."
  (let* ((normalized (%suggest-improvements-normalize-scope scope))
         (modules (%suggest-improvements-map-modules map)))
    (cond
      ((string= normalized "all")
       (let ((paths nil))
         (dolist (module modules (nreverse paths))
           (dolist (file (%suggest-improvements-module-files module))
             (let ((path (%suggest-improvements-file-path file)))
               (when path
                 (push path paths)))))))
      (t
       (let* ((module (find normalized modules
                            :key (lambda (m)
                                   (string-downcase (or (%suggest-improvements-module-name m) "")))
                            :test #'string=)))
         (cond
           (module
            (let ((paths nil))
              (dolist (file (%suggest-improvements-module-files module) (nreverse paths))
                (let ((path (%suggest-improvements-file-path file)))
                  (when path
                    (push path paths))))))
           (t
            (let ((matched-path nil))
              (dolist (module modules)
                (dolist (file (%suggest-improvements-module-files module))
                  (let ((path (%suggest-improvements-file-path file)))
                    (when (and path
                               (%suggest-improvements-file-match-p normalized path))
                      (setf matched-path path)))))
              (if matched-path
                  (list matched-path)
                  (error "Unknown scope: ~a" scope))))))))))

(defun %suggest-improvements-read-sexp (path)
  (let* ((result (execute-tool "read-sexp"
                               (list (cons "path" path))))
         (entries (%suggest-improvements-parse-sexp-result result)))
    (%suggest-improvements-ensure-list entries)))

(defun %suggest-improvements-collect-definitions (files)
  "Collect definition metadata from FILES using read-sexp."
  (let ((definitions nil))
    (dolist (path files (nreverse definitions))
      (handler-case
          (let* ((resolved (%suggest-improvements-resolve-path path))
                 (entries (%suggest-improvements-read-sexp resolved)))
            (dolist (entry entries)
              (push (list :file path
                          :type (gethash "type" entry)
                          :name (gethash "name" entry)
                          :start-line (gethash "start_line" entry)
                          :form (gethash "form" entry))
                    definitions)))
        (error () nil)))))

(defun %suggest-improvements-public-name-p (name)
  (and (stringp name)
       (string/= name "")
       (not (char= (char name 0) #\%))))

(defun %suggest-improvements-build-tests-index (tests-content)
  "Build a hash-set of all whitespace/punctuation-delimited tokens in TESTS-CONTENT.
Tokens are already lowercased (tests-content is pre-lowercased by the caller).
Returns a hash-table with equal test for O(1) membership checks, replacing
the O(n) repeated (search key tests-content) calls."
  (let ((index (make-hash-table :test 'equal))
        (len (length tests-content))
        (start nil))
    (flet ((flush (end)
             (when (and start (> end start))
               (setf (gethash (subseq tests-content start end) index) t)
               (setf start nil))))
      (dotimes (i len)
        (let ((c (char tests-content i)))
          (if (or (char= c #\Space) (char= c #\Newline) (char= c #\Tab)
                  (char= c #\Return) (char= c #\() (char= c #\))
                  (char= c #\") (char= c #\') (char= c #\,)
                  (char= c #\;))
              (flush i)
              (unless start (setf start i)))))
      (flush len))
    index))

(defun %suggest-improvements-parse-form (form-string)
  (let ((*read-eval* nil))
    (handler-case
        (read-from-string form-string)
      (error () nil))))

(defun %suggest-improvements-docstring-present-p (form)
  (when (consp form)
    (let ((head (string-downcase (symbol-name (first form)))))
      (when (member head '("defun" "defmethod") :test #'string=)
        (let* ((tail (rest form))
               (lambda-pos (position-if #'listp tail)))
          (when lambda-pos
            (let ((candidate (nth (1+ lambda-pos) tail)))
              (stringp candidate))))))))

(defun %suggest-improvements-form-contains-symbol-p (form names)
  (let ((targets (mapcar #'string-downcase names)))
    (labels ((walk (node)
               (cond
                 ((symbolp node)
                  (member (string-downcase (symbol-name node))
                          targets
                          :test #'string=))
                 ((consp node)
                  (or (walk (car node))
                      (walk (cdr node))))
                 (t nil))))
      (walk form))))

(defun %suggest-improvements-form-has-error-handling-p (form)
  (%suggest-improvements-form-contains-symbol-p
   form
   '("handler-case" "ignore-errors" "handler-bind" "restart-case")))

(defun %suggest-improvements-form-has-risky-ops-p (form)
  (%suggest-improvements-form-contains-symbol-p
   form
   '("with-open-file" "read-file-string" "write-file-string"
     "run-program" "execute-tool")))

(defun %suggest-improvements-package-for-file (file-path)
  (let ((lower (string-downcase file-path)))
    (cond
      ((search "src/tools/" lower) "SIBYL.TOOLS")
      ((search "src/agent/" lower) "SIBYL.AGENT")
      ((search "src/llm/" lower) "SIBYL.LLM")
      ((search "src/repl/" lower) "SIBYL.REPL")
      ((search "src/util/" lower) "SIBYL.UTIL")
      ((search "src/conditions/" lower) "SIBYL.CONDITIONS")
      ((search "src/config/" lower) "SIBYL.CONFIG")
      ((search "src/system/" lower) "SIBYL.SYSTEM")
      (t "SIBYL"))))

(defun %suggest-improvements-callers-count (function-string)
  "Return caller count from who-calls output, or NIL."
  (when (find-tool "who-calls")
    (handler-case
        (let* ((result (execute-tool "who-calls"
                                     (list (cons "function" function-string))))
               (lower (string-downcase result))
               (marker "callers ("))
          (cond
            ((search marker lower)
             (let* ((start (search marker lower))
                    (from (+ start (length marker)))
                    (end (position #\) lower :start from)))
               (when end
                 (parse-integer (subseq lower from end)
                                :junk-allowed t))))
            ((search "none found" lower) 0)
            ((search "not found" lower) 0)
            (t nil)))
      (error () nil))))

(defun %suggest-improvements-make-suggestion (category priority description
                                              file line rationale)
  (let ((entry (make-hash-table :test 'equal)))
    (setf (gethash "priority" entry) priority)
    (setf (gethash "category" entry) category)
    (setf (gethash "description" entry) description)
    (setf (gethash "file" entry) file)
    (setf (gethash "line" entry) (or line 0))
    (setf (gethash "rationale" entry) rationale)
    entry))

(defun %suggest-improvements-suggestion-plist (suggestion)
  "Convert suggestion hash table to plist for store-suggestions."
  (list :id (gethash "id" suggestion)
        :description (gethash "description" suggestion)
        :rationale (gethash "rationale" suggestion)
        :priority (gethash "priority" suggestion)
        :category (gethash "category" suggestion)
        :file (gethash "file" suggestion)
        :line (gethash "line" suggestion)))

(defun %suggest-improvements-split-lines (content)
  (%tools-split-lines content))

(defun %suggest-improvements-read-tests-content ()
  (let* ((tests-dir (asdf:system-relative-pathname :sibyl "tests/"))
         (files (and (probe-file tests-dir)
                     (%codebase-map-find-lisp-files tests-dir))))
    (string-downcase
     (string-join (string #\Newline)
                  (mapcar (lambda (path)
                            (uiop:read-file-string path))
                          files)))))

(defun %suggest-improvements-missing-docstrings (definitions)
  (let ((suggestions nil)
        (count 0))
    (dolist (definition definitions (nreverse suggestions))
      (let* ((type (getf definition :type))
             (name (getf definition :name))
             (form-string (getf definition :form))
             (form (%suggest-improvements-parse-form form-string)))
        (when (and (< count 3)
                   (member type '("defun" "defmethod") :test #'string=)
                   (%suggest-improvements-public-name-p name)
                   form
                   (not (%suggest-improvements-docstring-present-p form)))
          (incf count)
          (push (%suggest-improvements-make-suggestion
                 "missing-docstrings"
                 "medium"
                 (format nil "Function \"~a\" lacks a docstring." name)
                 (getf definition :file)
                 (getf definition :start-line)
                 "Public functions should document intent for maintainability and self-analysis.")
                 suggestions))))))

(defun %suggest-improvements-missing-tests
       (definitions tests-content baseline-callers)
  ;; Build a one-time O(n) token index instead of O(n) per-symbol search calls.
  (let ((suggestions nil)
        (seen (make-hash-table :test 'equal))
        (tests-index (%suggest-improvements-build-tests-index tests-content))
        (tool-count 0)
        (function-count 0))
    (dolist (definition definitions (nreverse suggestions))
      (let* ((type (getf definition :type)) (name (getf definition :name)))
        (when (and name (stringp name) (string/= name ""))
          (let ((key (string-downcase name)))
            (unless (gethash key seen)
              (setf (gethash key seen) t)
              (when (not (gethash key tests-index))
                (cond
                 ((and (string= type "deftool") (< tool-count 3))
                  (incf tool-count)
                  (push
                   (%suggest-improvements-make-suggestion "test-coverage"
                    (if (and baseline-callers (> baseline-callers 0))
                        "high"
                        "medium")
                    (format nil "Tool \"~a\" lacks explicit test coverage."
                            name)
                    (getf definition :file) (getf definition :start-line)
                    "User-facing tools should have regression tests to avoid behavioral drift.")
                   suggestions))
                 ((and (member type '("defun" "defmethod") :test #'string=)
                       (%suggest-improvements-public-name-p name)
                       (< function-count 2))
                  (incf function-count)
                  (let* ((package
                          (%suggest-improvements-package-for-file
                           (getf definition :file)))
                         (qualified
                          (format nil "~a::~a" (string-downcase package) name))
                         (callers
                          (%suggest-improvements-callers-count qualified))
                         (priority
                          (if (and callers (> callers 0))
                              "high"
                              "medium"))
                         (rationale
                          (if (and callers (> callers 0))
                              (format nil
                                      "Function has ~a internal callers; missing tests increase regression risk."
                                      callers)
                              "Public functions benefit from explicit tests to lock in expectations.")))
                    (push
                     (%suggest-improvements-make-suggestion "test-coverage"
                      priority
                      (format nil "Function \"~a\" appears untested." name)
                      (getf definition :file) (getf definition :start-line)
                      rationale)
                     suggestions))))))))))))

(defun %suggest-improvements-todo-comments (files)
  (let ((suggestions nil)
        (count 0))
    (dolist (path files (nreverse suggestions))
      (when (< count 3)
        (handler-case
            (let* ((resolved (%suggest-improvements-resolve-path path))
                   (content (uiop:read-file-string resolved))
                   (lines (%suggest-improvements-split-lines content))
                   (line-number 0))
              (dolist (line lines)
                (incf line-number)
                (when (and (< count 3)
                           (let ((lower (string-downcase line)))
                             (or (search "todo" lower)
                                 (search "fixme" lower))))
                  (incf count)
                  (push (%suggest-improvements-make-suggestion
                         "todo"
                         "medium"
                         (format nil "TODO comment: ~a"
                                 (string-trim-whitespace line))
                         path
                         line-number
                         "Outstanding TODOs should be triaged or resolved to reduce uncertainty.")
                         suggestions))))
          (error () nil))))))

(defun %suggest-improvements-error-handling (definitions)
  (let ((suggestions nil)
        (count 0))
    (dolist (definition definitions (nreverse suggestions))
      (let* ((type (getf definition :type))
             (name (getf definition :name))
             (form-string (getf definition :form))
             (form (%suggest-improvements-parse-form form-string)))
        (when (and (< count 2)
                   (string= type "defun")
                   (%suggest-improvements-public-name-p name)
                   form
                   (%suggest-improvements-form-has-risky-ops-p form)
                   (not (%suggest-improvements-form-has-error-handling-p form)))
          (incf count)
          (let* ((package (%suggest-improvements-package-for-file
                           (getf definition :file)))
                 (qualified (format nil "~a::~a"
                                    (string-downcase package)
                                    name))
                 (callers (%suggest-improvements-callers-count qualified))
                 (priority (if (and callers (> callers 0))
                               "high"
                               "medium"))
                 (rationale (if (and callers (> callers 0))
                                (format nil "Function has ~a internal callers; missing error handling risks cascading failures."
                                        callers)
                                "I/O and tool calls should handle failures to avoid cascading errors.")))
            (push (%suggest-improvements-make-suggestion
                   "error-handling"
                   priority
                   (format nil "Function \"~a\" performs risky operations without error handling." name)
                   (getf definition :file)
                   (getf definition :start-line)
                   rationale)
                   suggestions)))))))

(defun %suggest-improvements-duplications (definitions)
  (let ((table (make-hash-table :test 'equal))
        (suggestions nil)
        (count 0))
    (dolist (definition definitions)
      (let ((form (getf definition :form)))
        (when (and form (stringp form))
          (let ((key (string-downcase (string-trim-whitespace form))))
            (push definition (gethash key table))))))
    (maphash
     (lambda (key defs)
       (declare (ignore key))
       (when (< count 2)
         (let* ((paths (remove-duplicates
                        (mapcar (lambda (def) (getf def :file)) defs)
                        :test #'string=))
                (name (getf (first defs) :name)))
           (when (> (length paths) 1)
             (incf count)
             (push (%suggest-improvements-make-suggestion
                    "code-duplication"
                    "low"
                    (format nil "Identical definition~@[ for \"~a\"~] appears in: ~{~a~^, ~}."
                            name paths)
                    (getf (first defs) :file)
                    (getf (first defs) :start-line)
                    "Consolidating duplicates reduces drift and simplifies maintenance.")
                   suggestions)))))
     table)
    (nreverse suggestions)))

(defun %suggest-improvements-priority-rank (priority)
  (cond
    ((string= priority "high") 0)
    ((string= priority "medium") 1)
    (t 2)))

(defun %suggest-improvements-sort (suggestions)
  (sort (copy-list suggestions)
        (lambda (a b)
          (let ((rank-a (%suggest-improvements-priority-rank
                         (gethash "priority" a)))
                (rank-b (%suggest-improvements-priority-rank
                         (gethash "priority" b))))
            (if (/= rank-a rank-b)
                (< rank-a rank-b)
                (string< (gethash "category" a)
                         (gethash "category" b)))))))

(defun %suggest-improvements-dedupe (suggestions)
  (let ((seen (make-hash-table :test 'equal))
        (result nil))
    (dolist (suggestion suggestions (nreverse result))
      (let ((key (format nil "~a|~a|~a"
                         (gethash "category" suggestion)
                         (gethash "file" suggestion)
                         (gethash "line" suggestion))))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push suggestion result))))))

(defun %suggest-improvements-limit (suggestions max-count)
  (let ((count (length suggestions)))
    (if (and max-count (> count max-count))
        (subseq suggestions 0 max-count)
        suggestions)))

(defun %suggest-improvements-assign-ids (suggestions)
  (loop for suggestion in suggestions
        for idx from 1
        do (setf (gethash "id" suggestion) idx))
  suggestions)

(defun %suggest-improvements-filter-by-priority (suggestions min-priority)
  "Filter SUGGESTIONS to only include those at or above MIN-PRIORITY level.
   Priority order: high (0) > medium (1) > low (2).
   If MIN-PRIORITY is nil or \"low\", all suggestions are returned."
  (let ((min-rank (%suggest-improvements-priority-rank (or min-priority "low"))))
    (remove-if (lambda (s)
                 (> (%suggest-improvements-priority-rank
                     (gethash "priority" s))
                    min-rank))
               suggestions)))

(defun %suggest-improvements-filter-attempted (suggestions)
  "Return SUGGESTIONS unchanged (evolution tracking removed)."
  suggestions)

(defun %suggest-improvements-build-note (scope suggestions)
  (with-output-to-string (stream)
    (format stream "## [~a] suggest-improvements~%~%" (timestamp-now))
    (format stream "Scope: ~a~%~%" scope)
    (if (null suggestions)
        (progn
          (write-string "Suggestions: none" stream)
          (terpri stream)
          (terpri stream))
        (progn
          (write-string "Suggestions:" stream)
          (terpri stream)
          (dolist (suggestion suggestions)
            (let ((line (gethash "line" suggestion)))
              (format stream "- [~a][~a] ~a (~a:~a)~%"
                      (gethash "priority" suggestion)
                      (gethash "category" suggestion)
                      (gethash "description" suggestion)
                      (gethash "file" suggestion)
                      (if (integerp line) line 0)))
            (let ((rationale (gethash "rationale" suggestion)))
              (when rationale
                (format stream "  - Rationale: ~a~%" rationale))))
          (terpri stream)))))

(defun %suggest-improvements-append-learnings (scope suggestions)
  (let* ((path (asdf:system-relative-pathname
                :sibyl
                ".sisyphus/notepads/self-development-roadmap/learnings.md"))
         (note (%suggest-improvements-build-note scope suggestions)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (write-string note stream))))

(deftool "suggest-improvements"
    (:description "Analyze the codebase and suggest improvements."
     :category :analysis
     :parameters ((:name "scope" :type "string" :required nil
                   :description "Scope: all, module-name, or file-path")
                  (:name "min-priority" :type "string" :required nil
                   :description "Minimum priority to include: high, medium, or low (default: low)")))
  (block suggest-improvements
    (let* ((scope (getf args :scope))
           (min-priority (getf args :min-priority))
           (normalized-scope (%suggest-improvements-normalize-scope scope))
           (map-result (execute-tool "codebase-map"
                                     '(("detail-level" . "summary"))))
           (map (%suggest-improvements-parse-json map-result))
           (files (%suggest-improvements-collect-scope-files map normalized-scope))
           (definitions (%suggest-improvements-collect-definitions files))
           (tests-content (%suggest-improvements-read-tests-content))
           (baseline-callers (%suggest-improvements-callers-count
                               "sibyl.tools:execute-tool-call"))
           (suggestions nil))
      (setf suggestions
            (append (%suggest-improvements-missing-tests
                     definitions tests-content baseline-callers)
                    (%suggest-improvements-missing-docstrings definitions)
                    (%suggest-improvements-todo-comments files)
                    (%suggest-improvements-error-handling definitions)
                    (%suggest-improvements-duplications definitions)))
      (setf suggestions (%suggest-improvements-dedupe suggestions))
      (setf suggestions (%suggest-improvements-sort suggestions))
      (setf suggestions (%suggest-improvements-filter-by-priority suggestions min-priority))
      (setf suggestions (%suggest-improvements-limit suggestions 10))
      (setf suggestions (%suggest-improvements-assign-ids suggestions))
      (%suggest-improvements-append-learnings normalized-scope suggestions)
      (let ((result (make-hash-table :test 'equal)))
        (setf (gethash "suggestions" result) suggestions)
        result))))

;;; ============================================================
;;; Self-analysis: self-assess
;;; ============================================================

(defvar *self-assess-running* nil
  "Guard against recursive test runs inside self-assess.")

(defvar *self-assess-last-test-results* nil
  "Cache of the most recent run-tests result (parsed).")

(defvar *codebase-map-cache* nil
  "Dynamically-scoped hash table for codebase-map results during test runs.
   NIL means no caching (production default).
   Bound by with-codebase-map-cache macro; key is detail-level string.")

(defmacro with-codebase-map-cache (() &body body)
  "Execute BODY with an active codebase-map cache.
   Within BODY, codebase-map results are cached by detail-level to avoid
   redundant file system scans. Cache is scoped to this macro's dynamic extent.
   Only use during testing — do NOT use in production code."
  `(let ((*codebase-map-cache* (make-hash-table :test 'equal)))
     ,@body))

(defparameter *self-assess-cyclomatic-branches*
  '("if" "when" "unless" "cond" "case" "typecase" "ecase" "ccase"
    "handler-case" "restart-case" "and" "or")
  "Symbols counted as cyclomatic decision points.")

(defparameter *self-assess-tool-categories*
  '(("file_ops" "read-file" "write-file" "list-directory" "grep" "file-info" "shell")
    ("lisp_introspection" "read-sexp" "describe-symbol" "macroexpand-form"
                           "package-symbols" "who-calls" "codebase-map")
    ("testing" "run-tests" "write-test")
    ("self_modification" "safe-redefine" "sync-to-file" "add-definition")
    ("analysis" "suggest-improvements" "self-assess")
    ("evaluation" "eval-form"))
  "Category map for registered tool names.")

(defun %self-assess-ensure-list (value)
  (%tools-ensure-list value))

(defun %self-assess-parse-json (json)
  (%tools-parse-json json))

(defun %self-assess-collect-files-from-map (map)
  (let* ((modules (%self-assess-ensure-list (gethash "modules" map)))
         (paths nil))
    (dolist (module modules (nreverse paths))
      (dolist (file (%self-assess-ensure-list (gethash "files" module)))
        (let ((path (gethash "path" file)))
          (when path
            (push path paths)))))))

(defun %self-assess-count-lines (content)
  (cond
    ((null content) 0)
    ((string= content "") 0)
    (t (1+ (count #\Newline content)))))

(defun %self-assess-total-lines (paths)
  (let ((total 0))
    (dolist (path paths total)
      (handler-case
          (let* ((resolved (%suggest-improvements-resolve-path path))
                 (content (uiop:read-file-string resolved)))
            (incf total (%self-assess-count-lines content)))
        (error () nil)))))

(defun %self-assess-definition-function-p (definition)
  (let ((type (getf definition :type)))
    (member type '("defun" "defmethod") :test #'string=)))

(defun %self-assess-collect-function-names (definitions)
  (let ((names nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (definition definitions (nreverse names))
      (let ((name (getf definition :name)))
        (when (and name
                   (%self-assess-definition-function-p definition)
                   (%suggest-improvements-public-name-p name))
          (let ((key (string-downcase name)))
            (unless (gethash key seen)
              (setf (gethash key seen) t)
              (push name names))))))))

(defun %self-assess-parse-form (form-string)
  (let ((*read-eval* nil))
    (handler-case
        (multiple-value-bind (form read-position)
            (read-from-string form-string)
          (declare (ignore read-position))
          form)
      (error () nil))))

(defun %self-assess-cyclomatic-count (form)
  (let ((count 0))
    (labels ((walk (node)
               (when (consp node)
                 (let ((head (car node)))
                   (when (and (symbolp head)
                              (member (string-downcase (symbol-name head))
                                      *self-assess-cyclomatic-branches*
                                      :test #'string=))
                     (incf count)))
                 (walk (car node))
                 (walk (cdr node)))))
      (walk form))
    (1+ count)))

(defun %self-assess-total-cyclomatic (definitions)
  (let ((total 0)
        (count 0))
    (dolist (definition definitions (values total count))
      (when (%self-assess-definition-function-p definition)
        (let* ((form-string (getf definition :form))
               (form (and form-string (%self-assess-parse-form form-string))))
          (when form
            (incf count)
            (incf total (%self-assess-cyclomatic-count form))))))))

(defun %self-assess-test-files ()
  (let* ((tests-dir (asdf:system-relative-pathname :sibyl "tests/"))
         (files (and (probe-file tests-dir)
                     (%codebase-map-find-lisp-files tests-dir))))
    (%self-assess-ensure-list files)))

(defun %self-assess-count-tests-in-content (content)
  (let ((count 0)
        (*read-eval* nil)
        (*package* (or (find-package "SIBYL.TESTS") *package*)))
    (handler-case
        (with-input-from-string (stream content)
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (when (and (consp form)
                              (symbolp (car form))
                              (string= (string-downcase (symbol-name (car form)))
                                       "test"))
                     (incf count))))
      (error () nil))
    count))

(defun %self-assess-count-tests (files)
  (let ((total 0))
    (dolist (file files total)
      (handler-case
          (let ((content (uiop:read-file-string file)))
            (incf total (%self-assess-count-tests-in-content content)))
        (error () nil)))))

(defun %self-assess-untested-functions (function-names tests-content)
  (let ((untested nil))
    (dolist (name function-names (nreverse untested))
      (let ((lower (string-downcase name)))
        (unless (search lower tests-content)
          (push name untested))))))

(defun %self-assess-coverage-estimate (total-functions untested-count)
  (if (and (integerp total-functions) (> total-functions 0))
      (/ (float (- total-functions untested-count))
         total-functions)
      0.0))

(defun %self-assess-tool-category (tool-name)
  (let ((lower (string-downcase tool-name)))
    (dolist (entry *self-assess-tool-categories* "other")
      (let ((category (first entry))
            (names (rest entry)))
        (when (find lower names :test #'string=)
          (return category))))))

(defun %self-assess-categories-counts (tools)
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (tool tools counts)
      (let* ((name (tool-name tool))
             (category (%self-assess-tool-category name))
             (current (gethash category counts 0)))
        (setf (gethash category counts) (1+ current))))))

(defun %self-assess-tool-entries (tools)
  (mapcar (lambda (tool)
            (let ((entry (make-hash-table :test 'equal)))
              (setf (gethash "name" entry) (tool-name tool))
              (setf (gethash "description" entry) (tool-description tool))
              (setf (gethash "category" entry)
                    (%self-assess-tool-category (tool-name tool)))
              entry))
          tools))

(defun %self-assess-tool-name-set (tools)
  (let ((set (make-hash-table :test 'equal)))
    (dolist (tool tools set)
      (setf (gethash (string-downcase (tool-name tool)) set) t))))

(defun %self-assess-capabilities (tool-set execute-tool-description)
  (let ((caps nil))
    (when (and (gethash "read-file" tool-set)
               (gethash "write-file" tool-set))
      (push "Can read and write files" caps))
    (when (or (gethash "list-directory" tool-set)
              (gethash "grep" tool-set))
      (push "Can search and list files/directories" caps))
    (when (gethash "eval-form" tool-set)
      (push "Can evaluate Lisp forms safely" caps))
    (when (and (gethash "read-sexp" tool-set)
               (gethash "describe-symbol" tool-set)
               (gethash "codebase-map" tool-set))
      (push "Can introspect Lisp symbols and codebase structure" caps))
    (when (and (gethash "run-tests" tool-set)
               (gethash "write-test" tool-set))
      (push "Can generate and run tests programmatically" caps))
    (when (and (gethash "safe-redefine" tool-set)
               (gethash "sync-to-file" tool-set))
      (push "Can modify functions safely and sync to disk" caps))
    (when (gethash "suggest-improvements" tool-set)
      (push "Can analyze codebase and propose improvements" caps))
    (when (and execute-tool-description
               (search "status: found" (string-downcase execute-tool-description)))
      (push "Can execute registered tools with error handling" caps))
    (nreverse caps)))

(defun %self-assess-toolset-limitations (tool-set)
  (let ((limitations nil))
    (unless (gethash "safe-redefine" tool-set)
      (push "Cannot safely redefine functions in-memory" limitations))
    (unless (gethash "sync-to-file" tool-set)
      (push "Cannot persist self-modifications to disk" limitations))
    (unless (gethash "run-tests" tool-set)
      (push "Cannot execute tests programmatically" limitations))
    (push "Cannot modify external dependencies or Quicklisp libraries" limitations)
    (push "Cannot modify system prompt or LLM client" limitations)
    (nreverse limitations)))

(defun %self-assess-known-limitations ()
  (list
   "Cannot modify system prompt or LLM client"
   "Requires human approval for self-modifications"
   "No persistent task execution logging yet"
   "Self-modification limited to Sibyl packages"
   "ASDF reload protection required for in-memory modifications"
   "Cannot bypass approval flow or safety guardrails"
   "Requires SBCL (sb-introspect) for full introspection"
   "Tooling is Lisp-only; no multi-language parsing"
   "External dependency changes require manual updates"))

(defun %self-assess-describe-symbol (symbol-string)
  (handler-case
      (execute-tool "describe-symbol" (list (cons "symbol" symbol-string)))
    (error () nil)))

(defun %self-assess-run-tests ()
  ;; When already inside a test run, return cached results to prevent
  ;; recursive full-suite re-execution.
  (when *self-assess-running*
    (return-from %self-assess-run-tests
      (if *self-assess-last-test-results*
          (values *self-assess-last-test-results* nil)
          (let ((default (make-hash-table :test 'equal)))
            (setf (gethash "passed" default) 0
                  (gethash "failed" default) 0
                  (gethash "total" default) 0)
            (values default nil)))))
  (when (find-tool "run-tests")
    (let ((*self-assess-running* t))
      (handler-case
          (let* ((result (execute-tool "run-tests" nil))
                 (parsed (%self-assess-parse-json result)))
            (setf *self-assess-last-test-results* parsed)
            (values parsed nil))
        (error (e)
          (values nil (format nil "~a" e)))))))

(deftool "self-assess"
    (:description "Evaluate Sibyl's current capabilities and limitations."
     :category :analysis
     :parameters ())
  (block self-assess
    (let* ((map-json (execute-tool "codebase-map"
                                   '(("detail-level" . "summary"))))
           (map (%self-assess-parse-json map-json))
           (modules (%self-assess-ensure-list (gethash "modules" map)))
           (module-count (length modules))
           (src-files (%self-assess-collect-files-from-map map))
           (total-lines (%self-assess-total-lines src-files))
           (definitions (%suggest-improvements-collect-definitions src-files))
           (function-names (%self-assess-collect-function-names definitions))
           (total-functions (length function-names))
           (cyclomatic-total 0)
           (cyclomatic-count 0)
           (package-symbols-json (execute-tool "package-symbols"
                                               '(("package" . "SIBYL.TOOLS"))))
           (package-symbols-parsed (%self-assess-parse-json package-symbols-json))
           (package-symbols (%self-assess-ensure-list package-symbols-parsed))
           (package-symbols-count (length package-symbols))
           (tools (list-tools))
           (tool-set (%self-assess-tool-name-set tools))
           (tool-entries (%self-assess-tool-entries tools))
           (categories (%self-assess-categories-counts tools))
           (execute-tool-description
             (%self-assess-describe-symbol "sibyl.tools:execute-tool"))
           (capabilities (%self-assess-capabilities tool-set execute-tool-description))
           (toolset-limitations (%self-assess-toolset-limitations tool-set))
           (tests-files (%self-assess-test-files))
           (test-files-count (length tests-files))
           (tests-content (%suggest-improvements-read-tests-content))
           (untested-functions (%self-assess-untested-functions function-names tests-content))
           (untested-count (length untested-functions))
           (coverage-estimate (%self-assess-coverage-estimate total-functions untested-count))
           (test-count (%self-assess-count-tests tests-files))
           (test-results nil)
           (test-results-error nil))
      (multiple-value-setq (cyclomatic-total cyclomatic-count)
        (%self-assess-total-cyclomatic definitions))
      (cond
        ((not *self-assess-running*)
         (multiple-value-setq (test-results test-results-error)
           (%self-assess-run-tests)))
        (t
         (setf test-results *self-assess-last-test-results*)))
      (let* ((tests-total (or (and test-results (gethash "total" test-results))
                              test-count))
             (tests-passed (and test-results (gethash "passed" test-results)))
             (tests-failed (and test-results (gethash "failed" test-results)))
             (pass-rate (if (and tests-total (integerp tests-total) (> tests-total 0)
                                 (integerp tests-passed))
                            (/ (float tests-passed) tests-total)
                            nil))
             (codebase (make-hash-table :test 'equal))
             (complexity (make-hash-table :test 'equal))
             (toolset (make-hash-table :test 'equal))
             (test-coverage (make-hash-table :test 'equal))
             (result (make-hash-table :test 'equal)))
        (setf (gethash "total" complexity) cyclomatic-total)
        (setf (gethash "average" complexity)
              (if (> cyclomatic-count 0)
                  (/ (float cyclomatic-total) cyclomatic-count)
                  0.0))
        (setf (gethash "function_count" complexity) cyclomatic-count)
        (setf (gethash "total_lines" codebase) total-lines)
        (setf (gethash "total_functions" codebase) total-functions)
        (setf (gethash "total_modules" codebase) module-count)
        (setf (gethash "test_files" codebase) test-files-count)
        (setf (gethash "cyclomatic_complexity" codebase) complexity)
        (setf (gethash "total_tools" toolset) (hash-table-count *tool-registry*))
        (setf (gethash "package_symbols" toolset) package-symbols-count)
        (setf (gethash "categories" toolset) categories)
        (setf (gethash "capabilities" toolset) capabilities)
        (setf (gethash "limitations" toolset) toolset-limitations)
        (setf (gethash "tools" toolset) tool-entries)
        (setf (gethash "total_tests" test-coverage) tests-total)
        (setf (gethash "passed" test-coverage) tests-passed)
        (setf (gethash "failed" test-coverage) tests-failed)
        (setf (gethash "pass_rate" test-coverage) pass-rate)
        (setf (gethash "untested_functions" test-coverage) untested-count)
        (setf (gethash "coverage_estimate" test-coverage) coverage-estimate)
        (setf (gethash "untested" test-coverage) untested-functions)
        (when test-results-error
          (setf (gethash "test_run_error" test-coverage) test-results-error))
        (setf (gethash "toolset" result) toolset)
        (setf (gethash "codebase" result) codebase)
        (setf (gethash "test_coverage" result) test-coverage)
        (setf (gethash "limitations" result) (%self-assess-known-limitations))
        result))))

;;; ============================================================
;;; Self-analysis: improvement-plan
;;; ============================================================

(defun %improvement-plan-ensure-list (value)
  (%tools-ensure-list value))

(defun %improvement-plan-parse-json (json)
  (%tools-parse-json json))

(defun %improvement-plan-string-contains-p (text keywords)
  (when text
    (let ((lower (string-downcase text)))
      (some (lambda (keyword)
              (search keyword lower))
            keywords))))

(defun %improvement-plan-limitations-match-p (limitations keywords)
  (let ((items (%improvement-plan-ensure-list limitations)))
    (some (lambda (entry)
            (%improvement-plan-string-contains-p entry keywords))
          items)))

(defun %improvement-plan-toolset-has-keyword-p (tools keywords)
  (let ((entries (%improvement-plan-ensure-list tools)))
    (some (lambda (tool)
            (let ((name (gethash "name" tool))
                  (desc (gethash "description" tool)))
              (or (%improvement-plan-string-contains-p name keywords)
                  (%improvement-plan-string-contains-p desc keywords))))
          entries)))

(defun %improvement-plan-make-entry (title description category priority
                                    risk effect timeframe rationale estimated-effort)
  (let ((entry (make-hash-table :test 'equal)))
    (setf (gethash "title" entry) title)
    (setf (gethash "description" entry) description)
    (setf (gethash "category" entry) category)
    (setf (gethash "priority" entry) priority)
    (setf (gethash "risk" entry) risk)
    (setf (gethash "effect" entry) effect)
    (setf (gethash "timeframe" entry) timeframe)
    (setf (gethash "rationale" entry) rationale)
    (setf (gethash "estimated_effort" entry) estimated-effort)
    entry))

(defun %improvement-plan-assign-ids (improvements)
  (loop for improvement in improvements
        for idx from 1
        do (setf (gethash "id" improvement) idx))
  improvements)

(defun %improvement-plan-priority-rank (priority)
  (cond
    ((string= priority "high") 0)
    ((string= priority "medium") 1)
    (t 2)))

(defun %improvement-plan-sort (improvements)
  (sort (copy-list improvements)
        (lambda (a b)
          (let ((rank-a (%improvement-plan-priority-rank
                         (gethash "priority" a)))
                (rank-b (%improvement-plan-priority-rank
                         (gethash "priority" b))))
            (if (/= rank-a rank-b)
                (< rank-a rank-b)
                (string< (gethash "category" a)
                         (gethash "category" b)))))))

(defun %improvement-plan-summary (improvements)
  (let ((summary (make-hash-table :test 'equal)))
    (setf (gethash "total_improvements" summary) (length improvements))
    (setf (gethash "high_priority" summary)
          (count-if (lambda (improvement)
                      (string= (gethash "priority" improvement) "high"))
                    improvements))
    (setf (gethash "medium_priority" summary)
          (count-if (lambda (improvement)
                      (string= (gethash "priority" improvement) "medium"))
                    improvements))
    (setf (gethash "low_priority" summary)
          (count-if (lambda (improvement)
                      (string= (gethash "priority" improvement) "low"))
                    improvements))
    (setf (gethash "short_term" summary)
          (count-if (lambda (improvement)
                      (string= (gethash "timeframe" improvement) "short"))
                    improvements))
    (setf (gethash "medium_term" summary)
          (count-if (lambda (improvement)
                      (string= (gethash "timeframe" improvement) "medium"))
                    improvements))
    (setf (gethash "long_term" summary)
          (count-if (lambda (improvement)
                      (string= (gethash "timeframe" improvement) "long"))
                    improvements))
    summary))

(defun %improvement-plan-estimate-test-effort (untested-count)
  (cond
    ((>= untested-count 40) "6-10 hours")
    ((>= untested-count 20) "4-6 hours")
    ((>= untested-count 10) "3-5 hours")
    (t "2-4 hours")))

(defun %improvement-plan-definition-table (definitions)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (definition definitions table)
      (let ((name (getf definition :name))
            (type (getf definition :type)))
        (when (and name
                   (member type '("defun" "defmethod") :test #'string=))
          (push definition (gethash (string-downcase name) table)))))))

(defun %improvement-plan-callers-for-definition (definition)
  (let* ((name (getf definition :name))
         (file (getf definition :file))
         (package (%suggest-improvements-package-for-file file))
         (qualified (format nil "~a::~a" (string-downcase package) name))
         (callers (%suggest-improvements-callers-count qualified)))
    (or callers 0)))

(defun %improvement-plan-collect-untested-callers (untested definitions)
  (let ((table (%improvement-plan-definition-table definitions))
        (best (make-hash-table :test 'equal)))
    (dolist (name untested)
      (let* ((key (string-downcase name))
             (defs (gethash key table)))
        (dolist (definition defs)
          (let* ((callers (%improvement-plan-callers-for-definition definition))
                 (current (gethash key best)))
            (when (or (null current)
                      (> callers (getf current :callers)))
              (setf (gethash key best)
                    (list :name name
                          :callers callers
                          :file (getf definition :file))))))))
    (let ((entries nil))
      (maphash (lambda (key value)
                 (declare (ignore key))
                 (push value entries))
               best)
      entries)))

(defun %improvement-plan-sort-untested (entries)
  (sort (copy-list entries)
        #'>
        :key (lambda (entry) (or (getf entry :callers) 0))))

(defun %improvement-plan-format-untested-sample (entries)
  (let* ((nonzero (remove-if (lambda (entry)
                               (<= (getf entry :callers) 0))
                             entries))
         (sample (subseq (or nonzero entries)
                         0
                         (min 3 (length (or nonzero entries))))))
    (when sample
      (if nonzero
          (string-join ", "
                       (mapcar (lambda (entry)
                                 (format nil "~a (~a callers)"
                                         (getf entry :name)
                                         (getf entry :callers)))
                               sample))
          (string-join ", "
                       (mapcar (lambda (entry)
                                 (getf entry :name))
                               sample))))))

(defun %improvement-plan-store-suggestions (improvements)
  (let* ((repl-package (find-package "SIBYL.REPL"))
         (store (and repl-package (find-symbol "STORE-SUGGESTIONS" repl-package))))
    (when (and store (fboundp store))
      (funcall store
               (mapcar (lambda (improvement)
                         (list :id (gethash "id" improvement)
                               :description (gethash "title" improvement)
                               :rationale (gethash "rationale" improvement)
                               :priority (gethash "priority" improvement)))
                       improvements)))))

(deftool "improvement-plan"
    (:description "Generate a prioritized improvement plan from self-assessment."
     :category :analysis
     :parameters ())
  (block improvement-plan
    (let* ((assessment-json (execute-tool "self-assess" nil))
           (assessment (%improvement-plan-parse-json assessment-json))
           (toolset (gethash "toolset" assessment))
           (codebase (gethash "codebase" assessment))
           (test-coverage (gethash "test_coverage" assessment))
           (limitations (%improvement-plan-ensure-list
                         (gethash "limitations" assessment)))
           (tools (%improvement-plan-ensure-list
                   (and toolset (gethash "tools" toolset))))
           (timestamp (timestamp-now))
           (improvements nil))
      ;; Test coverage improvements
      (let* ((untested (%improvement-plan-ensure-list
                        (and test-coverage (gethash "untested" test-coverage))))
             (untested-count (length untested)))
        (when (> untested-count 0)
          (let* ((map-json (execute-tool "codebase-map"
                                         '(("detail-level" . "summary"))))
                 (map (%improvement-plan-parse-json map-json))
                 (files (%self-assess-collect-files-from-map map))
                 (definitions (%suggest-improvements-collect-definitions files))
                 (untested-info (%improvement-plan-collect-untested-callers
                                 untested definitions))
                 (sorted (%improvement-plan-sort-untested untested-info))
                 (high-callers (remove-if (lambda (entry)
                                            (< (getf entry :callers) 3))
                                          sorted))
                 (priority (if high-callers "high"
                               (if (> untested-count 10) "medium" "low")))
                 (sample (%improvement-plan-format-untested-sample sorted))
                 (title (if high-callers
                            "Add tests for high-traffic untested functions"
                            "Improve test coverage for untested functions"))
                 (description (if sample
                                  (format nil "~a functions lack tests; prioritize ~a."
                                          untested-count sample)
                                  (format nil "~a functions lack tests; prioritize critical paths."
                                          untested-count)))
                 (rationale (if high-callers
                                "Functions with 3+ callers need tests to prevent regressions."
                                "Untested public functions increase regression risk."))
                 (estimated-effort (%improvement-plan-estimate-test-effort
                                    untested-count)))
            (push (%improvement-plan-make-entry
                   title description "test-coverage" priority
                   "low" "high" "short" rationale estimated-effort)
                  improvements))))
      ;; Infrastructure gaps from limitations
      (when (%improvement-plan-limitations-match-p
             limitations '("logging" "log"))
        (push (%improvement-plan-make-entry
               "Implement task execution logging"
               "Add persistent logging for task success/failure tracking."
               "infrastructure" "medium" "medium" "high" "medium"
               "Self-assessment notes missing persistent logging for task outcomes."
               "5-8 hours")
              improvements))
      ;; Tooling gaps: debugging/tracing
      (when (not (%improvement-plan-toolset-has-keyword-p
                  tools '("debug" "trace" "profil" "diagnos")))
        (push (%improvement-plan-make-entry
               "Add debugging and tracing tools"
               "Provide lightweight tracing or debug inspection for tool runs and agent steps."
               "tooling" "medium" "medium" "high" "medium"
               "Current toolset lacks debugging/tracing capabilities for diagnosing failures."
               "4-6 hours")
              improvements))
      ;; Codebase complexity or size driven refactoring
      (let* ((total-lines (gethash "total_lines" codebase))
             (total-functions (gethash "total_functions" codebase))
             (complexity (and codebase (gethash "cyclomatic_complexity" codebase)))
             (avg-complexity (and complexity (gethash "average" complexity))))
        (when (or (and total-lines (> total-lines 3000))
                  (and total-functions (> total-functions 80))
                  (and avg-complexity (> avg-complexity 3.0)))
          (push (%improvement-plan-make-entry
                 "Refactor large modules and hotspots"
                 (format nil "Codebase has ~a lines and ~a functions with avg complexity ~,2f; refactor hotspots for maintainability."
                         (or total-lines 0)
                         (or total-functions 0)
                         (or avg-complexity 0.0))
                 "refactoring" "medium" "medium" "medium" "medium"
                 "Targeted refactors keep complexity low and reduce future change risk."
                 "6-10 hours")
                improvements)))
      ;; Tooling gaps: visualization/graphing
      (when (not (%improvement-plan-toolset-has-keyword-p
                  tools '("graph" "visual" "diagram" "chart")))
        (push (%improvement-plan-make-entry
               "Add visualization tools for codebase graphs"
               "Provide graph/diagram views for module and call relationships."
               "features" "low" "low" "medium" "long"
               "Visualization accelerates architectural understanding and review workflows."
               "8-12 hours")
              improvements))
      (setf improvements (%improvement-plan-sort improvements))
      (setf improvements (%improvement-plan-assign-ids improvements))
      (%improvement-plan-store-suggestions improvements)
      (let ((result (make-hash-table :test 'equal)))
        (setf (gethash "plan_id" result)
              (format nil "plan-~a" timestamp))
        (setf (gethash "based_on_assessment" result)
              (format nil "self-assess-~a" timestamp))
        (setf (gethash "improvements" result) improvements)
        (setf (gethash "summary" result)
              (%improvement-plan-summary improvements))
        result))))

;;; ============================================================
;;; Programmatic test execution
;;; ============================================================


(defun %file-to-suite-mapping ()
  "Return a hash-table mapping source file paths to their test suite name(s).
   Values are either a string (single suite) or a list of strings (multiple suites)."
  (let ((m (make-hash-table :test 'equal)))
    (setf (gethash "src/tools/lisp-tools.lisp" m)
          '("read-sexp-tests" "describe-symbol-tests" "eval-form-tests"
            "macroexpand-form-tests" "package-symbols-tests" "codebase-map-tests"
            "who-calls-tests"))
    (setf (gethash "src/tools/analysis-tools.lisp" m)
          '("suggest-improvements-tests" "suggest-improvements-enhanced-tests"
            "self-assess-tests" "improvement-plan-tests" "safe-redefine-tests"
            "sync-to-file-tests" "run-tests-tests" "write-test-tests"))
    (setf (gethash "src/tools/creation-tools.lisp" m)
          '("creation-integration-tests" "add-definition-tests" "add-export-tests"
            "create-module-tests" "asdf-registration-tests"))
    (setf (gethash "src/tools/planning-tools.lisp" m) "planning-tests")
    (setf (gethash "src/tools/tools.lisp" m) "tools-tests")
    (setf (gethash "src/agent/core.lisp" m) "agent-tests")
    (setf (gethash "src/agent/message.lisp" m) "message-tests")
    (setf (gethash "src/agent/client.lisp" m) "client-tests")
    (setf (gethash "src/agent/token-tracking.lisp" m)
          '("token-tracking-suite" "model-selector-suite" "memory-compaction-suite"))
    (setf (gethash "src/agent/parallel-agent.lisp" m) "parallel-agent-tests")
    (setf (gethash "src/agent/parallel-runner.lisp" m) "parallel-runner-tests")
    (setf (gethash "src/repl/repl.lisp" m)
          '("repl-tests" "register-command-tests" "tokens-tests"))
    (setf (gethash "src/repl/rich-repl.lisp" m) "rich-repl-tests")
    (setf (gethash "src/system/asdf-protection.lisp" m) "asdf-protection-tests")
    (setf (gethash "src/mcp/client.lisp" m) "mcp-tests")
    m))

(defun %suites-for-files (file-paths)
  "Given a list of source file paths, return a deduplicated list of test suite names.
   Handles both single-string and list values in the mapping."
  (let ((mapping (%file-to-suite-mapping))
        (suites nil))
    (dolist (path file-paths)
      (let ((entry (gethash path mapping)))
        (when entry
          (let ((names (if (listp entry) entry (list entry))))
            (dolist (name names)
              (unless (member name suites :test #'string=)
                (push name suites)))))))
    (nreverse suites)))

(defun %run-tests-resolve-target (suite-name test-name)
  "Resolve suite or test name to a symbol in SIBYL.TESTS package."
  (let ((tests-package (find-package "SIBYL.TESTS")))
    (unless tests-package
      (error "SIBYL.TESTS package not found. Load tests first."))
    (cond
      (test-name
       (let ((sym (find-symbol (string-upcase test-name) tests-package)))
         (unless sym
           (error "Test not found: ~a" test-name))
         sym))
      (suite-name
       (let ((sym (find-symbol (string-upcase suite-name) tests-package)))
         (unless sym
           (error "Suite not found: ~a" suite-name))
         sym))
      (t
       (let ((default-suite (find-symbol "SIBYL-TESTS" tests-package)))
         (unless default-suite
           (error "Default test suite SIBYL-TESTS not found"))
         default-suite)))))

(defun %run-tests-count-results (results)
  "Count total, passed, and failed tests from FiveAM results."
  (let ((total 0)
        (passed 0)
        (failed 0)
        (failures nil)
        (fiveam-pkg (find-package "FIVEAM")))
    (unless fiveam-pkg
      (error "FiveAM package not found"))
    (let ((test-result-class (find-symbol "TEST-RESULT" fiveam-pkg))
          (test-passed-class (find-symbol "TEST-PASSED" fiveam-pkg))
          (test-failure-class (find-symbol "TEST-FAILURE" fiveam-pkg))
          (test-suite-result-class (find-symbol "TEST-SUITE-RESULT" fiveam-pkg))
          (test-case-slot (find-symbol "TEST-CASE" fiveam-pkg))
          (name-slot (find-symbol "NAME" fiveam-pkg))
          (reason-accessor (find-symbol "REASON" fiveam-pkg))
          (results-accessor (find-symbol "RESULTS" fiveam-pkg)))
      (labels ((get-test-name (result)
                 (let ((test-case (slot-value result test-case-slot)))
                   (when test-case
                     (slot-value test-case name-slot))))
               (walk-result (result)
                 (cond
                   ;; Test result
                   ((typep result test-result-class)
                    (incf total)
                    (if (typep result test-passed-class)
                        (incf passed)
                        (progn
                          (incf failed)
                          (push (make-hash-table :test 'equal) failures)
                          (let ((failure (car failures))
                                (test-name (get-test-name result)))
                            (setf (gethash "test" failure)
                                  (if test-name
                                      (string-downcase (symbol-name test-name))
                                      "unknown"))
                            (when (typep result test-failure-class)
                              (setf (gethash "reason" failure)
                                    (format nil "~a" (funcall reason-accessor result))))))))
                   ;; Suite result - walk children
                   ((typep result test-suite-result-class)
                    (dolist (child (funcall results-accessor result))
                      (walk-result child)))
                   ;; List of results
                   ((listp result)
                    (dolist (r result)
                      (walk-result r))))))
        (walk-result results))
      (values total passed failed (nreverse failures)))))

(defun %run-tests-format-result (result)
  "Format run-tests result hash-table as a compact human-readable string.
All-pass:  \"✓ 42/42 tests passed\"
Failures:  \"✗ 40/42 passed\nFailures:\n  • test-name: reason\""
  (let* ((total    (or (gethash "total"    result) 0))
         (passed   (or (gethash "passed"   result) 0))
         (failed   (or (gethash "failed"   result) 0))
         (failures (gethash "failures" result)))
    (if (zerop failed)
        (format nil "✓ ~a/~a tests passed" passed total)
        (with-output-to-string (s)
          (format s "✗ ~a/~a passed" passed total)
          (when failures
            (format s "~%Failures:")
            (dolist (f failures)
              (let ((test-name (gethash "test"   f "unknown"))
                    (reason    (gethash "reason" f "")))
                (format s "~%  • ~a: ~a" test-name reason))))))))


(deftool "run-tests"
 (:description
  "Run FiveAM tests programmatically and return structured results." :category
  :analysis :parameters
  ((:name "suite" :type "string" :required nil :description
    "Suite name (e.g., \"sibyl-tests\"), defaults to all tests")
   (:name "test" :type "string" :required nil :description
    "Specific test name (e.g., \"read-sexp-basic\")")
   (:name "files" :type "string" :required nil :description
    "Comma-separated source file paths; runs only their mapped test suites")))
 (block run-tests
   (let* ((suite-name (getf args :suite))
          (test-name (getf args :test))
          (files-str (getf args :files))
          (suite-name
           (or suite-name
               (when files-str
                 (let* ((paths
                         (mapcar (lambda (s) (string-trim '(#\  #\Tab) s))
                                 (uiop/utility:split-string files-str
                                                            :separator ",")))
                        (suites (%suites-for-files paths)))
                   (when suites (first suites))))))
          (target (%run-tests-resolve-target suite-name test-name))
          (fiveam-pkg (find-package "FIVEAM")))
     (unless fiveam-pkg (error "FiveAM package not found. Load FiveAM first."))
     (let ((run-fn (find-symbol "RUN" fiveam-pkg)))
       (unless run-fn (error "FiveAM:RUN function not found"))
       (let* ((*standard-output* (make-string-output-stream))
              (*error-output* (make-string-output-stream))
              (it.bese.fiveam:*test-dribble* (make-broadcast-stream))
              (results (funcall run-fn target)))
         (multiple-value-bind (total passed failed failures)
             (%run-tests-count-results results)
           (let ((result (make-hash-table :test 'equal)))
             (setf (gethash "total" result) total)
             (setf (gethash "passed" result) passed)
             (setf (gethash "failed" result) failed)
             (setf (gethash "failures" result) failures)
             result)))))))

;;; ============================================================
;;; Programmatic test generation
;;; ============================================================

(defun %write-test-validate-name (name)
  "Validate test NAME is a non-empty string."
  (unless (and (stringp name) (string/= name ""))
    (error "Test name must be a non-empty string: ~a" name))
  name)

(defun %write-test-validate-body (body)
  "Validate test BODY is a non-empty string and valid Lisp."
  (unless (and (stringp body) (string/= body ""))
    (error "Test body must be a non-empty string: ~a" body))
  ;; Validate it's valid Lisp
  (let ((*read-eval* nil))
    (handler-case
        (read-from-string body)
      (error (e)
        (error "Invalid test body syntax: ~a" e))))
  body)

(defun %write-test-check-duplicate (name package)
  "Check if a test with NAME already exists in PACKAGE."
  (let* ((test-symbol (intern (string-upcase name) package))
         (fiveam-pkg (find-package "FIVEAM"))
         (get-test-fn (and fiveam-pkg (find-symbol "GET-TEST" fiveam-pkg))))
    (when (and get-test-fn (funcall get-test-fn test-symbol))
      (error "Test ~a already exists" name))))

(defun %write-test-generate-code (name body)
  "Generate test definition code."
  (format nil "(test ~a~%  \"Auto-generated test\"~%  ~a)"
          name body))

(defun %write-test-register-in-memory (test-code package-name)
  "Register test in memory using eval-form."
  (let ((result (execute-tool "eval-form"
                              (list (cons "form" test-code)
                                    (cons "package" package-name)))))
    (when (search "blocked unsafe form" (string-downcase result))
      (error "Failed to register test: ~a" result))
    (when (search "timeout" (string-downcase result))
      (error "Failed to register test: ~a" result))
    result))

(defun %write-test-append-to-file (file test-code)
  "Append test code to FILE."
  (unless (uiop:file-exists-p file)
    (error "Test file not found: ~a" file))
  (with-open-file (stream file :direction :output
                          :if-exists :append
                          :if-does-not-exist :error)
    (terpri stream)
    (write-string test-code stream)
    (terpri stream)))

(deftool "write-test"
    (:description "Generate and register a FiveAM test programmatically."
     :category :analysis
     :parameters ((:name "name" :type "string" :required t
                   :description "Test name (e.g., \"my-new-test\")")
                  (:name "suite" :type "string" :required nil
                   :description "Suite name (default: \"sibyl-tests\")")
                  (:name "body" :type "string" :required t
                   :description "Test body as S-expression string (e.g., \"(is (equal 1 1))\")")
                  (:name "file" :type "string" :required nil
                   :description "Target file (default: \"tests/sexp-tools-test.lisp\")")))
  (block write-test
    (let* ((name (%write-test-validate-name (getf args :name)))
           (suite (or (getf args :suite) "sibyl-tests"))
           (body (%write-test-validate-body (getf args :body)))
           (file (or (getf args :file)
                     (namestring
                      (asdf:system-relative-pathname :sibyl
                                                     "tests/sexp-tools-test.lisp"))))
           (package-name "SIBYL.TESTS")
           (package (find-package package-name)))
      
      (unless package
        (error "Package not found: ~a" package-name))
      
      ;; Check for duplicate
      (%write-test-check-duplicate name package)
      
      ;; Generate test code
      (let ((test-code (%write-test-generate-code name body)))
        
        ;; Register in-memory
        (%write-test-register-in-memory test-code package-name)
        
        ;; Persist to file
        (%write-test-append-to-file file test-code)
        
        (format nil "Success: Test ~a created and registered in ~a"
                name (file-namestring file))))))
