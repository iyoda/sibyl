;;;; evolution-tools.lisp — Evolution state management and progress reporting

(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package #:sibyl.tools))

;;; ============================================================
;;; Evolution state management
;;; ============================================================

(defvar *evolution-state* nil
  "Current evolution cycle state. A hash-table with keys:
   - \"cycle-number\"           : integer, current cycle number
   - \"attempted-improvements\" : list of strings, descriptions of attempted improvements
   - \"results\"                : list of (description . result) pairs
   - \"baseline-test-count\"    : integer, test count at start of evolution
   - \"baseline-tool-count\"    : integer, tool count at start of evolution
   - \"modified-files\"         : list of strings, files modified during evolution
   NIL when no evolution cycle is active.")

(defvar *evolution-state-lock* (bt:make-recursive-lock "evolution-state-lock")
  "Recursive lock protecting *evolution-state*.
   Lock order: tool-registry (1st) < evolution-state (2nd) < modified-files (3rd) < command-handlers (4th)")

(defun evolution-state-init ()
  "Initialize *evolution-state* with a fresh hash-table.
   Resets all fields to their default values."
  (bt:with-recursive-lock-held (*evolution-state-lock*)
    (let ((state (make-hash-table :test 'equal)))
      (setf (gethash "cycle-number" state) 0)
      (setf (gethash "attempted-improvements" state) nil)
      (setf (gethash "results" state) nil)
      (setf (gethash "baseline-test-count" state) 0)
      (setf (gethash "baseline-tool-count" state) 0)
      (setf (gethash "modified-files" state) nil)
      (setf *evolution-state* state)
      state)))

(defun evolution-state-record-attempt (description result)
  "Record an improvement attempt in *evolution-state*.
   DESCRIPTION is a string describing the improvement.
   RESULT is a string describing the outcome (e.g. \"success\", \"failed\").
   Initializes state if not already initialized."
  (bt:with-recursive-lock-held (*evolution-state-lock*)
    (unless *evolution-state*
      (evolution-state-init))
    (let ((state *evolution-state*))
      ;; Add to attempted-improvements list
      (push description (gethash "attempted-improvements" state))
      ;; Add to results as (description . result) pair
      (push (cons description result) (gethash "results" state))
      state)))

(defun %evolution-state-to-plist (state)
  "Convert evolution state hash-table to a plist suitable for JSON encoding."
  (when state
    (let* ((attempted (gethash "attempted-improvements" state))
           (results (gethash "results" state))
           (results-list (mapcar (lambda (pair)
                                   (let ((ht (make-hash-table :test 'equal)))
                                     (setf (gethash "description" ht) (car pair))
                                     (setf (gethash "result" ht) (cdr pair))
                                     ht))
                                 results)))
      (list "cycle-number" (gethash "cycle-number" state 0)
            "attempted-improvements" (or attempted (list))
            "results" (or results-list (list))
            "baseline-test-count" (gethash "baseline-test-count" state 0)
            "baseline-tool-count" (gethash "baseline-tool-count" state 0)
            "modified-files" (or (gethash "modified-files" state) (list))))))

(defun evolution-state-save (&optional (path nil))
  "Save *evolution-state* to a JSON file.
   PATH defaults to .sibyl/evolution-log.json relative to the system root.
   Creates parent directories if needed."
  (bt:with-recursive-lock-held (*evolution-state-lock*)
    (let* ((save-path (or path
                          (asdf:system-relative-pathname
                           :sibyl ".sibyl/evolution-log.json")))
           (plist (%evolution-state-to-plist *evolution-state*)))
      (ensure-directories-exist save-path)
      (with-open-file (stream save-path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (yason:encode-plist plist stream))
      save-path)))

(defun %evolution-state-from-hash (ht)
  "Convert a parsed JSON hash-table back to evolution state format."
  (let ((state (make-hash-table :test 'equal)))
    (setf (gethash "cycle-number" state)
          (or (gethash "cycle-number" ht) 0))
    ;; attempted-improvements: JSON array of strings
    (let ((attempted (gethash "attempted-improvements" ht)))
      (setf (gethash "attempted-improvements" state)
            (when attempted
              (coerce attempted 'list))))
    ;; results: JSON array of {description, result} objects → list of (desc . result) pairs
    (let ((results (gethash "results" ht)))
      (setf (gethash "results" state)
            (when results
              (mapcar (lambda (r)
                        (cons (gethash "description" r "")
                              (gethash "result" r "")))
                      (coerce results 'list)))))
    (setf (gethash "baseline-test-count" state)
          (or (gethash "baseline-test-count" ht) 0))
    (setf (gethash "baseline-tool-count" state)
          (or (gethash "baseline-tool-count" ht) 0))
    (let ((files (gethash "modified-files" ht)))
      (setf (gethash "modified-files" state)
            (when files (coerce files 'list))))
    state))

(defun evolution-state-load (&optional (path nil))
  "Load *evolution-state* from a JSON file.
   PATH defaults to .sibyl/evolution-log.json relative to the system root.
   Returns NIL gracefully if the file does not exist."
  (bt:with-recursive-lock-held (*evolution-state-lock*)
    (let* ((load-path (or path
                          (asdf:system-relative-pathname
                           :sibyl ".sibyl/evolution-log.json"))))
      (when (probe-file load-path)
        (handler-case
            (let* ((json-string (uiop:read-file-string load-path))
                   (parsed (yason:parse json-string :object-as :hash-table))
                   (state (%evolution-state-from-hash parsed)))
              (setf *evolution-state* state)
              state)
          (error (e)
            (warn "evolution-state-load: failed to parse ~a: ~a" load-path e)
            nil))))))

(defun %evolution-status-format-state (state)
  "Format evolution state as a human-readable string."
  (if (null state)
      "Evolution state: not initialized (no active evolution cycle)"
      (with-output-to-string (stream)
        (format stream "=== Evolution Status ===~%")
        (format stream "Cycle number: ~a~%"
                (gethash "cycle-number" state 0))
        (format stream "Baseline test count: ~a~%"
                (gethash "baseline-test-count" state 0))
        (format stream "Baseline tool count: ~a~%"
                (gethash "baseline-tool-count" state 0))
        (let ((attempted (gethash "attempted-improvements" state)))
          (format stream "Attempted improvements: ~a~%"
                  (length (or attempted nil)))
          (when attempted
            (dolist (desc attempted)
              (format stream "  - ~a~%" desc))))
        (let ((results (gethash "results" state)))
          (format stream "Results: ~a~%"
                  (length (or results nil)))
          (when results
            (dolist (pair results)
              (format stream "  - ~a → ~a~%" (car pair) (cdr pair)))))
        (let ((files (gethash "modified-files" state)))
          (format stream "Modified files: ~a~%"
                  (length (or files nil)))
          (when files
            (dolist (f files)
              (format stream "  - ~a~%" f)))))))

(deftool "evolution-status"
    (:description "Return the current evolution cycle state as a formatted string."
     :parameters ())
  (block evolution-status
    (%evolution-status-format-state *evolution-state*)))

;;; ============================================================
;;; Evolution Progress Reporting Functions
;;; ============================================================

(defun evolution-report-cycle-start (cycle-num max-cycles)
  "Print the start of an evolution cycle.
   FORMAT: === Evolution Cycle N/M ===
   CYCLE-NUM: integer, current cycle number (1-indexed)
   MAX-CYCLES: integer, total number of cycles"
  (format t "~%=== Evolution Cycle ~a/~a ===~%" cycle-num max-cycles))

(defun evolution-report-improvement-start (index total name)
  "Print the start of an improvement attempt.
   FORMAT: [N/M] Improving: NAME
   INDEX: integer, current improvement number (1-indexed)
   TOTAL: integer, total improvements in this cycle
   NAME: string, description of the improvement"
  (format t "[~a/~a] Improving: ~a~%" index total name))

(defun evolution-report-step (step-name)
  "Print a step in the improvement process.
   FORMAT: (no newline, appends to current line)
           Step: STEP-NAME...
   STEP-NAME: string, name of the step (e.g. \"RED\", \"GREEN\", \"PERSIST\")"
  (format t "      Step: ~a..." step-name))

(defun evolution-report-improvement-result (success-p)
  "Print the result of an improvement attempt.
   FORMAT: ✓ (on success) or ✗ (skipped) (on failure)
   SUCCESS-P: boolean, true if improvement succeeded"
  (if success-p
      (format t " ✓~%")
      (format t " ✗ (test regression, skipped)~%")))

(defun evolution-report-cycle-summary (succeeded skipped baseline-tests current-tests baseline-tools current-tools)
  "Print a summary of a completed evolution cycle.
   FORMAT:
     Cycle N complete: S succeeded, K skipped
     Tests: B → C (+/-D)
     Tools: B → C
   SUCCEEDED: integer, number of successful improvements
   SKIPPED: integer, number of skipped improvements
   BASELINE-TESTS: integer, test count at cycle start
   CURRENT-TESTS: integer, test count at cycle end
   BASELINE-TOOLS: integer, tool count at cycle start
   CURRENT-TOOLS: integer, tool count at cycle end"
  (let ((test-delta (- current-tests baseline-tests))
        (test-sign (if (>= (- current-tests baseline-tests) 0) "+" "")))
    (format t "~%Cycle complete: ~a succeeded, ~a skipped~%" succeeded skipped)
    (format t "Tests: ~a → ~a (~a~a)~%" baseline-tests current-tests test-sign test-delta)
    (format t "Tools: ~a → ~a~%" baseline-tools current-tools)))

(defun evolution-report-final-summary (cycles productive-cycles succeeded skipped baseline-tests current-tests)
  "Print the final summary of all evolution cycles.
   FORMAT:
     === Evolution Summary ===
     Cycles: N (P productive, E empty)
     Improvements: S succeeded, K skipped
     Tests: B → C (+/-D)
   CYCLES: integer, total number of cycles run
   PRODUCTIVE-CYCLES: integer, number of cycles with improvements
   SUCCEEDED: integer, total successful improvements
   SKIPPED: integer, total skipped improvements
   BASELINE-TESTS: integer, test count at evolution start
   CURRENT-TESTS: integer, test count at evolution end"
  (let ((empty-cycles (- cycles productive-cycles))
        (test-delta (- current-tests baseline-tests))
        (test-sign (if (>= (- current-tests baseline-tests) 0) "+" "")))
    (format t "~%=== Evolution Summary ===~%")
    (format t "Cycles: ~a (~a productive, ~a empty)~%" cycles productive-cycles empty-cycles)
    (format t "Improvements: ~a succeeded, ~a skipped~%" succeeded skipped)
    (format t "Tests: ~a → ~a (~a~a)~%" baseline-tests current-tests test-sign test-delta)))

(defun %register-command-make-keyword (name)
  "Convert NAME string to a keyword symbol."
  (intern (string-upcase (string-trim-whitespace name)) :keyword))

(deftool "register-command"
    (:description "Dynamically register a new REPL command in *command-handlers*."
     :parameters ((:name "name" :type "string" :required t
                   :description "Command name (e.g. \"evolve\"). Used as keyword key in *command-handlers*.")
                  (:name "description" :type "string" :required t
                   :description "Help text describing what the command does.")
                  (:name "handler-body" :type "string" :required t
                   :description "Lambda expression as S-expression string: (lambda (agent input) ...)")))
  (block register-command
    (let* ((name (getf args :name))
           (description (getf args :description))
           (handler-body (getf args :handler-body)))
      ;; Validate name
      (%register-command-validate-name name)
      ;; Validate description
      (unless (and (stringp description) (string/= (string-trim-whitespace description) ""))
        (error 'sibyl.conditions:tool-execution-error
               :tool-name "register-command"
               :message "Command description must be a non-empty string"
               :inner-error nil))
      ;; Parse and compile handler
      (let* ((handler-fn (%register-command-parse-handler handler-body))
             (keyword (%register-command-make-keyword name))
             (repl-package (find-package "SIBYL.REPL"))
             (handlers-sym (and repl-package
                                (find-symbol "*COMMAND-HANDLERS*" repl-package))))
        (unless (and handlers-sym (boundp handlers-sym))
          (error 'sibyl.conditions:tool-execution-error
                 :tool-name "register-command"
                 :message "sibyl.repl::*command-handlers* not found or unbound"
                 :inner-error nil))
        ;; Push new handler to the front of the alist (overwrites on assoc lookup)
        (push (cons keyword handler-fn) (symbol-value handlers-sym))
        (format nil "Success: command ~a registered (~a)"
                (string-downcase (symbol-name keyword))
                description)))))
