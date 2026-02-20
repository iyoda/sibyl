;;;; repl-test.lisp — Tests for REPL commands

(in-package #:sibyl.tests)

(def-suite repl-tests
  :description "Tests for REPL command handling.")

(in-suite repl-tests)

;;; ============================================================
;;; Command recognition tests
;;; ============================================================

(test improve-command-registered
  "Test that /improve command is registered in *repl-commands*."
  (is (assoc "/improve" sibyl.repl::*repl-commands* :test #'string-equal))
  (is (eq :improve (cdr (assoc "/improve" sibyl.repl::*repl-commands* 
                                :test #'string-equal)))))

(test improve-command-recognized
  "Test that /improve is recognized as a command."
  (is (eq :improve (sibyl.repl:repl-command-p "/improve"))))

;;; ============================================================
;;; Argument parsing tests
;;; ============================================================

(test parse-improve-args-basic
  "Test parsing basic /improve command with task description."
  (multiple-value-bind (task auto-commit)
      (sibyl.repl::parse-improve-args "/improve add new feature")
    (is (string= "add new feature" task))
    (is (null auto-commit))))

(test parse-improve-args-with-auto-commit
  "Test parsing /improve command with --auto-commit flag."
  (multiple-value-bind (task auto-commit)
      (sibyl.repl::parse-improve-args "/improve add feature --auto-commit")
    (is (string= "add feature" task))
    (is (eq t auto-commit))))

(test parse-improve-args-auto-commit-middle
  "Test parsing /improve with --auto-commit in middle of task."
  (multiple-value-bind (task auto-commit)
      (sibyl.repl::parse-improve-args "/improve add --auto-commit new feature")
    ;; Should extract everything before --auto-commit as task
    (is (string= "add" task))
    (is (eq t auto-commit))))

(test parse-improve-args-empty-task
  "Test parsing /improve with empty task description."
  (multiple-value-bind (task auto-commit)
      (sibyl.repl::parse-improve-args "/improve")
    (is (string= "" task))
    (is (null auto-commit))))

(test parse-improve-args-only-auto-commit
  "Test parsing /improve with only --auto-commit flag."
  (multiple-value-bind (task auto-commit)
      (sibyl.repl::parse-improve-args "/improve --auto-commit")
    (is (string= "" task))
    (is (eq t auto-commit))))

(test parse-improve-args-whitespace-handling
  "Test that parse-improve-args handles extra whitespace correctly."
  (multiple-value-bind (task auto-commit)
      (sibyl.repl::parse-improve-args "/improve   task with spaces   --auto-commit  ")
    (is (string= "task with spaces" task))
    (is (eq t auto-commit))))

;;; ============================================================
;;; Help text verification
;;; ============================================================

(test help-includes-improve
  "Test that /help output includes /improve command."
  (let ((output (with-output-to-string (*standard-output*)
                  (sibyl.repl::handle-repl-command :help nil))))
    (is (search "/improve" output :test #'string-equal))))

;;; ============================================================
;;; /review command tests
;;; ============================================================

(test review-command-registered
  "Test that /review command is registered in *repl-commands*."
  (is (assoc "/review" sibyl.repl::*repl-commands* :test #'string-equal))
  (is (eq :review (cdr (assoc "/review" sibyl.repl::*repl-commands* 
                              :test #'string-equal)))))

(test review-command-recognized
  "Test that /review is recognized as a command."
  (is (eq :review (sibyl.repl:repl-command-p "/review"))))

(test log-command-registered
  "Test that /log command is registered in *repl-commands*."
  (is (assoc "/log" sibyl.repl::*repl-commands* :test #'string-equal))
  (is (eq :log (cdr (assoc "/log" sibyl.repl::*repl-commands*
                           :test #'string-equal)))))

(test log-command-recognized
  "Test that /log is recognized as a command."
  (is (eq :log (sibyl.repl:repl-command-p "/log"))))

(test help-includes-log
  "Test that /help output includes /log command."
  (let ((output (with-output-to-string (*standard-output*)
                  (sibyl.repl::handle-repl-command :help nil))))
    (is (search "/log" output :test #'string-equal))))

(test log-command-off-on-roundtrip
  "Test that /log off mutes output and /log on restores the previous stream."
  (let* ((original-stream (make-string-output-stream))
         (sibyl.logging:*log-stream* original-stream)
         (sibyl.repl::*saved-log-stream* nil)
         (sibyl.repl::*muted-log-stream* nil))
    (with-output-to-string (*standard-output*)
      (sibyl.repl::handle-log-command nil "/log off"))
    (is (sibyl.repl::log-output-muted-p))
    (is (eq sibyl.logging:*log-stream* sibyl.repl::*muted-log-stream*))
    (with-output-to-string (*standard-output*)
      (sibyl.repl::handle-log-command nil "/log on"))
    (is (not (sibyl.repl::log-output-muted-p)))
    (is (eq sibyl.logging:*log-stream* original-stream))))

(test help-includes-review
  "Test that /help output includes /review command."
  (let ((output (with-output-to-string (*standard-output*)
                  (sibyl.repl::handle-repl-command :help nil))))
    (is (search "/review" output :test #'string-equal))))

;;; ============================================================
;;; Suggestion state management tests
;;; ============================================================

(test store-suggestion-creates-id
  "Test that store-suggestion creates a suggestion with an ID."
  (sibyl.repl::clear-suggestions)
  (let ((suggestion (sibyl.repl::store-suggestion 
                     "Add feature X" 
                     "It would be useful"
                     "high")))
    (is (numberp (getf suggestion :id)))
    (is (string= "Add feature X" (getf suggestion :description)))
    (is (string= "It would be useful" (getf suggestion :rationale)))
    (is (string= "high" (getf suggestion :priority)))
    (is (string= "pending" (getf suggestion :status)))))

(test get-suggestion-by-id-retrieves
  "Test that get-suggestion-by-id retrieves the correct suggestion."
  (sibyl.repl::clear-suggestions)
  (let* ((s1 (sibyl.repl::store-suggestion "Feature 1" "Reason 1" "high"))
         (s2 (sibyl.repl::store-suggestion "Feature 2" "Reason 2" "medium"))
         (id1 (getf s1 :id))
         (id2 (getf s2 :id)))
    (is (eq s1 (sibyl.repl::get-suggestion-by-id id1)))
    (is (eq s2 (sibyl.repl::get-suggestion-by-id id2)))))

(test get-suggestion-by-id-returns-nil-when-not-found
  "Test that get-suggestion-by-id returns NIL for non-existent ID."
  (sibyl.repl::clear-suggestions)
  (is (null (sibyl.repl::get-suggestion-by-id 99999))))

(test update-suggestion-status-modifies
  "Test that update-suggestion-status changes suggestion status."
  (sibyl.repl::clear-suggestions)
  (let* ((suggestion (sibyl.repl::store-suggestion 
                      "Feature" "Reason" "high"))
         (id (getf suggestion :id)))
    (is (string= "pending" (getf suggestion :status)))
    (sibyl.repl::update-suggestion-status id "approved")
    (is (string= "approved" (getf (sibyl.repl::get-suggestion-by-id id) :status)))))

(test store-suggestions-batch
  "Test that store-suggestions can store multiple suggestions."
  (sibyl.repl::clear-suggestions)
  (sibyl.repl::store-suggestions 
   (list (list :description "Feature 1" :rationale "Reason 1" :priority "high")
         (list :description "Feature 2" :rationale "Reason 2" :priority "medium")))
  (is (= 2 (length sibyl.repl::*pending-suggestions*))))

(test clear-suggestions-empties-list
  "Test that clear-suggestions removes all suggestions."
  (sibyl.repl::clear-suggestions)
  (sibyl.repl::store-suggestion "Feature" "Reason" "high")
  (is (> (length sibyl.repl::*pending-suggestions*) 0))
  (sibyl.repl::clear-suggestions)
  (is (= 0 (length sibyl.repl::*pending-suggestions*))))

;;; ============================================================
;;; /review argument parsing tests
;;; ============================================================

(test parse-review-args-no-action
  "Test parsing /review with no arguments (list all)."
  (multiple-value-bind (action id modification)
      (sibyl.repl::parse-review-args "/review")
    (is (null action))
    (is (null id))
    (is (null modification))))

(test parse-review-args-approve
  "Test parsing /review approve <id>."
  (multiple-value-bind (action id modification)
      (sibyl.repl::parse-review-args "/review approve 42")
    (is (string= "approve" action))
    (is (= 42 id))
    (is (null modification))))

(test parse-review-args-reject
  "Test parsing /review reject <id>."
  (multiple-value-bind (action id modification)
      (sibyl.repl::parse-review-args "/review reject 7")
    (is (string= "reject" action))
    (is (= 7 id))
    (is (null modification))))

(test parse-review-args-modify
  "Test parsing /review modify <id> <description>."
  (multiple-value-bind (action id modification)
      (sibyl.repl::parse-review-args "/review modify 3 add unit tests too")
    (is (string= "modify" action))
    (is (= 3 id))
    (is (string= "add unit tests too" modification))))

(test parse-review-args-invalid-id
  "Test parsing /review with invalid (non-numeric) ID."
  (multiple-value-bind (action id modification)
      (sibyl.repl::parse-review-args "/review approve notanumber")
    (is (string= "approve" action))
    (is (null id))
    (is (null modification))))

(test parse-review-args-whitespace
  "Test that parse-review-args handles extra whitespace."
  (multiple-value-bind (action id modification)
      (sibyl.repl::parse-review-args "/review   approve   5   ")
    (is (string= "approve" action))
    (is (= 5 id))
    (is (null modification))))

;;; ============================================================
;;; Dynamic command dispatch tests
;;; ============================================================

(test command-dispatch-help
  "Test that :help command dispatches correctly."
  (let ((output (with-output-to-string (*standard-output*)
                  (sibyl.repl::handle-repl-command :help nil))))
    (is (search "Sibyl REPL commands" output :test #'string-equal))))

(test command-dispatch-tools
  "Test that :list-tools command dispatches correctly."
  (let ((agent (sibyl.agent:make-agent :client nil :name "Test")))
    (let ((output (with-output-to-string (*standard-output*)
                    (sibyl.repl::handle-repl-command :list-tools agent))))
      (is (search "Registered tools" output :test #'string-equal)))))

(test command-dispatch-reset
  "Test that :reset command dispatches correctly."
  (let ((agent (sibyl.agent:make-agent :client nil :name "Test")))
    (let ((output (with-output-to-string (*standard-output*)
                    (sibyl.repl::handle-repl-command :reset agent))))
      (is (search "Conversation reset" output :test #'string-equal)))))

(test command-dispatch-history
  "Test that :history command dispatches correctly."
  (let ((agent (sibyl.agent:make-agent :client nil :name "Test")))
    (let ((output (with-output-to-string (*standard-output*)
                    (sibyl.repl::handle-repl-command :history agent))))
      (is (search "Conversation history" output :test #'string-equal)))))

(test command-dispatch-quit
  "Test that :quit command dispatches correctly and returns :quit."
  ;; Test return value
  (let ((output (with-output-to-string (*standard-output*)
                  (let ((result (sibyl.repl::handle-repl-command :quit nil)))
                    (is (eq :quit result))))))
    ;; Verify output contains goodbye message
    (is (search "Goodbye" output :test #'string-equal))))

(test command-dispatch-unknown
  "Test that unknown commands are handled gracefully."
  (let ((output (with-output-to-string (*standard-output*)
                  (sibyl.repl::handle-repl-command :nonexistent nil))))
    (is (search "Unknown command" output :test #'string-equal))))

(test dynamic-command-registration
  "Test that dynamically registered commands can be dispatched."
  (let ((original-commands sibyl.repl::*command-handlers*)
        (test-executed nil))
    (unwind-protect
         (progn
           ;; Register a test command
           (push (cons :test-dynamic
                       (lambda (agent input)
                         (declare (ignore agent input))
                         (setf test-executed t)
                         "TEST-EXECUTED"))
                 sibyl.repl::*command-handlers*)
           
           ;; Dispatch the test command
           (sibyl.repl::handle-repl-command :test-dynamic nil)
           
           ;; Verify it was executed
           (is (eq t test-executed)))
      
      ;; Cleanup: restore original commands
      (setf sibyl.repl::*command-handlers* original-commands))))

;;; ============================================================
;;; register-command tool tests
;;; ============================================================

(def-suite register-command-tests
  :description "Tests for the register-command tool."
  :in sibyl-tests)

(in-suite register-command-tests)

(test register-command-adds-to-handlers
  "Test that register-command adds a new command to *command-handlers*."
  (let ((original-commands sibyl.repl::*command-handlers*))
    (unwind-protect
         (progn
           ;; Register a new command via the tool
           (sibyl.tools:execute-tool
            "register-command"
            (list (cons "name" "test-rc-cmd")
                  (cons "description" "A test command")
                  (cons "handler-body" "(lambda (agent input) (declare (ignore agent input)) \"test-rc-result\")")))
           ;; Verify it appears in *command-handlers*
           (let ((entry (assoc :test-rc-cmd sibyl.repl::*command-handlers*)))
             (is (not (null entry)))
             (is (eq :test-rc-cmd (car entry)))
             (is (functionp (sibyl.repl::command-entry-handler (cdr entry))))))
      ;; Cleanup
      (setf sibyl.repl::*command-handlers* original-commands))))

(test register-command-handler-is-callable
  "Test that the registered handler can be called via funcall."
  (let ((original-commands sibyl.repl::*command-handlers*))
    (unwind-protect
         (progn
           ;; Register a command that returns a known value
           (sibyl.tools:execute-tool
            "register-command"
            (list (cons "name" "test-rc-callable")
                  (cons "description" "A callable test command")
                  (cons "handler-body" "(lambda (agent input) (declare (ignore agent input)) \"callable-result\")")))
           ;; Find the handler and call it
           (let* ((entry (assoc :test-rc-callable sibyl.repl::*command-handlers*))
                  (handler (sibyl.repl::command-entry-handler (cdr entry)))
                  (result (funcall handler nil nil)))
             (is (not (null entry)))
             (is (string= "callable-result" result))))
      ;; Cleanup
      (setf sibyl.repl::*command-handlers* original-commands))))

(test register-command-invalid-handler-body
  "Test that an invalid handler-body signals a tool-execution-error."
  (signals sibyl.conditions:tool-execution-error
    (sibyl.tools:execute-tool
     "register-command"
     (list (cons "name" "test-rc-invalid")
           (cons "description" "Invalid handler")
           (cons "handler-body" "(not-a-lambda x y z")))))

(test register-command-non-lambda-handler-body
  "Test that a non-lambda handler-body signals a tool-execution-error."
  (signals sibyl.conditions:tool-execution-error
    (sibyl.tools:execute-tool
     "register-command"
     (list (cons "name" "test-rc-nonlambda")
           (cons "description" "Non-lambda handler")
           (cons "handler-body" "(+ 1 2)")))))

(test register-command-empty-name-error
  "Test that an empty name signals a tool-execution-error."
  (signals sibyl.conditions:tool-execution-error
    (sibyl.tools:execute-tool
     "register-command"
     (list (cons "name" "")
           (cons "description" "Some description")
           (cons "handler-body" "(lambda (agent input) (declare (ignore agent input)) nil)")))))

(test register-command-duplicate-overwrites
  "Test that registering a duplicate command name overwrites the existing handler."
  (let ((original-commands sibyl.repl::*command-handlers*))
    (unwind-protect
         (progn
           ;; Register first version
           (sibyl.tools:execute-tool
            "register-command"
            (list (cons "name" "test-rc-dup")
                  (cons "description" "First version")
                  (cons "handler-body" "(lambda (agent input) (declare (ignore agent input)) \"first\")")))
           ;; Register second version (overwrite)
           (sibyl.tools:execute-tool
            "register-command"
            (list (cons "name" "test-rc-dup")
                  (cons "description" "Second version")
                  (cons "handler-body" "(lambda (agent input) (declare (ignore agent input)) \"second\")")))
           ;; The most recently registered handler should be first in the alist
           (let* ((entry (assoc :test-rc-dup sibyl.repl::*command-handlers*))
                  (handler (sibyl.repl::command-entry-handler (cdr entry)))
                  (result (funcall handler nil nil)))
             (is (not (null entry)))
             (is (string= "second" result))))
       ;; Cleanup
       (setf sibyl.repl::*command-handlers* original-commands))))

;;; ============================================================
;;; /tokens command tests
;;; ============================================================

(def-suite tokens-tests
  :description "Tests for /tokens command."
  :in sibyl-tests)

(in-suite tokens-tests)

(test tokens-command-registered
  "Test that /tokens command is registered in *repl-commands*."
  (is (assoc "/tokens" sibyl.repl::*repl-commands* :test #'string-equal))
  (is (eq :tokens (cdr (assoc "/tokens" sibyl.repl::*repl-commands*
                               :test #'string-equal)))))

(test tokens-command-recognized
  "Test that /tokens is recognized as a REPL command."
  (is (eq :tokens (sibyl.repl:repl-command-p "/tokens"))))

(test format-token-usage-contains-labels
  "Test that format-token-usage output contains expected labels."
  (let* ((tracker (sibyl.llm::make-token-tracker)))
    (sibyl.llm::tracker-add-usage tracker '(:input-tokens 1000 :output-tokens 200
                                             :cache-read-tokens 800 :cache-write-tokens 100))
    (let ((output (sibyl.repl::format-token-usage tracker)))
      (is (search "Input:" output :test #'string-equal))
      (is (search "Output:" output :test #'string-equal))
      (is (search "Cache Read:" output :test #'string-equal))
      (is (search "Cache Write:" output :test #'string-equal)))))

(test format-token-usage-shows-counts
  "Test that format-token-usage displays the correct token counts."
  (let* ((tracker (sibyl.llm::make-token-tracker)))
    (sibyl.llm::tracker-add-usage tracker '(:input-tokens 1234 :output-tokens 567
                                             :cache-read-tokens 8900 :cache-write-tokens 123))
    (let ((output (sibyl.repl::format-token-usage tracker)))
      ;; ~:d formats with commas: 1234 -> "1,234", 8900 -> "8,900"
      (is (search "1,234" output :test #'string-equal))
      (is (search "567" output :test #'string-equal))
      (is (search "8,900" output :test #'string-equal))
      (is (search "123" output :test #'string-equal)))))

(test tokens-command-dispatches
  "Test that :tokens command dispatches and prints token usage."
  (let* ((tracker (sibyl.llm::make-token-tracker))
         (agent (sibyl.agent:make-agent :client nil :name "Test")))
    (sibyl.llm::tracker-add-usage tracker '(:input-tokens 500 :output-tokens 100
                                              :cache-read-tokens 0 :cache-write-tokens 0))
    (setf (sibyl.agent:agent-token-tracker agent) tracker)
    ;; Muffle pricing warning: client is nil so model defaults to "unknown"
    (let ((output (with-output-to-string (*standard-output*)
                    (handler-bind ((warning #'muffle-warning))
                      (sibyl.repl::handle-repl-command :tokens agent)))))
      (is (search "Input" output :test #'string-equal))
      (is (search "Output" output :test #'string-equal)))))

(test help-includes-tokens
  "Test that /help output includes /tokens command."
  (let ((output (with-output-to-string (*standard-output*)
                  (sibyl.repl::handle-repl-command :help nil))))
    (is (search "/tokens" output :test #'string-equal))))

(test format-elapsed-time-with-tokens
  "Auto-generated test"
  
(let* ((sibyl.repl::*use-colors* nil)
       (result (with-output-to-string (s)
                 (sibyl.repl::format-elapsed-time 1.5 :stream s
                                                      :model "claude-sonnet"
                                                      :tokens 123))))
  (is (search "1.5s" result) "should contain elapsed time")
  (is (search "123" result) "should contain token count")
  (is (search "claude-sonnet" result) "should contain model name"))
)

(test tracker-delta-tokens
  "Auto-generated test"
  
(let* ((tracker (sibyl.llm::make-token-tracker)))
   ;; Add 100 tokens initially
   (sibyl.llm::tracker-add-usage tracker '(:input-tokens 100 :output-tokens 50
                                           :cache-read-tokens 0 :cache-write-tokens 0))
     (let ((before-total (+ (sibyl.llm::token-tracker-input-tokens tracker)
                          (sibyl.llm::token-tracker-output-tokens tracker))))
     ;; Add 200 more tokens
     (sibyl.llm::tracker-add-usage tracker '(:input-tokens 200 :output-tokens 80
                                             :cache-read-tokens 0 :cache-write-tokens 0))
    (let ((after-total (+ (sibyl.llm::token-tracker-input-tokens tracker)
                          (sibyl.llm::token-tracker-output-tokens tracker))))
      (is (= 150 before-total) "before: 100+50=150")
      (is (= 430 after-total) "after: 300+130=430")
      (is (= 280 (- after-total before-total)) "delta: 280 tokens"))))
)

(test format-tool-call-summary-shell
  "Auto-generated test"
  
(let ((tc (sibyl.llm:make-tool-call
           :name "shell"
           :arguments '(("command" . "ls -la src/") ("timeout" . 30)))))
  (is (string= "(ls -la src/)"
               (sibyl.repl::format-tool-call-summary tc))))
)

(test format-tool-call-summary-eval-form
   "Auto-generated test"
   
;; eval-form: Lisp forms are displayed as-is without parentheses
(let ((tc (sibyl.llm:make-tool-call
           :name "eval-form"
           :arguments '(("form" . "(defun foo () 42)") ("package" . "SIBYL")))))
  (is (string= "(defun foo () 42)"
               (sibyl.repl::format-tool-call-summary tc))))
)

(test format-tool-call-summary-grep
   "Auto-generated test"
   
;; grep: display both pattern and path
(let ((tc (sibyl.llm:make-tool-call
           :name "grep"
           :arguments '(("pattern" . "spinner") ("path" . "src/")))))
  (is (string= "(spinner src/)"
               (sibyl.repl::format-tool-call-summary tc))))
)

(test format-tool-call-summary-truncate
   "Auto-generated test"
   
;; Long arguments are truncated by max-length
(let ((tc (sibyl.llm:make-tool-call
           :name "shell"
           :arguments '(("command" . "find . -name '*.lisp' -exec grep -l 'defun' {} \\; | sort | head -20")))))
  (let ((result (sibyl.repl::format-tool-call-summary tc :max-length 20)))
    (is (> (length result) 0))
    (is (search "..." result))))
)

(test format-tool-call-summary-no-args
   "Auto-generated test"
   
;; Tools with no arguments return empty string
 (let ((tc (sibyl.llm:make-tool-call
            :name "shell"
            :arguments nil)))
  (is (string= "" (sibyl.repl::format-tool-call-summary tc))))
)

(def-suite repl-hooks-tests
  :description "Tests for REPL hook closures (tool-call, compact, etc.)."
  :in sibyl-tests)

(in-suite repl-hooks-tests)

(test make-tool-call-hook-force-output
   "Verify make-tool-call-hook output is immediately available"
   ;; Cannot mock force-output due to SBCL package lock constraints,
   ;; so verify indirectly that output is immediately available via with-output-to-string
   (let* ((output (with-output-to-string (s)
                   (let ((*standard-output* s)
                         (sibyl.repl::*current-spinner* nil)
                         (sibyl.repl::*use-colors* nil))
                     (let ((hook (sibyl.repl::make-tool-call-hook)))
                       (funcall hook (sibyl.llm:make-tool-call
                                      :name "shell"
                                      :arguments '(("command" . "echo test")))))))))
    (is (search "shell" output) "tool name must appear in output")
    (is (search "echo test" output) "command argument must appear in output"))
)

(test tool-call-hook-outputs-before-execution
   "Auto-generated test"
   ;; Verify hook outputs immediately when spinner is not active
(let* ((output (with-output-to-string (s)
                 (let ((*standard-output* s)
                       (sibyl.repl::*current-spinner* nil)
                       (sibyl.repl::*use-colors* nil))
                   (let ((hook (sibyl.repl::make-tool-call-hook)))
                     (let ((tc (sibyl.llm:make-tool-call
                                :id "test-id"
                                :name "update-plan-status"
                                :arguments '(("plan-id" . "plan-123")
                                             ("status" . "completed")))))
                       (funcall hook tc)))))))
  (is (search "update-plan-status" output)
      "tool name must appear in output immediately")
  (is (search "🔧" output)
      "tool execution indicator must appear immediately")))

(test make-compact-hook-captures-summary
  "make-compact-hook returns a callable that prints a compaction notice."
  (let ((hook-fn (sibyl.repl::make-compact-hook)))
    (is (functionp hook-fn) "make-compact-hook should return a function")
    ;; Invoke the hook and capture stdout
    (let* ((sibyl.repl::*current-spinner* nil)
           (sibyl.repl.display:*use-colors* nil)
           (output (with-output-to-string (*standard-output*)
                     (funcall hook-fn "test summary"))))
      ;; Clean up the spinner started by the hook
      (when sibyl.repl::*current-spinner*
        (sibyl.repl.spinner:stop-spinner sibyl.repl::*current-spinner*)
        (setf sibyl.repl::*current-spinner* nil))
      (is (search "Context compacted" output)
          "Invoking the hook should print a compaction notice"))))

(test make-compact-start-hook-prints-start-notice
  "make-compact-start-hook should print the compaction start notice."
  (let ((hook-fn (sibyl.repl::make-compact-start-hook)))
    (is (functionp hook-fn) "make-compact-start-hook should return a function")
    (let* ((sibyl.repl::*current-spinner* nil)
           (sibyl.repl.display:*use-colors* nil)
           (output (with-output-to-string (*standard-output*)
                     (funcall hook-fn))))
      (is (search "Context compacting" output)
          "Invoking the hook should print a start notice"))))

(test make-compact-start-hook-with-phase-label
  "make-compact-start-hook should include phase text when provided."
  (let ((hook-fn (sibyl.repl::make-compact-start-hook)))
    (let* ((sibyl.repl::*current-spinner* nil)
           (sibyl.repl.display:*use-colors* nil)
           (output (with-output-to-string (*standard-output*)
                     (funcall hook-fn "Phase 3: compacting conversation"))))
      (is (search "Phase 3: compacting conversation" output)
          "Phase label should be included in start notice"))))

(test command-entry-accessors
  "Auto-generated test"
  ;; plist形式のエントリからアクセサが正しく値を取得できる
(let ((entry (list :handler #'identity
                   :description "Test command"
                   :hidden nil)))
  (is (eq #'identity (sibyl.repl::command-entry-handler entry)))
  (is (string= "Test command" (sibyl.repl::command-entry-description entry)))
  (is (null (sibyl.repl::command-entry-hidden-p entry))))
;; hidden省略時のデフォルト
(let ((minimal (list :handler #'identity :description "Minimal")))
  (is (null (sibyl.repl::command-entry-hidden-p minimal))))
;; 後方互換: 生の関数もhandlerとして取得できる
(let ((legacy-entry #'identity))
  (is (eq #'identity (sibyl.repl::command-entry-handler legacy-entry)))
  (is (null (sibyl.repl::command-entry-description legacy-entry)))))

(test command-handlers-new-structure
  "Auto-generated test"
  ;; *command-handlers* の全エントリが新plist形式である
(dolist (pair sibyl.repl::*command-handlers*)
  (let ((key (car pair))
        (entry (cdr pair)))
    (is (keywordp key) (format nil "~a should be keyword" key))
    (is (listp entry) (format nil "~a entry should be plist" key))
    (is (functionp (sibyl.repl::command-entry-handler entry))
        (format nil "~a should have handler" key))
    (is (stringp (sibyl.repl::command-entry-description entry))
        (format nil "~a should have description" key)))))

(test colors-in-repl-commands
  "Auto-generated test"
  ;; /colors が *repl-commands* に登録されている
(is (assoc "/colors" sibyl.repl::*repl-commands* :test #'string-equal)
    "/colors should be in *repl-commands*")
;; コマンドとして認識される
(is (eq :colors (sibyl.repl:repl-command-p "/colors"))
    "/colors should be recognized as command"))

(test help-auto-generated-includes-all-visible
  "Auto-generated test"
  ;; /help の出力に、hidden でない全コマンドの説明が自動的に含まれる
(let ((help-output (with-output-to-string (*standard-output*)
                     (let ((sibyl.repl::*use-colors* nil))
                       (sibyl.repl::handle-help-command nil nil)))))
  ;; 以前表示されていなかった /cost-report が表示される
  (is (search "/cost-report" help-output)
      "/cost-report should appear in help")
  ;; /colors が表示される
  (is (search "/colors" help-output)
      "/colors should appear in help")
  ;; /help 自体が表示される
  (is (search "/help" help-output)
      "/help should appear in help")
  ;; 各コマンドの説明文も表示される
  (is (search "Show this help" help-output)
      "Description should appear")))

(test help-hides-hidden-commands
  "Auto-generated test"
  ;; hidden フラグが立ったコマンドはヘルプに表示されない
(let ((original sibyl.repl::*command-handlers*))
  (unwind-protect
       (progn
         (push (cons :test-hidden
                     (list :handler (lambda (a i) (declare (ignore a i)) nil)
                           :description "Secret command"
                           :hidden t))
               sibyl.repl::*command-handlers*)
         (let ((help-output (with-output-to-string (*standard-output*)
                              (let ((sibyl.repl::*use-colors* nil))
                                (sibyl.repl::handle-help-command nil nil)))))
           (is (null (search "Secret command" help-output))
               "Hidden command description should NOT appear in help")))
    (setf sibyl.repl::*command-handlers* original))))
