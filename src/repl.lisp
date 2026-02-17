;;;; repl.lisp — Interactive REPL interface for Sibyl
;;;; Provides a user-facing loop for conversing with the agent.

(in-package #:sibyl.repl)

;;; ============================================================
;;; REPL commands
;;; ============================================================

(defparameter *repl-commands*
  '(("/quit"    . :quit)
    ("/exit"    . :quit)
    ("/reset"   . :reset)
    ("/tools"   . :list-tools)
    ("/help"    . :help)
    ("/history" . :history)
    ("/improve" . :improve)
    ("/review"  . :review)
    ("/evolve"  . :evolve))
  "Mapping of REPL commands to actions.")

;;; ============================================================
;;; Suggestion state management
;;; ============================================================

(defvar *pending-suggestions* nil
  "List of pending improvement suggestions from suggest-improvements.
   Each suggestion is a plist with:
   - :id (integer)
   - :description (string) — what to improve
   - :rationale (string) — why it should be improved
   - :priority (string) — 'high', 'medium', or 'low'
   - :status (string) — 'pending', 'approved', 'rejected', 'modified'")

(defvar *next-suggestion-id* 1
  "Counter for generating unique suggestion IDs.")

(defun store-suggestion (description rationale priority)
  "Store a new improvement suggestion. Returns the created suggestion."
  (let ((suggestion (list :id *next-suggestion-id*
                          :description description
                          :rationale rationale
                          :priority priority
                          :status "pending")))
    (push suggestion *pending-suggestions*)
    (incf *next-suggestion-id*)
    suggestion))

(defun store-suggestions (suggestions-list)
  "Store multiple suggestions from a list of plists.
   Each plist should have :description, :rationale, and :priority."
  (dolist (suggestion suggestions-list)
    (store-suggestion (getf suggestion :description)
                      (getf suggestion :rationale)
                      (getf suggestion :priority))))

(defun get-suggestion-by-id (id)
  "Retrieve a suggestion by its ID. Returns NIL if not found."
  (find id *pending-suggestions* :key (lambda (s) (getf s :id))))

(defun update-suggestion-status (id new-status)
  "Update the status of a suggestion."
  (let ((suggestion (get-suggestion-by-id id)))
    (when suggestion
      (setf (getf suggestion :status) new-status)
      t)))

(defun clear-suggestions ()
  "Clear all pending suggestions."
  (setf *pending-suggestions* nil)
  (setf *next-suggestion-id* 1))

(defun repl-command-p (input)
  "Check if INPUT is a REPL command. Returns the command keyword or NIL.
   Handles commands with arguments by extracting the first word."
  (let* ((trimmed (string-trim '(#\Space #\Tab) input))
         ;; Extract first word (command part)
         (space-pos (position #\Space trimmed))
         (command-part (if space-pos
                          (subseq trimmed 0 space-pos)
                          trimmed)))
    (cdr (assoc command-part *repl-commands* :test #'string-equal))))

;;; ============================================================
;;; Command handler functions
;;; ============================================================

(defun handle-quit-command (agent input)
  "Handler for :quit command."
  (declare (ignore agent input))
  (format t "~%Goodbye.~%")
  :quit)

(defun handle-reset-command (agent input)
  "Handler for :reset command."
  (declare (ignore input))
  (sibyl.agent:agent-reset agent)
  (format t "~%[Conversation reset]~%")
  nil)

(defun handle-tools-command (agent input)
  "Handler for :list-tools command."
  (declare (ignore input))
  (let ((tools (sibyl.tools:list-tools)))
    (format t "~%Registered tools (~a):~%" (length tools))
    (dolist (tool tools)
      (format t "  ~a — ~a~%"
              (sibyl.tools:tool-name tool)
              (sibyl.tools:tool-description tool))))
  nil)

(defun handle-help-command (agent input)
  "Handler for :help command."
  (declare (ignore agent input))
  (format t "~%Sibyl REPL commands:~%")
  (format t "  /help     — Show this help~%")
  (format t "  /tools    — List registered tools~%")
  (format t "  /reset    — Reset conversation~%")
  (format t "  /history  — Show conversation history~%")
  (format t "  /improve  — Request self-improvement (TDD cycle)~%")
  (format t "  /review   — Review improvement suggestions~%")
  (format t "  /evolve   — Autonomous continuous improvement loop~%")
  (format t "  /quit     — Exit REPL~%")
  (format t "~%Type anything else to chat with the agent.~%")
  nil)

(defun handle-history-command (agent input)
  "Handler for :history command."
  (declare (ignore input))
  (let* ((mem (sibyl.agent:agent-memory agent))
         (messages (sibyl.llm:conversation-to-list
                    (sibyl.agent:memory-conversation mem))))
    (format t "~%Conversation history (~a messages):~%" (length messages))
    (dolist (msg messages)
      (format t "  [~a] ~a~%"
              (sibyl.llm:message-role msg)
              (sibyl.util:truncate-string
               (or (sibyl.llm:message-content msg) "(tool call)")
               120))))
  nil)

(defun handle-improve-command-wrapper (agent input)
  "Wrapper handler for :improve command."
  (handle-improve-command agent input)
  nil)

(defun handle-review-command-wrapper (agent input)
  "Wrapper handler for :review command."
  (handle-review-command agent input)
  nil)

(defun handle-evolve-command-wrapper (agent input)
  "Wrapper handler for :evolve command."
  (handle-evolve-command agent input)
  nil)

;;; Command handler registry
(defparameter *command-handlers*
  (list (cons :quit #'handle-quit-command)
        (cons :reset #'handle-reset-command)
        (cons :list-tools #'handle-tools-command)
        (cons :help #'handle-help-command)
        (cons :history #'handle-history-command)
        (cons :improve #'handle-improve-command-wrapper)
        (cons :review #'handle-review-command-wrapper)
        (cons :evolve #'handle-evolve-command-wrapper))
  "Mapping of command keywords to handler functions.")

(defun handle-repl-command (command agent &optional original-input)
  "Handle a REPL command. Returns :quit to exit, or NIL to continue.
   ORIGINAL-INPUT is the full command string including arguments.
   Uses dynamic dispatch via *command-handlers* alist."
  (let ((handler (cdr (assoc command *command-handlers*))))
    (if handler
        (funcall handler agent original-input)
        (progn
          (format t "~%Unknown command: ~a~%" command)
          nil))))

(defun parse-improve-args (input)
  "Parse /improve command arguments. Returns (values task-description auto-commit-p).
   Example: '/improve add feature --auto-commit' => ('add feature' T)"
  (let* ((trimmed (string-trim '(#\Space #\Tab) input))
         ;; Remove '/improve' prefix
         (args-str (if (search "/improve" trimmed :test #'string-equal)
                       (string-trim '(#\Space #\Tab)
                                    (subseq trimmed (length "/improve")))
                       trimmed))
         ;; Check for --auto-commit flag
         (auto-commit-pos (search "--auto-commit" args-str :test #'string-equal))
         (task-desc (if auto-commit-pos
                        (string-trim '(#\Space #\Tab)
                                     (subseq args-str 0 auto-commit-pos))
                        args-str)))
    (values task-desc (not (null auto-commit-pos)))))

(defun parse-review-args (input)
  "Parse /review command arguments. Returns (values action id modification).
   Examples:
   - '/review' => (NIL NIL NIL) - list all
   - '/review approve 1' => ('approve' 1 NIL)
   - '/review reject 2' => ('reject' 2 NIL)
   - '/review modify 3 \"add unit tests\"' => ('modify' 3 'add unit tests')"
  (let* ((trimmed (string-trim '(#\Space #\Tab) input))
         ;; Remove '/review' prefix
         (args-str (if (search "/review" trimmed :test #'string-equal)
                       (string-trim '(#\Space #\Tab)
                                    (subseq trimmed (length "/review")))
                       trimmed)))
    (if (string= "" args-str)
        ;; No arguments - list all
        (values nil nil nil)
        ;; Parse arguments
        (let* ((parts (cl-ppcre:split "\\s+" args-str :limit 3))
               (action (first parts))
               (id-str (second parts))
               (modification (third parts)))
          (values action
                  (when id-str
                    (handler-case
                        (parse-integer id-str)
                      (error () nil)))
                   modification)))))

(defun %evolve-normalize-priority (value)
  (when value
    (let ((normalized (string-downcase (string-trim '(#\Space #\Tab) value))))
      (when (member normalized '("high" "medium" "low") :test #'string=)
        normalized))))

(defun parse-evolve-args (input)
  "Parse /evolve command arguments. Returns (values max-cycles priority-threshold).
   Examples:
   - '/evolve' => (10 "medium")
   - '/evolve 5 high' => (5 "high")
   - '/evolve max-cycles=3 priority=low' => (3 "low")"
  (let* ((trimmed (string-trim '(#\Space #\Tab) (or input "")))
         (args-str (if (search "/evolve" trimmed :test #'string-equal)
                       (string-trim '(#\Space #\Tab)
                                    (subseq trimmed (length "/evolve")))
                       trimmed))
         (parts (if (string= args-str "")
                    nil
                    (cl-ppcre:split "\\s+" args-str)))
         (max-cycles nil)
         (priority nil))
    (dolist (part parts)
      (let* ((clean (string-trim '(#\Space #\Tab) part))
             (token (string-left-trim '(#\-) clean)))
        (cond
          ((search "=" token)
           (let* ((pair (cl-ppcre:split "=" token :limit 2))
                  (key (string-downcase (first pair)))
                  (value (second pair)))
             (cond
               ((member key '("max-cycles" "cycles" "max") :test #'string=)
                (let ((parsed (ignore-errors (parse-integer value :junk-allowed t))))
                  (when parsed
                    (setf max-cycles parsed))))
               ((member key '("priority" "priority-threshold" "min-priority" "threshold")
                        :test #'string=)
                (let ((parsed (%evolve-normalize-priority value)))
                  (when parsed
                    (setf priority parsed)))))))
          (t
           (let ((parsed-int (ignore-errors (parse-integer token :junk-allowed t)))
                 (parsed-priority (%evolve-normalize-priority token)))
             (when (and parsed-int (null max-cycles))
               (setf max-cycles parsed-int))
             (when (and parsed-priority (null priority))
               (setf priority parsed-priority)))))))
    (values (max 0 (or max-cycles 10))
            (or priority "medium"))))

(defun %evolve-parse-json (json)
  (handler-case
      (yason:parse json :object-as :hash-table)
    (error () nil)))

(defun %evolve-ensure-list (value)
  (cond
    ((vectorp value) (coerce value 'list))
    ((null value) nil)
    (t value)))

(defun %evolve-format-location (file line)
  (cond
    ((and file (integerp line))
     (format nil "~a:~a" file line))
    (file file)
    (t "unknown")))

(defun %evolve-build-prompt (suggestion)
  (let* ((description (or (gethash "description" suggestion) ""))
         (rationale (or (gethash "rationale" suggestion) ""))
         (priority (or (gethash "priority" suggestion) "medium"))
         (category (or (gethash "category" suggestion) "unknown"))
         (file (gethash "file" suggestion))
         (line (gethash "line" suggestion))
         (location (%evolve-format-location file line)))
    (format nil
            "Improve Sibyl autonomously based on this suggestion:~%~%~
             Description: ~a~%~
             Rationale: ~a~%~
             Priority: ~a~%~
             Category: ~a~%~
             Location: ~a~%~%~
             Follow the TDD workflow strictly:~%~
             1. UNDERSTAND: use read-sexp and who-calls to inspect current code~%~
             2. RED: write a failing test using write-test; confirm with run-tests~%~
             3. GREEN: implement using safe-redefine; re-run run-tests~%~
             4. REFACTOR if needed; re-run run-tests~%~
             5. PERSIST: use sync-to-file to write changes to disk and unprotect file~%~%~
             Constraints: stay within sibyl.* packages, compile new code, do not modify system prompt."
            description rationale priority category location)))

(defun list-pending-suggestions ()
  "Display all pending suggestions."
  (if (null *pending-suggestions*)
      (format t "~%No pending suggestions.~%")
      (progn
        (format t "~%Pending improvement suggestions:~%~%")
        (dolist (suggestion (reverse *pending-suggestions*))
          (format t "  [~a] ~a (Priority: ~a)~%"
                  (getf suggestion :id)
                  (getf suggestion :description)
                  (getf suggestion :priority))
          (format t "      Rationale: ~a~%"
                  (getf suggestion :rationale))
          (format t "      Status: ~a~%~%"
                  (getf suggestion :status))))))

(defun approve-and-implement (agent id)
  "Approve suggestion and delegate to /improve command."
  (let ((suggestion (get-suggestion-by-id id)))
    (if (null suggestion)
        (format t "~%Error: Suggestion #~a not found.~%" id)
        (progn
          (update-suggestion-status id "approved")
          (format t "~%Approving and implementing suggestion #~a:~%" id)
          (format t "  ~a~%~%" (getf suggestion :description))
          ;; Call /improve with the suggestion description
          (handle-improve-command agent 
                                  (format nil "/improve ~a" 
                                          (getf suggestion :description)))))))

(defun reject-suggestion (id)
  "Reject a suggestion."
  (let ((suggestion (get-suggestion-by-id id)))
    (if (null suggestion)
        (format t "~%Error: Suggestion #~a not found.~%" id)
        (progn
          (update-suggestion-status id "rejected")
          (format t "~%Rejected suggestion #~a.~%" id)))))

(defun modify-and-approve (agent id modification)
  "Modify a suggestion and approve it."
  (let ((suggestion (get-suggestion-by-id id)))
    (if (null suggestion)
        (format t "~%Error: Suggestion #~a not found.~%" id)
        (progn
          (update-suggestion-status id "modified")
          (format t "~%Modifying and implementing suggestion #~a:~%" id)
          (format t "  Original: ~a~%" (getf suggestion :description))
          (format t "  Modified: ~a~%~%" modification)
          ;; Call /improve with the modified description
          (handle-improve-command agent
                                  (format nil "/improve ~a" modification))))))

(defun handle-review-command (agent original-input)
  "Handle /review command for managing improvement suggestions.
   
   Usage:
   - /review                    — List all pending suggestions
   - /review approve <id>       — Approve and implement suggestion
   - /review reject <id>        — Reject suggestion
   - /review modify <id> <desc> — Modify and implement suggestion"
  (multiple-value-bind (action id modification)
      (parse-review-args original-input)
    
    (cond
      ;; No action: list all suggestions
      ((null action)
       (list-pending-suggestions))
      
      ;; Approve
      ((string-equal action "approve")
       (if (null id)
           (format t "~%Error: No suggestion ID provided.~%~
                       Usage: /review approve <id>~%")
           (approve-and-implement agent id)))
      
      ;; Reject
      ((string-equal action "reject")
       (if (null id)
           (format t "~%Error: No suggestion ID provided.~%~
                       Usage: /review reject <id>~%")
           (reject-suggestion id)))
      
      ;; Modify
      ((string-equal action "modify")
       (cond
         ((null id)
          (format t "~%Error: No suggestion ID provided.~%~
                      Usage: /review modify <id> <description>~%"))
         ((null modification)
          (format t "~%Error: No modification description provided.~%~
                      Usage: /review modify <id> <description>~%"))
         (t
          (modify-and-approve agent id modification))))
      
      ;; Unknown action
      (t
       (format t "~%Error: Unknown action '~a'.~%~
                   Valid actions: approve, reject, modify~%~
                   Usage: /review [approve|reject|modify] <id> [description]~%"
               action)))))

(defun handle-improve-command (agent original-input)
  "Handle /improve command for self-improvement tasks.
   Orchestrates full TDD cycle for requested improvement.
   
   Usage: /improve <task description> [--auto-commit]"
  (multiple-value-bind (task-desc auto-commit-p)
      (parse-improve-args original-input)
    
    ;; Validate task description
    (when (string= "" task-desc)
      (format t "~%Error: No task description provided.~%")
      (format t "Usage: /improve <task description> [--auto-commit]~%")
      (return-from handle-improve-command))
    
    ;; Build TDD prompt for agent
    (let ((prompt (format nil 
                    "Improve Sibyl's code: ~a~%~%~
                     Follow the TDD workflow strictly:~%~
                     1. UNDERSTAND: Use read-sexp and who-calls to understand current code~%~
                     2. RED: Write test first using write-test → confirm it fails with run-tests~%~
                     3. GREEN: Implement using safe-redefine → confirm test passes with run-tests~%~
                     4. REFACTOR (if needed): Clean up code → run-tests to ensure still green~%~
                     ~:[5. WAIT: Do NOT use sync-to-file yet - report results and wait for human approval~;~
                     5. PERSIST: Use sync-to-file to write changes to disk~]"
                    task-desc auto-commit-p)))
      
      (format t "~%[Starting self-improvement task]~%")
      (format t "Task: ~a~%" task-desc)
      (format t "Auto-commit: ~:[NO~;YES~]~%~%" auto-commit-p)
      
      ;; Execute agent step with TDD prompt
      (handler-case
          (let ((response (sibyl.agent:agent-run agent prompt)))
            (format t "~%~a~%~%" response)
            
            ;; If not auto-commit, ask for confirmation
            (unless auto-commit-p
              (format t "~%Do you want to commit these changes? (y/n): ")
              (force-output)
              (let ((confirmation (read-line *standard-input* nil nil)))
                (when (and confirmation 
                           (string-equal (string-trim '(#\Space #\Tab) confirmation) "y"))
                  (format t "~%Committing changes to disk...~%")
                  (let ((sync-response
                          (sibyl.agent:agent-run agent 
                            "Please use sync-to-file to persist all changes to disk, then use sibyl.system:unprotect-file.")))
                    (format t "~%~a~%~%" sync-response))))))
        
        (sibyl.conditions:llm-error (e)
          (format t "~%[LLM Error: ~a]~%~%" e))
        (error (e)
          (format t "~%[Error: ~a]~%~%" e))))))

(defun handle-evolve-command (agent original-input)
  "Handle /evolve command for autonomous continuous improvement.
   Usage: /evolve [max-cycles] [priority]
          /evolve max-cycles=5 priority=high"
  (multiple-value-bind (max-cycles priority-threshold)
      (parse-evolve-args original-input)
    (let ((cycles-run 0)
          (productive-cycles 0)
          (total-succeeded 0)
          (total-skipped 0)
          (consecutive-skip-cycles 0)
          (baseline-tests 0)
          (current-tests 0)
          (baseline-tools (length (sibyl.tools:list-tools)))
          (current-tools (length (sibyl.tools:list-tools))))
      (sibyl.tools:evolution-state-init)
      (when (<= max-cycles 0)
        (sibyl.tools:evolution-report-final-summary 0 0 0 0 baseline-tests current-tests)
        (return-from handle-evolve-command))
      (block evolve-loop
        (loop for cycle from 1 to max-cycles do
          (setf cycles-run cycle)
          (setf current-tools (length (sibyl.tools:list-tools)))
          (sibyl.tools:evolution-report-cycle-start cycle max-cycles)
          (let* ((suggestions-json
                   (sibyl.tools:execute-tool
                    "suggest-improvements"
                    (list (cons "min-priority" priority-threshold)
                          (cons "exclude-attempted" "true"))))
                 (parsed (%evolve-parse-json suggestions-json))
                 (suggestions (%evolve-ensure-list
                               (and parsed (gethash "suggestions" parsed))))
                 (suggestion-count (length suggestions)))
            (when (or (null suggestions) (zerop suggestion-count))
              (format t "~%No new suggestions. Stopping.~%")
              (return-from evolve-loop))
            (incf productive-cycles)
            (let ((cycle-succeeded 0)
                  (cycle-skipped 0))
              (loop for suggestion in suggestions
                    for index from 1 do
                      (let* ((name (or (gethash "description" suggestion) "(unnamed)"))
                             (prompt (%evolve-build-prompt suggestion)))
                        (sibyl.tools:evolution-report-improvement-start index suggestion-count name)
                        (handler-case
                            (let ((response (sibyl.agent:agent-run agent prompt)))
                              (sibyl.tools:evolution-state-record-attempt name "success")
                              (incf cycle-succeeded)
                              (incf total-succeeded)
                              (sibyl.tools:evolution-report-improvement-result t)
                              (when response
                                (format t "~%~a~%" response)))
                          (sibyl.conditions:llm-error (e)
                            (sibyl.tools:evolution-state-record-attempt name (format nil "failed: ~a" e))
                            (incf cycle-skipped)
                            (incf total-skipped)
                            (format t "~%[LLM Error: ~a]~%" e)
                            (sibyl.tools:evolution-report-improvement-result nil))
                          (error (e)
                            (sibyl.tools:evolution-state-record-attempt name (format nil "failed: ~a" e))
                            (incf cycle-skipped)
                            (incf total-skipped)
                            (format t "~%[Error: ~a]~%" e)
                            (sibyl.tools:evolution-report-improvement-result nil)))))
              (let* ((tests-json (sibyl.tools:execute-tool "run-tests" nil))
                     (tests-parsed (%evolve-parse-json tests-json))
                     (failed (or (and tests-parsed (gethash "failed" tests-parsed)) 0))
                     (total (or (and tests-parsed (gethash "total" tests-parsed)) current-tests)))
                (setf current-tests total)
                (setf current-tools (length (sibyl.tools:list-tools)))
                (when (and (integerp failed) (> failed 0))
                  (format t "~%Test regression detected. Stopping.~%")
                  (sibyl.tools:evolution-report-cycle-summary
                   cycle-succeeded cycle-skipped baseline-tests current-tests baseline-tools current-tools)
                  (return-from evolve-loop))
                (sibyl.tools:evolution-report-cycle-summary
                 cycle-succeeded cycle-skipped baseline-tests current-tests baseline-tools current-tools)
                (setf baseline-tests current-tests)
                (setf baseline-tools current-tools)
                (if (zerop cycle-succeeded)
                    (incf consecutive-skip-cycles)
                    (setf consecutive-skip-cycles 0))
                (when (>= consecutive-skip-cycles 3)
                  (format t "~%Three consecutive skip cycles. Stopping.~%")
                  (return-from evolve-loop)))))))
      (sibyl.tools:evolution-report-final-summary
       cycles-run productive-cycles total-succeeded total-skipped baseline-tests current-tests)
      nil)))

;;; ============================================================
;;; Main REPL loop
;;; ============================================================

(defun print-banner ()
  "Print the Sibyl welcome banner."
  (format t "~%~
╔═══════════════════════════════════════╗~%~
║          S I B Y L  v0.1.0           ║~%~
║     Lisp-based Coding Agent          ║~%~
╚═══════════════════════════════════════╝~%~
~%Type /help for commands, or start chatting.~%~%"))

(defun read-user-input ()
  "Read a line of input from the user. Returns NIL on EOF."
  (format t "~&sibyl> ")
  (force-output)
  (read-line *standard-input* nil nil))

(defun start-repl (&key client
                     (system-prompt sibyl.agent::*default-system-prompt*)
                     (name "Sibyl"))
  "Start the interactive REPL.

   Usage:
     (sibyl:with-config ()
       (sibyl:start-repl :client (sibyl:make-anthropic-client)))

   Or with an existing agent:
     (start-repl :client my-client)"
  (let ((agent (sibyl.agent:make-agent
                :client client
                :name name
                :system-prompt system-prompt)))
    (print-banner)
    (block repl-loop
      (tagbody
       next-iteration
         (let ((input (read-user-input)))
           ;; EOF
           (unless input
             (format t "~%Goodbye.~%")
             (return-from repl-loop))
           ;; Empty input
           (when (string= (string-trim '(#\Space #\Tab) input) "")
             (go next-iteration))
            ;; REPL command
            (let ((cmd (repl-command-p input)))
              (when cmd
                (when (eq (handle-repl-command cmd agent input) :quit)
                  (return-from repl-loop))
                (go next-iteration)))
           ;; Agent interaction
           (handler-case
               (let ((response (sibyl.agent:agent-run agent input)))
                 (format t "~%~a~%~%" response))
             (sibyl.conditions:llm-error (e)
               (format t "~%[LLM Error: ~a]~%~%" e))
             (error (e)
               (format t "~%[Error: ~a]~%~%" e))))
         (go next-iteration)))))
