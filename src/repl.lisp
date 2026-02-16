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
    ("/review"  . :review))
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

(defun handle-repl-command (command agent &optional original-input)
  "Handle a REPL command. Returns :quit to exit, or NIL to continue.
   ORIGINAL-INPUT is the full command string including arguments."
  (case command
    (:quit
     (format t "~%Goodbye.~%")
     :quit)
    (:reset
     (sibyl.agent:agent-reset agent)
     (format t "~%[Conversation reset]~%")
     nil)
    (:list-tools
     (let ((tools (sibyl.tools:list-tools)))
       (format t "~%Registered tools (~a):~%" (length tools))
       (dolist (tool tools)
         (format t "  ~a — ~a~%"
                 (sibyl.tools:tool-name tool)
                 (sibyl.tools:tool-description tool))))
     nil)
    (:help
     (format t "~%Sibyl REPL commands:~%")
     (format t "  /help     — Show this help~%")
     (format t "  /tools    — List registered tools~%")
     (format t "  /reset    — Reset conversation~%")
     (format t "  /history  — Show conversation history~%")
     (format t "  /improve  — Request self-improvement (TDD cycle)~%")
     (format t "  /review   — Review improvement suggestions~%")
     (format t "  /quit     — Exit REPL~%")
     (format t "~%Type anything else to chat with the agent.~%")
     nil)
    (:history
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
    (:improve
     (handle-improve-command agent original-input)
     nil)
    (:review
     (handle-review-command agent original-input)
     nil)))

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
