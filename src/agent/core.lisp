;;;; core.lisp — Agent core: the observe→think→act loop
;;;; This is the main agent loop that drives Sibyl.

(in-package #:sibyl.agent)

;;; ============================================================
;;; Agent class
;;; ============================================================

(defclass agent ()
  ((name          :initarg :name
                  :accessor agent-name
                  :initform "Sibyl"
                  :type string)
   (client        :initarg :client
                  :accessor agent-client
                  :documentation "An llm-client instance.")
   (memory        :initarg :memory
                  :accessor agent-memory
                  :initform (make-memory))
   (system-prompt :initarg :system-prompt
                  :accessor agent-system-prompt
                  :initform *default-system-prompt*
                  :type string)
   (max-steps     :initarg :max-steps
                  :accessor agent-max-steps
                  :initform 50
                  :type integer)
   (step-count    :initarg :step-count
                  :accessor agent-step-count
                  :initform 0
                  :type integer)
    (hooks         :initarg :hooks
                    :accessor agent-hooks
                    :initform nil
                    :documentation "Alist of hook-name → function.
                     Hooks: :before-step :after-step :on-tool-call :on-tool-result :on-error")
   (token-tracker :initform (sibyl.llm::make-token-tracker)
                  :accessor agent-token-tracker
                  :documentation "Cumulative token usage for this session.")
   (cost-records :initform nil
                 :accessor agent-cost-records
                 :documentation "List of task-cost-record instances for this session."))
  (:documentation "The central agent that orchestrates LLM and tools."))

;;; ============================================================
;;; Default system prompt
;;; ============================================================

(defparameter *default-system-prompt*
  "You are Sibyl, a coding agent implemented in Common Lisp.
You help users by reading, writing, and modifying code.

**SELF-AWARENESS**: You have the ability to access, understand, and modify your own
source code. You are a Lisp agent running in SBCL with introspective capabilities.

**Lisp-Aware Tools**:
You have access to specialized tools for working with Lisp code:
- read-sexp: Parse Lisp source files as S-expressions
- describe-symbol: Introspect functions, variables, classes, and macros
- eval-form: Safely evaluate Lisp expressions with auto-compilation
- macroexpand-form: Expand macros to understand their structure
- package-symbols: List symbols in any package
- who-calls: Analyze function call relationships
- codebase-map: Map your entire codebase architecture

Use these tools to understand and work with Lisp code at a structural level,
not just as text. When modifying code, consider S-expression structure,
package boundaries, and symbol visibility.

**TDD Workflow for Self-Modification**:
When modifying your own code, follow this test-driven development cycle:

1. **RED**: Write the test first
   - Use write-test tool to create a failing test
   - Run with run-tests to confirm it fails

2. **GREEN**: Implement minimal solution
   - Use safe-redefine to modify the function in-memory
   - Run-tests again to verify the test passes

3. **REFACTOR**: Improve while maintaining green
   - Use safe-redefine to clean up the implementation
   - Run-tests to ensure tests still pass

4. **PERSIST**: Save changes to disk
   - Use sync-to-file to write changes to source files
   - Use sibyl.system:unprotect-file to allow future reloads

Available TDD tools:
- write-test: Generate and register FiveAM tests
- run-tests: Execute tests and get structured results
- safe-redefine: Modify functions in-memory with rollback
- sync-to-file: Persist in-memory changes to source files

Never modify code without writing tests first. The TDD cycle ensures
your self-modifications are correct and maintainable.

You also have access to tools for file operations, shell commands, and search.

When you need to perform an action, use the appropriate tool.
Think step by step. Be precise and concise.
If you are unsure, ask for clarification."
  "Default system prompt for the agent.")

(defparameter *compact-system-prompt*
  "You are Sibyl, a Common Lisp coding agent running in SBCL.
You read, write, and modify code using tools.

Key tools: read-file, write-file, shell, grep, eval-form, describe-symbol,
read-sexp, who-calls, safe-redefine, run-tests, write-test, sync-to-file.

When modifying code: write test first, implement, verify tests pass, persist.
Be precise and concise. Use tools for actions."
  "Compact system prompt for local/smaller models to reduce input token count.")

(defun select-ollama-system-prompt (model-name)
  "Select an optimized system prompt for an Ollama model.
Large models (profile :large-model t) use the full *default-system-prompt*
since they have sufficient context window. Smaller models use the compact
prompt with an optional system-prompt-hint prefix to save tokens."
  (let ((profile (sibyl.llm::lookup-model-profile model-name)))
    (cond
      ;; Large models (e.g. gpt-oss:120b) — use full prompt
      ((and profile (getf profile :large-model))
       *default-system-prompt*)
      ;; Small models with hint — hint + compact
      ((and profile (getf profile :system-prompt-hint))
       (format nil "~a~%~%~a"
               (getf profile :system-prompt-hint)
               *compact-system-prompt*))
      ;; Fallback — compact only
      (t *compact-system-prompt*))))


;;; ============================================================
;;; Dynamic agent context
;;; ============================================================

(defvar *current-agent* nil
  "Dynamically bound to the currently-executing agent during agent-step.
   Tools can read this to access the agent's client, memory, etc.")

;;; ============================================================
;;; Agent construction
;;; ============================================================

(defun make-agent (&key client (name "Sibyl") (system-prompt *default-system-prompt*)
                   (max-steps 50) (max-memory-messages 50))
  "Create a new agent instance."
  (let* ((agent (make-instance 'agent
                  :client client
                  :name name
                  :system-prompt system-prompt
                  :max-steps max-steps
                  :memory (make-memory :max-messages max-memory-messages))))
    ;; Propagate the LLM client to memory so :llm compaction strategy works
    (when client
      (setf (memory-compaction-client (agent-memory agent)) client)
      ;; Set context window size for token-based proactive compaction
      ;; Guard: non-llm-client subclasses (e.g. test mocks) may lack the model slot
      (when (typep client 'sibyl.llm:llm-client)
        (let ((model-name (ignore-errors (client-model client))))
          (when model-name
            (setf (memory-context-window-size (agent-memory agent))
                  (sibyl.llm:context-window-for-model model-name))))))
    ;; Wire compaction callback → :on-compact agent hook
    (setf (memory-compaction-callback (agent-memory agent))
          (lambda (summary)
            (run-hook agent :on-compact summary)))
    agent))

;;; ============================================================
;;; Hook system
;;; ============================================================

(defun run-hook (agent hook-name &rest args)
  "Run a named hook if registered. Hooks are advisory; errors are caught."
  (let ((hook-fn (cdr (assoc hook-name (agent-hooks agent)))))
    (when hook-fn
      (handler-case (apply hook-fn args)
        (error (e)
          (log-warn "agent" "Hook ~a error: ~a" hook-name e))))))

(defun %execute-tool-with-timing (agent tc)
  "Execute tool call TC with timing measurement and :on-tool-result hook.
Returns the result string."
  (let* ((start (get-internal-real-time))
         (result (handler-case (execute-tool-call tc)
                   (tool-error (e)
                     (run-hook agent :on-error e)
                     (format nil "Error: ~a" e))))
         (end (get-internal-real-time))
         (elapsed-seconds (/ (- end start) internal-time-units-per-second)))
    (run-hook agent :on-tool-result tc result elapsed-seconds)
    result))

;;; ============================================================
;;; Tool category heuristic
;;; ============================================================

(defun infer-tool-categories (user-input)
  "Infer relevant tool categories from user input using simple heuristics.
Always includes :general. Adds :file if input mentions file paths or file ops.
Adds :code if input has code-related keywords. Adds :analysis for analysis keywords."
  (let ((cats '(:general)))
    (when (and user-input (stringp user-input))
      ;; File-related: file extensions or file operation keywords
      (when (or (cl-ppcre:scan "\\.(lisp|py|js|ts|rb|go|rs|c|h|cpp|java|txt|json|yaml)" user-input)
                (cl-ppcre:scan "(?i)\\bfile\\b|\\bpath\\b|\\bread\\b|\\bwrite\\b|\\bdirectory\\b" user-input))
        (pushnew :file cats))
      ;; Code-related: Lisp forms or code keywords
      (when (cl-ppcre:scan "(?i)\\bdefun\\b|\\bdefmethod\\b|\\bcode\\b|\\bfunction\\b|\\bclass\\b|\\brepl\\b|\\beval\\b|\\bsymbol\\b|\\bpackage\\b|\\bmacro\\b" user-input)
        (pushnew :code cats))
      ;; Analysis-related
      (when (cl-ppcre:scan "(?i)\\banalyze\\b|\\banalysis\\b|\\bsearch\\b|\\bfind\\b|\\bgrep\\b|\\btest\\b|\\bassess\\b|\\bimprove\\b" user-input)
        (pushnew :analysis cats)))
    cats))

;;; ============================================================
;;; Context overflow recovery
;;; ============================================================

(defun %prompt-too-long-p (condition)
  "Return T if CONDITION is a prompt-too-long error from the LLM API."
  (and (typep condition 'llm-api-error)
       (eql 400 (llm-error-status-code condition))
       (let ((body (llm-error-body condition)))
         (and body
              (search "too long"
                      (string-downcase (princ-to-string body)))))))

(defun %call-with-context-recovery (agent make-call-fn)
  "Call MAKE-CALL-FN, applying 3-phase recovery on prompt-too-long errors.
MAKE-CALL-FN is a zero-argument function that builds context from current
memory state and calls the LLM.  It is called fresh after each recovery
phase so that context reflects the reduced memory.
Returns (values response usage) on success."
  (let* ((mem (agent-memory agent))
         (recovery-phases
           (list
            ;; Phase 1: Deduplicate tool results
            (cons "Phase 1: deduplicating tool results"
                  (lambda () (memory-deduplicate-tool-results mem)))
            ;; Phase 2: Aggressively truncate largest outputs
            (cons "Phase 2: truncating largest tool outputs"
                  (lambda ()
                    (memory-truncate-largest-outputs mem
                      :target-ratio 0.5 :max-iterations 20)))
            ;; Phase 3: Full compaction (summarize)
            (cons "Phase 3: compacting conversation"
                  (lambda () (memory-compact mem))))))
    ;; Initial attempt
    (handler-case
        (funcall make-call-fn)
      (llm-api-error (initial-error)
        (unless (%prompt-too-long-p initial-error)
          (error initial-error))
        ;; Apply recovery phases sequentially until one succeeds
        (dolist (phase recovery-phases
                        ;; All phases exhausted — signal the last error
                        (error initial-error))
          (log-warn "agent" "Context overflow — ~a" (car phase))
          (run-hook agent :on-compact
                    (format nil "[context-recovery] ~a" (car phase)))
          (funcall (cdr phase))
          (handler-case
              (return-from %call-with-context-recovery
                (funcall make-call-fn))
            (llm-api-error (retry-error)
              (unless (%prompt-too-long-p retry-error)
                (error retry-error))
              ;; Still too long — continue to next phase
              (setf initial-error retry-error))))))))

;;; ============================================================
;;; Single step: send context → get response → handle tool calls
;;; ============================================================

(defgeneric agent-step (agent user-input)
  (:documentation
   "Execute a single agent step: process user input, get LLM response,
    execute any tool calls, and return the final text response."))

(defmethod agent-step ((agent agent) user-input)
  "One iteration of the agent loop."
  (let ((*current-agent* agent))
    (incf (agent-step-count agent))
    (log-debug "agent" "agent-step: step=~a/~a input-length=~a"
               (agent-step-count agent) (agent-max-steps agent)
               (if user-input (length user-input) 0))
    (with-request-context (:step-number (agent-step-count agent))
      (run-hook agent :before-step (agent-step-count agent))
      (when user-input
        (memory-push (agent-memory agent) (user-message user-input)))
      (let* ((inferred-cats (when user-input (infer-tool-categories user-input)))
             (tools-schema
              (if inferred-cats
                  (tools-as-schema :categories inferred-cats)
                  (tools-as-schema))))
        (log-debug "agent" "Calling LLM: tools=~a" (length tools-schema))
        (multiple-value-bind (response usage)
            (%call-with-context-recovery agent
              (lambda ()
                (let ((context
                        (memory-context-window (agent-memory agent)
                          :system-prompt (agent-system-prompt agent))))
                  (log-debug "agent" "LLM call: context-messages=~a"
                             (length context))
                  (if tools-schema
                      (complete-with-tools (agent-client agent) context tools-schema)
                      (complete (agent-client agent) context)))))
          (sibyl.llm:tracker-add-usage (agent-token-tracker agent) usage)
          ;; Estimate thinking tokens if present (API doesn't return them separately)
          (let ((thinking-text (sibyl.llm:message-thinking response)))
            (when (and thinking-text (plusp (length thinking-text)))
              (let ((thinking-tokens (ceiling (length thinking-text) 4)))
                (sibyl.llm:tracker-add-usage (agent-token-tracker agent)
                                             (list :thinking-tokens thinking-tokens)))))
          (memory-push (agent-memory agent) response)
          (cond
           ((message-tool-calls response)
            (let* ((tc-list (message-tool-calls response))
                   (use-parallel (>= (length tc-list)
                                     sibyl.tools:*parallel-tool-threshold*)))
               (log-info "agent" "LLM response: tool-calls=~a" (length tc-list))
               (if use-parallel
                   ;; --- 並列実行パス ---
                   (let ((results
                          (sibyl.tools:execute-tool-calls-parallel
                           tc-list
                           (lambda (tc)
                             (run-hook agent :on-tool-call tc)
                             (%execute-tool-with-timing agent tc)))))
                     (loop for tc in tc-list
                           for result in results
                           do (memory-push (agent-memory agent)
                                           (tool-result-message (tool-call-id tc) result))))
                   ;; --- シリアル実行パス (threshold 未満) ---
                   (dolist (tc tc-list)
                     (run-hook agent :on-tool-call tc)
                     (let ((result (%execute-tool-with-timing agent tc)))
                       (memory-push (agent-memory agent)
                                    (tool-result-message (tool-call-id tc) result))))))
            (if (< (agent-step-count agent) (agent-max-steps agent))
                (progn
                  (log-debug "agent" "Recursive step: step=~a" (agent-step-count agent))
                  (agent-step agent nil))
                (progn
                  (log-warn "agent" "Max steps reached: ~a/~a"
                            (agent-step-count agent) (agent-max-steps agent))
                  (format nil "[Sibyl: max steps (~a) reached]"
                          (agent-max-steps agent)))))
           (t
            (log-info "agent" "LLM response: text-length=~a"
                      (length (or (message-content response) "")))
            (run-hook agent :after-step response)
            (message-content response))))))))

;;; ============================================================
;;; Multi-turn run: process input until final text response
;;; ============================================================

(defgeneric agent-run (agent input)
  (:documentation "Run the agent with INPUT and return the final response."))

(defmethod agent-run ((agent agent) input)
  "High-level entry point. Resets step counter and runs."
  (setf (agent-step-count agent) 0)
  (with-request-context (:agent-name (agent-name agent)
                         :step-number 0
                         :user-input input)
    (agent-step agent input)))

;;; ============================================================
;;; Reset
;;; ============================================================

(defgeneric agent-reset (agent)
  (:documentation "Reset the agent's state for a fresh conversation."))

(defmethod agent-reset ((agent agent))
  "Clear memory and reset step counter."
  (memory-reset (agent-memory agent))
  (setf (agent-step-count agent) 0)
  agent)
