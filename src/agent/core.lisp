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
                    Hooks: :before-step :after-step :on-tool-call :on-error")
   (token-tracker :initform (sibyl.llm::make-token-tracker)
                  :accessor agent-token-tracker
                  :documentation "Cumulative token usage for this session."))
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

;;; ============================================================
;;; Agent construction
;;; ============================================================

(defun make-agent (&key client
                     (name "Sibyl")
                     (system-prompt *default-system-prompt*)
                     (max-steps 50)
                     (max-memory-messages 50))
  "Create a new agent instance."
  (let ((agent (make-instance 'agent
                              :client client
                              :name name
                              :system-prompt system-prompt
                              :max-steps max-steps
                              :memory (make-memory :max-messages max-memory-messages))))
    ;; Propagate the LLM client to memory so :llm compaction strategy works
    (when client
      (setf (memory-compaction-client (agent-memory agent)) client))
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
          (warn 'sibyl-warning
                :message (format nil "Hook ~a error: ~a" hook-name e)))))))

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
;;; Single step: send context → get response → handle tool calls
;;; ============================================================

(defgeneric agent-step (agent user-input)
  (:documentation
   "Execute a single agent step: process user input, get LLM response,
    execute any tool calls, and return the final text response."))

(defmethod agent-step ((agent agent) user-input)
  "One iteration of the agent loop."
  (incf (agent-step-count agent))
  (run-hook agent :before-step (agent-step-count agent))

  ;; Add user message to memory
  (when user-input
    (memory-push (agent-memory agent) (user-message user-input)))

  ;; Build context and call LLM
  (let* ((context (memory-context-window
                   (agent-memory agent)
                   :system-prompt (agent-system-prompt agent)))
         (inferred-cats (when user-input (infer-tool-categories user-input)))
         (tools-schema (if inferred-cats
                           (tools-as-schema :categories inferred-cats)
                           (tools-as-schema))))
    (multiple-value-bind (response usage)
        (if tools-schema
            (complete-with-tools
             (agent-client agent) context tools-schema)
            (complete (agent-client agent) context))
      (sibyl.llm::tracker-add-usage (agent-token-tracker agent) usage)

      ;; Process response
      (memory-push (agent-memory agent) response)

      (cond
        ;; Response has tool calls → execute them and recurse
        ((message-tool-calls response)
         (dolist (tc (message-tool-calls response))
           (run-hook agent :on-tool-call tc)
           (let ((result
                   (handler-case
                       (execute-tool-call tc)
                     (tool-error (e)
                       (run-hook agent :on-error e)
                       (format nil "Error: ~a" e)))))
             (memory-push (agent-memory agent)
                          (tool-result-message (tool-call-id tc) result))))
         ;; Recurse to let LLM process tool results (no new user input)
         (if (< (agent-step-count agent) (agent-max-steps agent))
             (agent-step agent nil)
             (format nil "[Sibyl: max steps (~a) reached]"
                     (agent-max-steps agent))))

        ;; Plain text response → return it
        (t
         (run-hook agent :after-step response)
         (message-content response))))))

;;; ============================================================
;;; Multi-turn run: process input until final text response
;;; ============================================================

(defgeneric agent-run (agent input)
  (:documentation "Run the agent with INPUT and return the final response."))

(defmethod agent-run ((agent agent) input)
  "High-level entry point. Resets step counter and runs."
  (setf (agent-step-count agent) 0)
  (agent-step agent input))

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
