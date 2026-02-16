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
                   Hooks: :before-step :after-step :on-tool-call :on-error"))
  (:documentation "The central agent that orchestrates LLM and tools."))

;;; ============================================================
;;; Default system prompt
;;; ============================================================

(defparameter *default-system-prompt*
  "You are Sibyl, a coding agent implemented in Common Lisp.
You help users by reading, writing, and modifying code.
You have access to tools for file operations, shell commands, and search.

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
                     (max-memory-messages 100))
  "Create a new agent instance."
  (make-instance 'agent
                 :client client
                 :name name
                 :system-prompt system-prompt
                 :max-steps max-steps
                 :memory (make-memory :max-messages max-memory-messages)))

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
         (tools-schema (tools-as-schema))
         (response (if tools-schema
                       (complete-with-tools
                        (agent-client agent) context tools-schema)
                       (complete (agent-client agent) context))))

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
       (message-content response)))))

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
