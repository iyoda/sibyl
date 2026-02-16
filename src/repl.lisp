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
    ("/history" . :history))
  "Mapping of REPL commands to actions.")

(defun repl-command-p (input)
  "Check if INPUT is a REPL command. Returns the command keyword or NIL."
  (cdr (assoc input *repl-commands* :test #'string-equal)))

(defun handle-repl-command (command agent)
  "Handle a REPL command. Returns :quit to exit, or NIL to continue."
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
               (when (eq (handle-repl-command cmd agent) :quit)
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
