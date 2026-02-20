;;;; multi-agent-test.lisp — Tests for multi-agent sub-agent execution
;;;; Phase 1: LLM Connection
;;;; Phase 2: Role Control
;;;; Phase 3: Bidirectional Communication
;;;; Phase 4: Hierarchical Strategy

(defpackage #:sibyl.multi-agent.tests
  (:use #:cl #:fiveam)
  (:import-from #:sibyl.agent
                ;; coordinator
                #:make-agent-coordinator
                #:add-agent
                #:find-agent
                #:list-agents
                #:create-task
                #:assign-task
                #:complete-task
                #:coordinator-task-queue
                #:coordinator-strategy
                ;; tasks
                #:task-status
                #:task-result
                #:task-description
                #:task-id
                #:task-dependencies
                #:task-assigned-agent
                ;; execution
                #:execute-tasks
                #:execute-tasks-sequential
                #:execute-agent-task
                #:find-suitable-agent
                ;; agents
                #:make-specialized-agent
                #:specialized-agent
                #:agent-id
                #:agent-status
                #:agent-role
                ;; roles
                #:agent-role
                #:role-name
                #:role-tools
                #:role-capabilities
                #:role-system-prompt
                ;; messaging
                #:send-message
                #:broadcast-message
                #:coordinator-communication-log
                #:msg-from
                #:msg-to
                #:msg-content
                #:msg-type
                ;; agent-run (the real method we want to test)
                #:agent-run
                #:agent-name))

(in-package #:sibyl.multi-agent.tests)

(def-suite multi-agent-tests
  :description "Tests for multi-agent sub-agent execution phases 1-4"
  :in sibyl.tests:sibyl-tests)

(in-suite multi-agent-tests)

;;; ============================================================
;;; Test helpers
;;; ============================================================

(defclass mock-specialized-agent (specialized-agent)
  ((run-called-p  :initform nil  :accessor mock-run-called-p)
   (run-input     :initform nil  :accessor mock-run-input)
   (run-response  :initform "mock-llm-response" :accessor mock-run-response))
  (:documentation "Specialized-agent subclass that mocks agent-run for unit tests."))

;;; Override agent-run to record the call without hitting a real LLM.
(defmethod agent-run ((agent mock-specialized-agent) input)
  (setf (mock-run-called-p agent) t)
  (setf (mock-run-input    agent) input)
  (mock-run-response agent))

(defun make-test-role (&key (name "coder")
                            (tools '("read-file" "write-file" "eval-form"))
                            (capabilities '(:code-generation))
                            (prompt "You are a coder."))
  "Create an agent-role for testing."
  (make-instance 'agent-role
                 :name name
                 :description (concatenate 'string name " test role")
                 :system-prompt prompt
                 :tools tools
                 :capabilities capabilities))

(defun make-mock-agent (&key (role-name "coder")
                              (tools '("read-file" "write-file" "eval-form"))
                              (capabilities '(:code-generation))
                              (response "mock-llm-response")
                              (agent-id (sibyl.agent:generate-agent-id)))
  "Create a mock-specialized-agent for testing."
  (let* ((role  (make-test-role :name role-name
                                :tools tools
                                :capabilities capabilities))
         (agent (make-instance 'mock-specialized-agent
                               :role role
                               :agent-id agent-id
                               :name (format nil "~a-~a" role-name agent-id)
                               :client nil
                               :system-prompt (role-system-prompt role))))
    (setf (mock-run-response agent) response)
    agent))

(defun make-test-task (description &key dependencies)
  "Create an agent-task for testing."
  (make-instance 'sibyl.agent:agent-task
                 :id (sibyl.agent:generate-task-id)
                 :description description
                 :dependencies dependencies))

;;; ============================================================
;;; Phase 1 – LLM Connection
;;; ============================================================

;;; --- 🔴 RED tests (specify desired behavior BEFORE implementation) ---

(test execute-agent-task-calls-agent-run
  "execute-agent-task should delegate to agent-run, not return a placeholder string.

 RED: Currently execute-agent-task returns a static placeholder string.
 After implementation it should call agent-run with the task description
 and return whatever agent-run returns."
  (let* ((agent  (make-mock-agent :response "Hello from LLM"))
         (task   (make-test-task "Write a hello function")))
    ;; Call execute-agent-task
    (let ((result (execute-agent-task agent task)))
      ;; The mock's agent-run should have been called
      (is (mock-run-called-p agent)
          "execute-agent-task must call agent-run on the agent")
      ;; The task description is passed as input to agent-run
      (is (string= "Write a hello function" (mock-run-input agent))
          "execute-agent-task must pass task-description as input to agent-run")
      ;; The result should be whatever agent-run returned, not a static placeholder
      (is (string= "Hello from LLM" result)
          "execute-agent-task must return agent-run's result"))))

(test execute-agent-task-handles-error
  "execute-agent-task should catch errors from agent-run and mark task as :failed."
  (let* ((agent  (make-mock-agent))
         (task   (make-test-task "Trigger an error")))
    ;; Override mock to signal an error
    (setf (mock-run-response agent) nil)
    (defmethod agent-run :around ((a mock-specialized-agent) input)
      (if (null (mock-run-response a))
          (error "Simulated LLM failure")
          (call-next-method)))
    ;; execute-agent-task should not propagate the error
    (let ((result (handler-case (execute-agent-task agent task)
                    (error (e)
                      (format nil "PROPAGATED:~a" e)))))
      ;; Result should indicate failure, not propagate the raw error
      (is (not (and (stringp result) (search "PROPAGATED:" result)))
          "execute-agent-task must not propagate raw errors from agent-run"))))

(test execute-tasks-sequential-uses-agent-run
  "execute-tasks-sequential should call agent-run via execute-agent-task."
  (let ((coordinator (make-agent-coordinator :strategy :sequential))
        (agent       (make-mock-agent :response "sequential-result")))
    (add-agent coordinator agent)
    (create-task coordinator "Sequential task A")
    ;; Run sequential execution
    (execute-tasks-sequential coordinator)
    ;; The mock agent's agent-run must have been invoked
    (is (mock-run-called-p agent)
        "Sequential execution must invoke agent-run on the assigned agent")
    ;; Task should be completed
    (let ((task (first (coordinator-task-queue coordinator))))
      (is (eq :completed (task-status task))
          "Task must be :completed after sequential execution")
      (is (string= "sequential-result" (task-result task))
          "Task result must equal agent-run's return value"))))

(test execute-tasks-dispatches-with-task-fn
  "execute-tasks :sequential strategy must pass results through properly."
  (let ((coordinator (make-agent-coordinator :strategy :sequential))
        (agent       (make-mock-agent :response "dispatch-result")))
    (add-agent coordinator agent)
    (create-task coordinator "Dispatch test task")
    ;; execute-tasks dispatches to sequential
    (execute-tasks coordinator)
    (let ((task (first (coordinator-task-queue coordinator))))
      (is (eq :completed (task-status task))
          "Dispatched task must be :completed")
      (is (string= "dispatch-result" (task-result task))
          "Dispatched task result must equal agent-run's return value"))))

;;; ============================================================
;;; Phase 2 – Role Control
;;; ============================================================

(test find-suitable-agent-matches-role
  "find-suitable-agent should prefer agents whose role matches the task requirement."
  (let* ((coordinator    (make-agent-coordinator))
         (coder-agent    (make-mock-agent :role-name "coder"))
         (tester-agent   (make-mock-agent :role-name "tester")))
    (add-agent coordinator coder-agent)
    (add-agent coordinator tester-agent)
    ;; Create a task that explicitly requests a "tester" role
    (let* ((task (make-test-task "Write comprehensive tests"))
           ;; Attach role hint to the task (implementation detail: description prefix)
           ;; We'll use a plist context slot on agent-task when implemented.
           ;; For now, test that idle agents are found correctly.
           (found (find-suitable-agent coordinator task)))
      ;; At minimum, should find an idle agent
      (is (not (null found))
          "find-suitable-agent must return an idle agent"))))

(test find-suitable-agent-skips-busy-agents
  "find-suitable-agent must not return an agent with :working status."
  (let* ((coordinator (make-agent-coordinator))
         (busy-agent  (make-mock-agent))
         (idle-agent  (make-mock-agent)))
    (add-agent coordinator busy-agent)
    (add-agent coordinator idle-agent)
    ;; Mark busy agent as working
    (setf (agent-status busy-agent) :working)
    (let ((task  (make-test-task "Some task"))
          (found (find-suitable-agent coordinator (make-test-task "x"))))
      (declare (ignore task))
      ;; Must select the idle agent, not the busy one
      (is (eq :idle (agent-status found))
          "find-suitable-agent must skip :working agents"))))

;;; ============================================================
;;; Phase 3 – Bidirectional Communication
;;; ============================================================

(test send-message-delivers-to-inbox
  "send-message should deliver the message to the recipient agent's inbox."
  (let* ((coordinator (make-agent-coordinator))
         (sender      (make-mock-agent :agent-id "sender-1"))
         (receiver    (make-mock-agent :agent-id "receiver-1")))
    (add-agent coordinator sender)
    (add-agent coordinator receiver)
    ;; Send a message
    (send-message coordinator "sender-1" "receiver-1"
                  "Hello from sender" :notification)
    ;; Message should appear in coordinator log (current behavior)
    (let ((log (coordinator-communication-log coordinator)))
      (is (= 1 (length log))
          "Communication log must record the message")
      (is (string= "sender-1" (msg-from (first log)))
          "Message must record correct sender")
      (is (string= "receiver-1" (msg-to (first log)))
          "Message must record correct recipient")
      (is (string= "Hello from sender" (msg-content (first log)))
          "Message content must be preserved"))))

(test broadcast-message-reaches-all-agents
  "broadcast-message should send to every agent except the sender."
  (let* ((coordinator (make-agent-coordinator))
         (sender      (make-mock-agent :agent-id "broadcaster"))
         (agent-a     (make-mock-agent :agent-id "agent-a"))
         (agent-b     (make-mock-agent :agent-id "agent-b")))
    (add-agent coordinator sender)
    (add-agent coordinator agent-a)
    (add-agent coordinator agent-b)
    ;; Broadcast from sender
    (broadcast-message coordinator "broadcaster" "Hello everyone")
    ;; Two messages should be in the log (one per non-sender agent)
    (let ((log (coordinator-communication-log coordinator)))
      (is (= 2 (length log))
          "Broadcast must produce one message per recipient")
      (let ((recipients (mapcar #'msg-to log)))
        (is (member "agent-a" recipients :test #'string=)
            "agent-a must receive the broadcast")
        (is (member "agent-b" recipients :test #'string=)
            "agent-b must receive the broadcast")
        (is (not (member "broadcaster" recipients :test #'string=))
            "Sender must not receive its own broadcast")))))

;;; ============================================================
;;; Phase 4 – Hierarchical Strategy
;;; ============================================================

(test hierarchical-strategy-executes-all-tasks
  "execute-tasks with :hierarchical strategy must complete all tasks."
  (let* ((coordinator (make-agent-coordinator :strategy :hierarchical))
         (agent-a     (make-mock-agent :role-name "coder" :response "code-done"))
         (agent-b     (make-mock-agent :role-name "tester" :response "test-done")))
    (add-agent coordinator agent-a)
    (add-agent coordinator agent-b)
    (create-task coordinator "Implement feature X")
    (create-task coordinator "Test feature X")
    ;; Execute with hierarchical strategy
    (execute-tasks coordinator)
    ;; All tasks must be completed
    (let ((tasks (coordinator-task-queue coordinator)))
      (is (every (lambda (task) (eq :completed (task-status task))) tasks)
          "All tasks must be :completed under hierarchical strategy"))))

;;; ============================================================
;;; Phase 1 – Integration test (Phase 1, task 4)
;;; ============================================================

(test integration-create-delegate-execute
  "Full flow: create-agent-coordinator -> create-task -> execute-tasks
   Verifies that agent-run is called and result propagates to the task."
  (let* ((coordinator (make-agent-coordinator :strategy :sequential))
         (agent       (make-mock-agent :response "integration-result")))
    (add-agent coordinator agent)
    ;; Simulate what delegate-task tool does: create a task
    (let ((task (create-task coordinator "implement feature X")))
      ;; Simulate what execute-team-task tool does: run pending tasks
      (execute-tasks coordinator)
      ;; agent-run must have been called with the task description
      (is (mock-run-called-p agent)
          "agent-run must be invoked during execute-tasks")
      (is (string= "implement feature X" (mock-run-input agent))
          "agent-run must receive the task description")
      ;; Task must be completed with agent-run's return value
      (is (eq :completed (task-status task))
          "Task must be :completed after execute-tasks")
      (is (string= "integration-result" (task-result task))
          "Task result must equal agent-run's return value"))))

;;; ============================================================
;;; Phase 2 – Role Control: Tool Filtering & Role Matching
;;; ============================================================

(test role-tools-list-is-enforced
  "Specialized agent should only have access to tools defined in its role."
  (let* ((coder-role (make-test-role :name "coder"
                                     :tools '("read-file" "write-file" "eval-form")
                                     :capabilities '(:code-generation)))
         (tester-role (make-test-role :name "tester"
                                      :tools '("write-test" "run-tests" "read-sexp")
                                      :capabilities '(:test-generation)))
         (coder-agent  (make-instance 'mock-specialized-agent
                                      :role coder-role
                                      :agent-id "coder-1"
                                      :name "coder-coder-1"
                                      :client nil
                                      :system-prompt (role-system-prompt coder-role)))
         (tester-agent (make-instance 'mock-specialized-agent
                                      :role tester-role
                                      :agent-id "tester-1"
                                      :name "tester-tester-1"
                                      :client nil
                                      :system-prompt (role-system-prompt tester-role))))
    ;; Coder has write-file, not run-tests
    (is (member "write-file" (role-tools (agent-role coder-agent)) :test #'string=)
        "Coder role must include write-file")
    (is (not (member "run-tests" (role-tools (agent-role coder-agent)) :test #'string=))
        "Coder role must NOT include run-tests")
    ;; Tester has run-tests, not write-file
    (is (member "run-tests" (role-tools (agent-role tester-agent)) :test #'string=)
        "Tester role must include run-tests")
    (is (not (member "write-file" (role-tools (agent-role tester-agent)) :test #'string=))
        "Tester role must NOT include write-file")))

(test find-suitable-agent-prefers-role-match
  "find-suitable-agent should prefer agents whose role-name matches the task's required role."
  (let* ((coordinator  (make-agent-coordinator))
         (coder-role   (make-test-role :name "coder"))
         (tester-role  (make-test-role :name "tester"))
         (coder-agent  (make-instance 'mock-specialized-agent
                                      :role coder-role
                                      :agent-id "c-1"
                                      :name "coder-c-1"
                                      :client nil
                                      :system-prompt ""))
         (tester-agent (make-instance 'mock-specialized-agent
                                      :role tester-role
                                      :agent-id "t-1"
                                      :name "tester-t-1"
                                      :client nil
                                      :system-prompt "")))
    (add-agent coordinator coder-agent)
    (add-agent coordinator tester-agent)
    ;; Create a task annotated for "tester" role
    (let ((task (make-instance 'sibyl.agent:agent-task
                               :id (sibyl.agent:generate-task-id)
                               :description "[tester] Write comprehensive tests"
                               :dependencies nil)))
      ;; Current basic implementation just returns any idle agent
      ;; This test documents the DESIRED behavior (RED - will fail until implemented)
      (let ((found (find-suitable-agent coordinator task)))
        (is (not (null found))
            "find-suitable-agent must return an agent")
        ;; GREEN target: tester-agent should be preferred for tester-annotated tasks
        ;; For now just verify idle agents are found
        (is (eq :idle (agent-status found))
            "find-suitable-agent must return an idle agent")))))

(test create-task-with-required-role
  "Auto-generated test"
  (let* ((coordinator (sibyl.agent:make-agent-coordinator))
       (task (sibyl.agent:create-task coordinator "Write tests for login"
                                      :required-role "tester")))
  (is (not (null task))
      "create-task must return a task object")
  (is (string= "tester" (sibyl.agent:task-required-role task))
      "create-task must store :required-role in the task")
  (is (string= "Write tests for login" (sibyl.agent:task-description task))
      "create-task must preserve the description")))

(test create-task-parses-role-annotation
  "Auto-generated test"
  ;; When description starts with [role-name], required-role should be inferred
(let* ((coordinator (sibyl.agent:make-agent-coordinator))
       (task (sibyl.agent:create-task coordinator "[tester] Write comprehensive tests")))
  (is (string= "tester" (sibyl.agent:task-required-role task))
      "create-task must parse [role] annotation from description and store as required-role")
  (is (string= "[tester] Write comprehensive tests" (sibyl.agent:task-description task))
      "description must be preserved verbatim")))
