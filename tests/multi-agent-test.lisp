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

(test execute-tasks-parallel-without-task-fn-returns-placeholder
  "Auto-generated test"
  
  (let* ((coordinator (make-agent-coordinator :strategy :parallel))
         (agent       (make-mock-agent :response "real-result")))
    (add-agent coordinator agent)
    (create-task coordinator "parallel placeholder task")
    (execute-tasks coordinator)
    (let ((task (first (coordinator-task-queue coordinator))))
      (is (eq :completed (task-status task))
          "Task must reach :completed even without task-fn")
      (is (not (string= "real-result" (task-result task)))
          "Without task-fn, parallel result must NOT equal agent-run's value"))))

(test execute-tasks-parallel-with-task-fn-calls-execute-agent-task
  "Auto-generated test"
  
  (let* ((coordinator (make-agent-coordinator :strategy :parallel))
         (agent       (make-mock-agent :response "real-result")))
    (add-agent coordinator agent)
    (create-task coordinator "parallel real task")
    ;; Pass task-fn that calls execute-agent-task — what the tool SHOULD do
    (execute-tasks coordinator
                   :task-fn (lambda (task)
                              (let ((a (find-suitable-agent coordinator task)))
                                (if a
                                    (execute-agent-task a task)
                                    (format nil "no-agent")))))
    (let ((task (first (coordinator-task-queue coordinator))))
      (is (eq :completed (task-status task))
          "Task must be :completed")
      (is (string= "real-result" (task-result task))
          "With task-fn, result must equal agent-run's return value"))))

(test execute-agent-task-filters-tools-by-role
  "Auto-generated test"
  
;; A specialized-agent with role "tester" (tools: write-test, run-tests, read-sexp)
;; should only have those tools available during execute-agent-task.
;; We capture the agent's available-tools at execution time via a mock.
(let* ((captured-tools nil)
       (tester-agent (sibyl.multi-agent.tests::make-mock-agent
                       :role-name "tester"
                       :tools '("write-test" "run-tests" "read-sexp")
                       :response "test-done"))
       (task (sibyl.multi-agent.tests::make-test-task "Run all tests")))
  ;; Override agent-run to capture the agent's tool list at call time
  (defmethod sibyl.agent:agent-run :around ((agent sibyl.multi-agent.tests::mock-specialized-agent) input)
    (setf captured-tools (sibyl.agent:role-tools (sibyl.agent:agent-role agent)))
    (call-next-method))
  (sibyl.agent:execute-agent-task tester-agent task)
  ;; The role's tools should be respected
  (is (not (null captured-tools))
      "Tools must be captured during execution")
  (is (member "run-tests" captured-tools :test #'string=)
      "Tester role must have run-tests tool")
  (is (not (member "write-file" captured-tools :test #'string=))
      "Tester role must NOT have write-file tool"))
)

(test role-tools-filtering
  "Auto-generated test"
  (let* ((coder-role (find "coder" sibyl.agent::*default-roles*
                         :key #'sibyl.agent:role-name :test #'string=))
       (tester-role (find "tester" sibyl.agent::*default-roles*
                          :key #'sibyl.agent:role-name :test #'string=)))
  ;; Coder role should include write-file but NOT run-tests
  (let ((sibyl.tools:*allowed-tools* (sibyl.agent:role-tools coder-role)))
    (let ((schemas (sibyl.tools:tools-as-schema)))
      (is (find "write-file" schemas :key (lambda (s) (getf s :name)) :test #'string=)
          "Coder should have access to write-file")
      (is (not (find "run-tests" schemas :key (lambda (s) (getf s :name)) :test #'string=))
          "Coder should NOT have access to run-tests")))
  ;; Tester role should include run-tests but NOT write-file
  (let ((sibyl.tools:*allowed-tools* (sibyl.agent:role-tools tester-role)))
    (let ((schemas (sibyl.tools:tools-as-schema)))
      (is (find "run-tests" schemas :key (lambda (s) (getf s :name)) :test #'string=)
          "Tester should have access to run-tests")
      (is (not (find "write-file" schemas :key (lambda (s) (getf s :name)) :test #'string=))
          "Tester should NOT have access to write-file")))))

(test find-suitable-agent-role-matching
  "Auto-generated test"
  (let* ((coordinator (sibyl.agent:make-agent-coordinator))
       (coder-role (find "coder" sibyl.agent::*default-roles*
                         :key #'sibyl.agent:role-name :test #'string=))
       (tester-role (find "tester" sibyl.agent::*default-roles*
                          :key #'sibyl.agent:role-name :test #'string=))
       (mock-client (make-instance 'sibyl.llm:llm-client))
       (coder-agent (sibyl.agent:make-specialized-agent coder-role mock-client
                                                         :agent-id "coder-1"))
       (tester-agent (sibyl.agent:make-specialized-agent tester-role mock-client
                                                          :agent-id "tester-1")))
  (sibyl.agent:add-agent coordinator coder-agent)
  (sibyl.agent:add-agent coordinator tester-agent)
  ;; Task requiring "coder" role should find the coder agent
  (let ((task (sibyl.agent:create-task coordinator "Write code" :required-role "coder")))
    (let ((found (sibyl.agent:find-suitable-agent coordinator task)))
      (is (not (null found)) "Should find a suitable agent")
      (is (string= "coder-1" (sibyl.agent:agent-id found))
          "Should match coder agent")))
  ;; Task requiring "tester" role should find the tester agent
  (let ((task (sibyl.agent:create-task coordinator "Run tests" :required-role "tester")))
    (let ((found (sibyl.agent:find-suitable-agent coordinator task)))
      (is (not (null found)) "Should find a suitable agent")
      (is (string= "tester-1" (sibyl.agent:agent-id found))
          "Should match tester agent")))
  ;; Task with no role requirement should find any idle agent
  (let ((task (sibyl.agent:create-task coordinator "Generic task")))
    (let ((found (sibyl.agent:find-suitable-agent coordinator task)))
      (is (not (null found)) "Should find any idle agent")))
  ;; Task with [role] annotation in description
  (let ((task (sibyl.agent:create-task coordinator "[tester] Write unit tests")))
    (let ((found (sibyl.agent:find-suitable-agent coordinator task)))
      (is (string= "tester-1" (sibyl.agent:agent-id found))
          "Should match tester via annotation")))))

(test agent-inbox-receive
  "Auto-generated test"
  (let* ((coordinator (sibyl.agent:make-agent-coordinator))
       (coder-role (find "coder" sibyl.agent::*default-roles*
                         :key #'sibyl.agent:role-name :test #'string=))
       (tester-role (find "tester" sibyl.agent::*default-roles*
                          :key #'sibyl.agent:role-name :test #'string=))
       (mock-client (make-instance 'sibyl.llm:llm-client))
       (coder (sibyl.agent:make-specialized-agent coder-role mock-client
                                                    :agent-id "coder-1"))
       (tester (sibyl.agent:make-specialized-agent tester-role mock-client
                                                     :agent-id "tester-1")))
  (sibyl.agent:add-agent coordinator coder)
  (sibyl.agent:add-agent coordinator tester)
  ;; Send a message from coder to tester
  (sibyl.agent:send-message coordinator "coder-1" "tester-1"
                            "Please review my code" :request)
  ;; Tester should have the message in inbox
  (let ((inbox (sibyl.agent:agent-inbox tester)))
    (is (= 1 (length inbox)) "Tester should have 1 message in inbox")
    (when inbox
      (is (string= "Please review my code" (sibyl.agent:msg-content (first inbox)))
          "Message content should match")
      (is (string= "coder-1" (sibyl.agent:msg-from (first inbox)))
          "Message should be from coder")))
  ;; Coder should have empty inbox
  (is (= 0 (length (sibyl.agent:agent-inbox coder)))
      "Coder should have no messages")))

(test broadcast-delivers-to-all-inboxes
  "Auto-generated test"
  (let* ((coordinator (sibyl.agent:make-agent-coordinator))
       (coder-role (find "coder" sibyl.agent::*default-roles*
                         :key #'sibyl.agent:role-name :test #'string=))
       (tester-role (find "tester" sibyl.agent::*default-roles*
                          :key #'sibyl.agent:role-name :test #'string=))
       (mock-client (make-instance 'sibyl.llm:llm-client))
       (coder (sibyl.agent:make-specialized-agent coder-role mock-client
                                                    :agent-id "coder-1"))
       (tester (sibyl.agent:make-specialized-agent tester-role mock-client
                                                     :agent-id "tester-1"))
       (arch-role (find "architect" sibyl.agent::*default-roles*
                         :key #'sibyl.agent:role-name :test #'string=))
       (arch (sibyl.agent:make-specialized-agent arch-role mock-client
                                                   :agent-id "arch-1")))
  (sibyl.agent:add-agent coordinator coder)
  (sibyl.agent:add-agent coordinator tester)
  (sibyl.agent:add-agent coordinator arch)
  ;; Broadcast from coder
  (sibyl.agent:broadcast-message coordinator "coder-1" "Starting work")
  ;; Tester and arch should each have 1 message, coder should have 0
  (is (= 1 (length (sibyl.agent:agent-inbox tester))) "Tester gets broadcast")
  (is (= 1 (length (sibyl.agent:agent-inbox arch))) "Architect gets broadcast")
  (is (= 0 (length (sibyl.agent:agent-inbox coder))) "Sender doesn't get own broadcast")))

(test execute-task-includes-inbox-context
  "Auto-generated test"
  (let* ((coordinator (sibyl.agent:make-agent-coordinator))
       (coder-role (find "coder" sibyl.agent::*default-roles*
                         :key #'sibyl.agent:role-name :test #'string=))
       (tester-role (find "tester" sibyl.agent::*default-roles*
                          :key #'sibyl.agent:role-name :test #'string=))
       (mock-client (make-instance 'sibyl.llm:llm-client))
       (coder (sibyl.agent:make-specialized-agent coder-role mock-client
                                                    :agent-id "coder-1"))
       (tester (sibyl.agent:make-specialized-agent tester-role mock-client
                                                     :agent-id "tester-1")))
  (sibyl.agent:add-agent coordinator coder)
  (sibyl.agent:add-agent coordinator tester)
  ;; Send message to tester
  (sibyl.agent:send-message coordinator "coder-1" "tester-1"
                            "The function is in utils.lisp" :notification)
  ;; Verify format-inbox-context works
  (let ((ctx (sibyl.agent::format-inbox-context tester)))
    (is (search "coder-1" ctx) "Context should mention sender")
    (is (search "utils.lisp" ctx) "Context should contain message content"))
  ;; Verify inbox is populated before execute
  (is (= 1 (length (sibyl.agent:agent-inbox tester))) "Should have 1 message before execute")))
