;;;; Multi-agent coordination and management system

(in-package :sibyl.agent)

;; Agent roles and specializations
(defclass agent-role ()
  ((name :initarg :name :accessor role-name :type string)
   (description :initarg :description :accessor role-description :type string)
   (system-prompt :initarg :system-prompt :accessor role-system-prompt :type string)
   (tools :initarg :tools :accessor role-tools :initform nil
          :documentation "List of tool names this role can access")
   (capabilities :initarg :capabilities :accessor role-capabilities :initform nil
                 :documentation "List of capability keywords")))

;; Specialized agent with role
(defclass specialized-agent (agent)
  ((role :initarg :role :accessor agent-role :type agent-role)
   (agent-id :initarg :agent-id :accessor agent-id :type string)
   (status :initarg :status :accessor agent-status :initform :idle
           :type (member :idle :working :waiting :error))))

;; Multi-agent coordinator
(defclass agent-coordinator ()
  ((agents :initarg :agents :accessor coordinator-agents :initform (make-hash-table :test 'equal)
           :documentation "Hash table of agent-id -> specialized-agent")
   (task-queue :initarg :task-queue :accessor coordinator-task-queue :initform nil)
   (communication-log :initarg :communication-log :accessor coordinator-communication-log 
                      :initform nil)
   (coordination-strategy :initarg :coordination-strategy :accessor coordinator-strategy
                          :initform :sequential :type (member :sequential :parallel :hierarchical))))

;; Task representation
(defclass agent-task ()
  ((id :initarg :id :accessor task-id :type string)
   (description :initarg :description :accessor task-description :type string)
   (assigned-agent :initarg :assigned-agent :accessor task-assigned-agent :initform nil)
   (dependencies :initarg :dependencies :accessor task-dependencies :initform nil
                 :documentation "List of task IDs this task depends on")
   (status :initarg :status :accessor task-status :initform :pending
           :type (member :pending :assigned :in-progress :completed :failed))
   (result :initarg :result :accessor task-result :initform nil)
   (created-at :initarg :created-at :accessor task-created-at :initform (get-universal-time))
   (completed-at :initarg :completed-at :accessor task-completed-at :initform nil)))

;; Inter-agent communication message
(defclass inter-agent-message ()
  ((from :initarg :from :accessor msg-from :type string)
   (to :initarg :to :accessor msg-to :type string)
   (content :initarg :content :accessor msg-content :type string)
   (message-type :initarg :message-type :accessor msg-type 
                 :type (member :request :response :notification :broadcast))
   (timestamp :initarg :timestamp :accessor msg-timestamp :initform (get-universal-time))
   (context :initarg :context :accessor msg-context :initform nil
            :documentation "Additional context data")))

;; Predefined agent roles
(defparameter *default-roles*
  (list
   (make-instance 'agent-role
                  :name "coder"
                  :description "Specialized in writing and modifying code"
                  :system-prompt "You are a coding specialist. Focus on writing clean, efficient code."
                  :tools '("read-file" "write-file" "eval-form" "safe-redefine" "sync-to-file")
                  :capabilities '(:code-generation :refactoring :debugging))
   
   (make-instance 'agent-role
                  :name "tester"
                  :description "Specialized in testing and quality assurance"
                  :system-prompt "You are a testing specialist. Focus on creating comprehensive tests."
                  :tools '("write-test" "run-tests" "read-sexp")
                  :capabilities '(:test-generation :test-execution :quality-assurance))
   
   (make-instance 'agent-role
                  :name "architect"
                  :description "Specialized in system design and architecture"
                  :system-prompt "You are an architecture specialist. Focus on system design and structure."
                  :tools '("codebase-map" "suggest-improvements" "describe-symbol" "who-calls")
                  :capabilities '(:system-design :code-analysis :architecture-review))
   
   (make-instance 'agent-role
                  :name "coordinator"
                  :description "Coordinates tasks between other agents"
                  :system-prompt "You are a coordination specialist. Focus on task management and delegation."
                  :tools '("list-directory" "grep" "file-info")
                  :capabilities '(:task-management :coordination :planning))))

;; Constructor functions
(defun make-specialized-agent (role client &key (agent-id (generate-agent-id)) name)
  "Create a specialized agent with a specific role"
  (make-instance 'specialized-agent
                 :role role
                 :agent-id agent-id
                 :client client
                 :name (or name (format nil "~a-~a" (role-name role) agent-id))
                 :system-prompt (role-system-prompt role)))

(defun make-agent-coordinator (&key (strategy :sequential))
  "Create a new agent coordinator"
  (make-instance 'agent-coordinator :coordination-strategy strategy))

(defun generate-agent-id ()
  "Generate a unique agent ID"
  (format nil "agent-~a" (get-universal-time)))

(defun generate-task-id ()
  "Generate a unique task ID"
  (format nil "task-~a" (get-universal-time)))

;; Agent management functions
(defmethod add-agent ((coordinator agent-coordinator) (agent specialized-agent))
  "Add an agent to the coordinator"
  (setf (gethash (agent-id agent) (coordinator-agents coordinator)) agent)
  agent)

(defmethod remove-agent ((coordinator agent-coordinator) agent-id)
  "Remove an agent from the coordinator"
  (remhash agent-id (coordinator-agents coordinator)))

(defmethod find-agent ((coordinator agent-coordinator) agent-id)
  "Find an agent by ID"
  (gethash agent-id (coordinator-agents coordinator)))

(defmethod list-agents ((coordinator agent-coordinator))
  "List all agents in the coordinator"
  (let ((agents nil))
    (maphash (lambda (id agent) 
               (declare (ignore id))
               (push agent agents))
             (coordinator-agents coordinator))
    agents))

;; Task management functions
(defmethod create-task ((coordinator agent-coordinator) description &key dependencies)
  "Create a new task"
  (let ((task (make-instance 'agent-task
                             :id (generate-task-id)
                             :description description
                             :dependencies dependencies)))
    (push task (coordinator-task-queue coordinator))
    task))

(defmethod assign-task ((coordinator agent-coordinator) (task agent-task) agent-id)
  "Assign a task to a specific agent"
  (let ((agent (find-agent coordinator agent-id)))
    (when agent
      (setf (task-assigned-agent task) agent-id)
      (setf (task-status task) :assigned)
      (setf (agent-status agent) :working)
      task)))

(defmethod complete-task ((coordinator agent-coordinator) (task agent-task) result)
  "Mark a task as completed with result"
  (setf (task-status task) :completed)
  (setf (task-result task) result)
  (setf (task-completed-at task) (get-universal-time))
  (when (task-assigned-agent task)
    (let ((agent (find-agent coordinator (task-assigned-agent task))))
      (when agent
        (setf (agent-status agent) :idle))))
  task)

;; Communication functions
(defmethod send-message ((coordinator agent-coordinator) from-id to-id content message-type &key context)
  "Send a message between agents"
  (let ((message (make-instance 'inter-agent-message
                                :from from-id
                                :to to-id
                                :content content
                                :message-type message-type
                                :context context)))
    (push message (coordinator-communication-log coordinator))
    message))

(defmethod broadcast-message ((coordinator agent-coordinator) from-id content &key context)
  "Broadcast a message to all agents"
  (let ((messages nil))
    (maphash (lambda (agent-id agent)
               (declare (ignore agent))
               (unless (string= agent-id from-id)
                 (push (send-message coordinator from-id agent-id content :broadcast :context context)
                       messages)))
             (coordinator-agents coordinator))
    messages))

;; Coordination strategies
(defmethod execute-tasks ((coordinator agent-coordinator))
  "Execute tasks based on coordination strategy"
  (case (coordinator-strategy coordinator)
    (:sequential (execute-tasks-sequential coordinator))
    (:parallel (execute-tasks-parallel coordinator))
    (:hierarchical (execute-tasks-hierarchical coordinator))))

(defmethod execute-tasks-sequential ((coordinator agent-coordinator))
  "Execute tasks one by one"
  (dolist (task (reverse (coordinator-task-queue coordinator)))
    (when (eq (task-status task) :pending)
      ;; Find suitable agent for task
      (let ((suitable-agent (find-suitable-agent coordinator task)))
        (when suitable-agent
          (assign-task coordinator task (agent-id suitable-agent))
          ;; Execute task (simplified - in real implementation would be async)
          (let ((result (execute-agent-task suitable-agent task)))
            (complete-task coordinator task result)))))))

(defmethod find-suitable-agent ((coordinator agent-coordinator) (task agent-task))
  "Find the most suitable agent for a task based on capabilities"
  ;; Simple implementation - find first idle agent
  ;; In real implementation, would match task requirements with agent capabilities
  (loop for agent in (list-agents coordinator)
        when (eq (agent-status agent) :idle)
        return agent))

(defmethod execute-agent-task ((agent specialized-agent) (task agent-task))
  "Execute a task with a specific agent"
  ;; This would integrate with the existing agent-run method
  ;; For now, return a placeholder result
  (format nil "Task '~a' executed by ~a" (task-description task) (agent-name agent)))

;; Enhanced specialized agent with adaptive model selection
(defmethod make-specialized-agent-adaptive ((role agent-role) &key (model-selector (sibyl.llm:make-model-selector)))
  "Create a specialized agent with adaptive model selection capabilities"
  (let* ((agent-name (format nil "~a-agent-~a" (role-name role) (random 1000000000)))
         (system-prompt (format nil "~a~%~%Role: ~a~%~a~%~%Available tools: ~{~a~^, ~}"
                                (role-system-prompt role)
                                (role-name role)
                                (role-description role)
                                (role-tools role)))
         (agent (sibyl.llm:make-adaptive-agent :model-selector model-selector
                                               :name agent-name
                                               :system-prompt system-prompt)))
    
    ;; Set the role
    (setf (agent-role agent) role)
    (setf (agent-status agent) :idle)
    
    agent))