;;;; builtin.lisp — Built-in tools for file operations, shell, and search
;;;; These are the core tools a coding agent needs.

(in-package #:sibyl.tools)

;;; ============================================================
;;; File operations
;;; ============================================================

(deftool "read-file"
    (:description "Read the contents of a file at the given path."
     :category :file
     :parameters ((:name "path" :type "string" :required t
                   :description "Absolute or relative file path to read")))
  (let ((path (getf args :path)))
    (unless (uiop:file-exists-p path)
      (error "File not found: ~a" path))
    (uiop:read-file-string path)))

(deftool "write-file"
    (:description "Write content to a file, creating or overwriting it."
     :category :file
     :parameters ((:name "path" :type "string" :required t
                   :description "File path to write to")
                  (:name "content" :type "string" :required t
                   :description "Content to write")))
  (let* ((path (getf args :path))
         (content (getf args :content))
         (original-content (when (probe-file path)
                             (uiop:read-file-string path))))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (write-string content stream))
    (let ((diff (when original-content
                  (sibyl.tools::%generate-unified-diff path original-content content))))
      (if diff
          (format nil "Written ~a bytes to ~a~%~%```diff~%~a```" (length content) path diff)
          (format nil "Written ~a bytes to ~a" (length content) path)))))

(deftool "list-directory"
    (:description "List files and subdirectories in a directory."
     :category :file
     :parameters ((:name "path" :type "string" :required t
                   :description "Directory path to list")))
  (let* ((path (getf args :path))
         (dir (uiop:directory-files path))
         (subdirs (uiop:subdirectories path))
         (entries (append
                   (mapcar (lambda (d)
                             (format nil "~a/" (car (last (pathname-directory d)))))
                           subdirs)
                   (mapcar #'file-namestring dir))))
    (string-join (string #\Newline) (sort entries #'string<))))

;;; ============================================================
;;; Shell execution
;;; ============================================================

(deftool "shell"
    (:description "Execute a shell command and return its output."
     :category :general
     :parameters ((:name "command" :type "string" :required t
                   :description "Shell command to execute")
                  (:name "timeout" :type "integer" :required nil
                   :description "Timeout in seconds (default: 30)")))
  (let* ((command (getf args :command))
         ;; timeout arg is available but not yet enforced in this skeleton
         (result (multiple-value-list
                  (uiop:run-program command
                                    :output :string
                                    :error-output :string
                                    :ignore-error-status t))))
    (destructuring-bind (stdout stderr exit-code) result
      (format nil "Exit code: ~a~%~a~@[~%STDERR: ~a~]"
              exit-code stdout
              (when (and stderr (string/= stderr ""))
                stderr)))))

;;; ============================================================
;;; Search
;;; ============================================================

(deftool "grep"
    (:description "Search for a pattern in files using regular expressions."
     :category :analysis
     :parameters ((:name "pattern" :type "string" :required t
                   :description "Regular expression pattern to search for")
                  (:name "path" :type "string" :required t
                   :description "Directory or file path to search in")
                  (:name "include" :type "string" :required nil
                   :description "File glob pattern to include (e.g. \"*.lisp\")")
                  (:name "exclude-dir" :type "string" :required nil
                   :description "Directory pattern to exclude (e.g. \".git\")")))
  (let* ((pattern (getf args :pattern))
         (path (getf args :path))
         (include (getf args :include))
         (exclude-dir (getf args :exclude-dir))
         (cmd (format nil "grep -rn~@[ --include=~a~]~@[ --exclude-dir=~a~] ~s ~s"
                      include exclude-dir pattern path)))
    (uiop:run-program cmd
                      :output :string
                      :error-output nil
                      :ignore-error-status t)))

;;; ============================================================
;;; Code intelligence
;;; ============================================================

(deftool "file-info"
    (:description "Get metadata about a file: size, type, modification time."
     :category :file
     :parameters ((:name "path" :type "string" :required t
                   :description "Path to the file")))
  (let ((path (getf args :path)))
    (unless (uiop:file-exists-p path)
      (error "File not found: ~a" path))
    (let ((stat (uiop:safe-file-write-date path)))
      (format nil "Path: ~a~%Size: ~a bytes~%Modified: ~a"
              path
              (with-open-file (s path) (file-length s))
              (or stat "unknown")))))

;;; ============================================================
;;; Multi-agent coordination
;;; ============================================================

(defvar *current-coordinator* nil
  "Current active agent coordinator")

(deftool "create-agent-team"
    (:description "Create a team of specialized agents for collaborative work"
     :category :general
     :parameters ((:name "roles" :type "string" :required t 
                   :description "Comma-separated list of agent roles (coder,tester,architect,coordinator)")
                  (:name "strategy" :type "string" :required nil
                   :description "Coordination strategy: sequential, parallel, or hierarchical")))
  (let* ((role-names (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                             (cl-ppcre:split "," (getf args :roles))))
         (strategy-keyword (if (getf args :strategy)
                              (intern (string-upcase (getf args :strategy)) :keyword)
                              :sequential))
         (coordinator (sibyl.agent:make-agent-coordinator :strategy strategy-keyword))
         ;; Use Anthropic client with current config
         (client (sibyl.llm:make-anthropic-client 
                  :api-key (sibyl.config:config-value "llm.anthropic.api-key"))))
    
    ;; Create agents for each role
    (dolist (role-name role-names)
      (let ((role (find role-name sibyl.agent:*default-roles* 
                        :key #'sibyl.agent:role-name :test #'string-equal)))
        (when role
          (let ((agent (sibyl.agent:make-specialized-agent role client)))
            (sibyl.agent:add-agent coordinator agent)))))
    
    ;; Set as current coordinator
    (setf *current-coordinator* coordinator)
    
    (format nil "Created agent team with ~a agents using ~a strategy:~%~{- ~a~%~}"
            (length (sibyl.agent:list-agents coordinator))
            strategy-keyword
            (mapcar (lambda (agent) 
                      (format nil "~a (~a)" 
                              (sibyl.agent:agent-name agent)
                              (sibyl.agent:role-name (sibyl.agent:agent-role agent))))
                     (sibyl.agent:list-agents coordinator)))))

(deftool "delegate-task"
    (:description "Delegate a task to a specific agent in the team"
     :category :general
     :parameters ((:name "task-description" :type "string" :required t
                   :description "Description of the task to delegate")
                  (:name "agent-role" :type "string" :required nil
                   :description "Preferred agent role for this task")
                  (:name "dependencies" :type "string" :required nil
                   :description "Comma-separated list of task IDs this task depends on")))
  (let ((coordinator *current-coordinator*))
    (if coordinator
        (let* ((deps (when (getf args :dependencies)
                       (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                               (cl-ppcre:split "," (getf args :dependencies)))))
               (task (sibyl.agent:create-task coordinator (getf args :task-description)
                                              :dependencies deps)))
          (if (getf args :agent-role)
              (let ((suitable-agent 
                     (find-if (lambda (agent)
                                (string-equal (getf args :agent-role)
                                              (sibyl.agent:role-name 
                                               (sibyl.agent:agent-role agent))))
                              (sibyl.agent:list-agents coordinator))))
                (if suitable-agent
                    (progn
                      (sibyl.agent:assign-task coordinator task 
                                               (sibyl.agent:agent-id suitable-agent))
                      (format nil "Task '~a' assigned to ~a (ID: ~a)" 
                              (getf args :task-description)
                              (sibyl.agent:agent-name suitable-agent)
                              (sibyl.agent:task-id task)))
                    (format nil "No agent found with role '~a'" (getf args :agent-role))))
              (format nil "Task '~a' created (ID: ~a), awaiting assignment" 
                      (getf args :task-description) (sibyl.agent:task-id task))))
        "No active agent coordinator. Create a team first with create-agent-team.")))

(deftool "list-agent-status"
    (:description "Show status of all agents in the current team"
     :category :general
     :parameters ())
  (let ((coordinator *current-coordinator*))
    (if coordinator
        (let ((agents (sibyl.agent:list-agents coordinator)))
          (if agents
              (format nil "Agent Team Status:~%~{~a~%~}"
                      (mapcar (lambda (agent)
                                (format nil "- ~a (~a): ~a" 
                                        (sibyl.agent:agent-name agent)
                                        (sibyl.agent:role-name (sibyl.agent:agent-role agent))
                                        (sibyl.agent:agent-status agent)))
                              agents))
              "No agents in the current team."))
        "No active agent coordinator. Create a team first with create-agent-team.")))

(deftool "execute-team-task"
    (:description "Execute a collaborative task using the agent team"
     :category :general
     :parameters ((:name "task-description" :type "string" :required t
                   :description "High-level description of the collaborative task")))
  (let ((coordinator *current-coordinator*))
    (if coordinator
        (progn
          ;; Create and execute tasks based on coordination strategy
          (sibyl.agent:execute-tasks coordinator)
          (format nil "Team task execution completed: ~a" (getf args :task-description)))
        "No active agent coordinator. Create a team first with create-agent-team.")))

(deftool "send-agent-message"
    (:description "Send a message between agents in the team"
     :category :general
     :parameters ((:name "from-agent" :type "string" :required t
                   :description "ID or role of the sending agent")
                  (:name "to-agent" :type "string" :required t
                   :description "ID or role of the receiving agent")
                  (:name "message" :type "string" :required t
                   :description "Message content")
                  (:name "message-type" :type "string" :required nil
                   :description "Message type: request, response, notification, broadcast")))
  (let ((coordinator *current-coordinator*))
    (if coordinator
        (let* ((msg-type (if (getf args :message-type)
                            (intern (string-upcase (getf args :message-type)) :keyword)
                            :notification))
               (from-id (or (getf args :from-agent) "system"))
               (to-id (or (getf args :to-agent) "broadcast")))
          (sibyl.agent:send-message coordinator from-id to-id (getf args :message) msg-type)
          (format nil "Message sent from ~a to ~a: ~a" from-id to-id (getf args :message)))
        "No active agent coordinator. Create a team first with create-agent-team.")))

;;; ============================================================
;;; Adaptive model selection
;;; ============================================================

(deftool "analyze-task-complexity"
    (:description "Analyze the complexity of a task description"
     :category :analysis
     :parameters ((:name "task-description" :type "string" :required t
                   :description "Description of the task to analyze")))
  (let* ((analyzer (sibyl.llm:make-task-analyzer))
         (analysis (sibyl.llm:analyze-task-complexity analyzer (getf args :task-description))))
    (format nil "Task Complexity Analysis:~%~a" (sibyl.llm:complexity-reasoning analysis))))