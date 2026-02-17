;;;; packages.lisp — Package definitions for Sibyl

(defpackage #:sibyl.system
  (:use #:cl)
  (:export
   ;; Protection state
   #:*modified-files*
   #:*modified-files-lock*
   ;; Protection API
   #:protect-file
   #:unprotect-file
   #:file-protected-p
   #:clear-all-protections))

(defpackage #:sibyl.conditions
  (:use #:cl)
  (:export
   ;; Base conditions
   #:sibyl-error
   #:sibyl-warning
   ;; LLM conditions
   #:llm-error
   #:llm-api-error
   #:llm-rate-limit-error
   #:llm-invalid-response
   #:llm-stream-error
   #:llm-cancelled
   #:llm-error-status-code
   #:llm-error-body
   ;; Tool conditions
   #:tool-error
   #:tool-not-found-error
   #:tool-execution-error
   #:tool-validation-error
   #:tool-error-tool-name
   ;; Config conditions
   #:config-error
   #:config-missing-key-error))

(defpackage #:sibyl.config
  (:use #:cl #:sibyl.conditions)
  (:export
   #:*config*
   #:load-config
   #:config-value
   #:with-config))

(defpackage #:sibyl.util
  (:use #:cl)
  (:export
   #:string-join
   #:string-trim-whitespace
   #:alist-to-hash
   #:hash-to-alist
   #:getf*
   #:with-gensyms
   #:timestamp-now
   #:truncate-string))

(defpackage #:sibyl.llm
  (:use #:cl #:sibyl.conditions #:sibyl.config #:sibyl.util)
  (:export
   ;; Message protocol
   #:message
   #:make-message
   #:message-role
   #:message-content
   #:message-tool-calls
   #:message-tool-call-id
   #:message-timestamp
   #:system-message
   #:user-message
   #:assistant-message
   #:tool-result-message
   ;; Tool call
   #:tool-call
   #:make-tool-call
   #:tool-call-id
   #:tool-call-name
   #:tool-call-arguments
   ;; Conversation
   #:conversation
   #:make-conversation
   #:conversation-messages
   #:conversation-push
   #:conversation-clear
   #:conversation-to-list
   #:conversation-length
   ;; Client protocol
   #:llm-client
   #:complete
   #:complete-with-tools
   #:count-tokens
   #:*streaming-text-callback*
   ;; Provider construction
   #:make-anthropic-client
   #:make-openai-client))

(defpackage #:sibyl.tools
   (:use #:cl #:sibyl.conditions #:sibyl.util)
   (:export
    ;; Tool protocol
    #:tool
    #:tool-name
    #:tool-description
    #:tool-parameters
    #:tool-handler
    #:deftool
     ;; Registry
     #:*tool-registry*
     #:*tool-registry-lock*
     #:register-tool
     #:find-tool
     #:list-tools
     #:tools-as-schema
    ;; Execution
    #:execute-tool
    #:execute-tool-call
     ;; Lisp introspection tools
     #:read-sexp
     #:describe-symbol
     #:eval-form
     #:macroexpand-form
     #:package-symbols
      ;; Evolution state management
      #:*evolution-state*
      #:*evolution-state-lock*
      #:evolution-state-init
      #:evolution-state-record-attempt
      #:evolution-state-save
      #:evolution-state-load
       ;; Evolution progress reporting
       #:evolution-report-cycle-start
       #:evolution-report-improvement-start
       #:evolution-report-step
       #:evolution-report-improvement-result
       #:evolution-report-cycle-summary
       #:evolution-report-final-summary
        ;; Self-assess guard (exported for test runner binding)
        #:*self-assess-running*
        ;; Codebase-map cache (exported for test runner use)
        #:*codebase-map-cache*
        #:with-codebase-map-cache))

(defpackage #:sibyl.agent
  (:use #:cl #:sibyl.conditions #:sibyl.config #:sibyl.util
        #:sibyl.llm #:sibyl.tools)
  (:export
   ;; Memory
   #:memory
   #:make-memory
   #:memory-conversation
   #:memory-summary
   #:memory-push
   #:memory-context-window
   #:memory-compact
   ;; Agent
   #:agent
   #:make-agent
   #:agent-name
   #:agent-client
   #:agent-memory
   #:agent-system-prompt
   #:agent-hooks
   #:agent-step
   #:agent-run
   #:agent-reset))

(defpackage #:sibyl.repl.spinner
  (:use #:cl)
  (:export
   #:start-spinner
   #:stop-spinner
   #:spinner-active-p
   #:update-spinner-message))

(defpackage #:sibyl.repl
  (:use #:cl #:sibyl.agent #:sibyl.config)
  (:export
   #:start-repl
   #:repl-command-p
   #:handle-repl-command
   #:readline-available-p))

(defpackage #:sibyl
  (:use #:cl)
  (:import-from #:sibyl.config
                #:load-config #:config-value #:with-config)
  (:import-from #:sibyl.llm
                #:make-anthropic-client #:make-openai-client)
  (:import-from #:sibyl.tools
                #:deftool #:list-tools #:execute-tool)
  (:import-from #:sibyl.agent
                #:make-agent #:agent-run #:agent-step #:agent-reset)
  (:import-from #:sibyl.repl
                #:start-repl)
  (:export
   ;; Top-level API
   #:load-config
   #:config-value
   #:with-config
   #:make-anthropic-client
   #:make-openai-client
   #:deftool
   #:list-tools
   #:make-agent
   #:agent-run
   #:agent-step
   #:agent-reset
   #:start-repl))
