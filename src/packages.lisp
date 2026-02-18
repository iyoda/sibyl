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
   #:*default-config-path*
   #:load-config
   #:config-value
   #:config-set
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

(defpackage #:sibyl.logging
  (:use #:cl #:sibyl.util)
  (:export
   ;; Configuration
   #:*log-levels*
   #:*log-level*
   #:*log-stream*
   #:*log-format*
   #:*enable-colors*
   ;; Core functions
   #:log-level-priority
   #:should-log-p
   #:log-message
   ;; Convenience functions
   #:log-trace
   #:log-debug
   #:log-info
   #:log-warn
   #:log-error
   #:log-fatal
   ;; Utilities
   #:with-logging-context
   #:log-execution-time))

(defpackage #:sibyl.llm
  (:use #:cl #:sibyl.conditions #:sibyl.config #:sibyl.util #:sibyl.logging)
  (:export
   ;; Message protocol
   #:message
   #:make-message
   #:message-role
   #:message-content
   #:message-tool-calls
   #:message-tool-call-id
   #:message-timestamp
   #:message-thinking
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
   #:client-api-key
   #:client-model
   #:client-max-tokens
   #:client-temperature
   #:client-base-url
   #:complete
   #:complete-with-tools
   #:count-tokens
   #:*streaming-text-callback*
   ;; Token tracking
   #:token-tracker
   #:make-token-tracker
   #:token-tracker-input-tokens
   #:token-tracker-output-tokens
   #:token-tracker-cache-read-tokens
   #:token-tracker-cache-write-tokens
   #:token-tracker-request-count
   #:tracker-add-usage
   #:tracker-cache-hit-rate
   ;; Provider construction
   #:make-anthropic-client
   #:model-tier
   #:model-config
   #:task-analyzer
   #:complexity-analysis
   #:model-selector
   #:adaptive-agent
   #:make-task-analyzer
   #:make-model-selector
   #:make-default-model-selector
   #:make-adaptive-agent
   #:analyze-task-complexity
   #:select-model-for-task
   #:create-client-for-model
   #:adapt-model-for-task
   #:agent-run-adaptive
   #:complexity-reasoning
   #:complexity-score
   #:complexity-factors
   #:recommended-tier
   #:tier-name
   #:tier-description
   #:tier-models
   #:model-name
   #:model-provider
   #:enhanced-model-config
   #:latest-model-selector
   #:make-latest-model-selector
   #:select-latest-model-for-task
   #:model-release-date
   #:model-version
   #:model-context-window
   #:model-capabilities
   #:call-llm
   #:lookup-model-pricing
   #:estimate-cost-usd
   #:estimate-baseline-cost-usd
   #:compute-savings-pct
   #:*model-pricing-table*
   #:task-cost-record
   #:make-task-cost-record
   #:task-cost-record-task-description
   #:task-cost-record-model-name
   #:task-cost-record-tier-name
   #:task-cost-record-timestamp
   #:task-cost-record-complexity-score
   #:task-cost-record-input-tokens
   #:task-cost-record-output-tokens
   #:task-cost-record-cache-write-tokens
   #:task-cost-record-cache-read-tokens
   #:task-cost-record-actual-cost-usd
   #:task-cost-record-baseline-cost-usd
   #:task-cost-record-savings-usd
   #:task-cost-record-savings-pct
   #:snapshot-tracker
   #:tracker-delta
   #:make-task-cost-record-from-delta
   #:session-cost-report
   #:make-session-cost-report
   #:session-cost-report-records
   #:session-cost-report-total-actual-cost-usd
   #:session-cost-report-total-baseline-cost-usd
   #:session-cost-report-total-savings-usd
   #:session-cost-report-total-savings-pct
   #:session-cost-report-total-input-tokens
   #:session-cost-report-total-output-tokens
   #:session-cost-report-total-cache-write-tokens
   #:session-cost-report-total-cache-read-tokens
   #:session-cost-report-tier-distribution
   #:session-cost-report-cache-hit-rate
   #:session-cost-report-task-count
   #:compute-session-report
   #:format-session-report
   ;; A/B tier provisioning stats
   #:provisioning-stats
   #:make-provisioning-stats
   #:provisioning-stats-total
   #:provisioning-stats-correct
   #:provisioning-stats-over
   #:provisioning-stats-under
   #:provisioning-stats-unknown
   #:provisioning-stats-accuracy
   #:provisioning-stats-over-rate
   #:provisioning-stats-under-rate
   #:ab-test-report
   #:make-ab-test-report
   #:ab-test-report-name-a
   #:ab-test-report-name-b
   #:ab-test-report-stats-a
   #:ab-test-report-stats-b
   #:ab-test-report-delta-accuracy
   #:ab-test-report-delta-over-rate
   #:ab-test-report-delta-under-rate
   #:ab-test-report-results-a
   #:ab-test-report-results-b
   #:tier-rank
   #:classify-provisioning
   #:compute-provisioning-stats
   #:evaluate-provisioning
   #:make-adaptive-tier-predictor
   #:make-fixed-tier-predictor
   #:run-tier-ab-test
   #:run-adaptive-vs-baseline-ab-test
   #:format-provisioning-stats
   #:format-ab-test-report
   #:agent-cost-records
    #:make-openai-client
    #:ollama-client
   #:make-ollama-client
   #:*ollama-model-profiles*
   #:lookup-model-profile
   #:extract-thinking-blocks
    #:ollama-model-info
   #:detect-model-capabilities
    #:ollama-pre-warm
    #:ollama-ensure-warm
    #:ollama-server-reachable-p
    #:ollama-running-models
    #:ollama-model-loaded-p
    #:ollama-model-exists-p
    #:ollama-health-check
    ;; HTTP timeout configuration
    #:*default-connect-timeout*
    #:*default-read-timeout*
    ;; Retry
    #:call-with-retry))

(defpackage #:sibyl.plan
  (:use #:cl #:sibyl.util)
  (:export
   ;; Data constructors
   #:make-plan
   #:make-phase
   #:make-task
   ;; Status transitions
   #:plan-update-status
   #:plan-update-task-status
   ;; Serialization
   #:plan->sexp
   #:sexp->plan
   ;; File I/O
   #:save-plan
   #:load-plan
   #:delete-plan
   #:list-plans
   ;; Plan mutation
   #:plan-add-phase
   #:plan-add-task
   ;; Index
   #:load-plan-index
   ;; Formatting
   #:format-plan
   ;; Configuration
   #:*default-plan-directory*))

(defpackage #:sibyl.tools
   (:use #:cl #:sibyl.conditions #:sibyl.util #:sibyl.logging)
   (:export
    ;; Tool protocol
     #:tool
     #:tool-name
     #:tool-description
     #:tool-parameters
     #:tool-handler
     #:tool-category
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
     #:execute-tool-calls-parallel
     #:*parallel-tool-threshold*
        #:with-codebase-map-cache))

(defpackage #:sibyl.agent
  (:use #:cl #:sibyl.conditions #:sibyl.config #:sibyl.util
        #:sibyl.llm #:sibyl.tools #:sibyl.logging)
  (:export
   ;; Memory
   #:memory
   #:make-memory
   #:memory-conversation
   #:memory-summary
   #:memory-max-messages
   #:memory-push
   #:memory-context-window
   #:memory-compact
   #:memory-compaction-strategy
    #:memory-compaction-client
    #:memory-sanitize
    ;; Agent
   #:agent
   #:make-agent
   #:select-ollama-system-prompt
   #:agent-name
   #:agent-client
   #:agent-memory
   #:agent-system-prompt
   #:agent-hooks
   #:agent-token-tracker
   #:agent-step
   #:agent-run
   #:agent-role
   #:specialized-agent
   #:agent-coordinator
   #:agent-task
   #:inter-agent-message
   #:make-specialized-agent
   #:make-agent-coordinator
   #:*default-roles*
   #:add-agent
   #:remove-agent
   #:find-agent
   #:list-agents
   #:create-task
   #:assign-task
   #:complete-task
   #:send-message
   #:broadcast-message
   #:execute-tasks
   #:role-name
   #:role-description
   #:role-system-prompt
   #:role-tools
   #:role-capabilities
   #:agent-id
   #:agent-status
   #:task-id
   #:task-description
   #:task-status
   #:task-result
   #:coordinator-task-queue
   #:coordinator-agents
   #:coordinator-strategy
   #:coordinator-communication-log
   #:task-assigned-agent
   #:task-dependencies
   #:task-created-at
   #:task-completed-at
   #:execute-tasks-parallel
   #:execute-tasks-sequential
   #:execute-tasks-hierarchical
   #:find-suitable-agent
   #:execute-agent-task
   #:send-message
   #:msg-from
   #:msg-to
   #:msg-content
   #:msg-type
   #:generate-task-id
   #:generate-agent-id
   #:*current-agent*
   #:infer-tool-categories
   #:agent-reset))

(defpackage #:sibyl.mcp
  (:use #:cl #:sibyl.conditions #:sibyl.config #:sibyl.util #:sibyl.logging)
  (:export
   ;; MCP client protocol
   #:mcp-client
   #:make-mcp-client
   #:mcp-client-name
   #:mcp-client-url
   #:mcp-client-session-id
   #:mcp-client-status
   #:mcp-client-server-info
   #:mcp-client-server-capabilities
   ;; Lifecycle
   #:mcp-initialize
   #:mcp-disconnect
   ;; Tool operations
   #:mcp-list-tools
   #:mcp-call-tool
   ;; Conditions
   #:mcp-error
   #:mcp-protocol-error
   #:mcp-transport-error
   #:mcp-error-code
   #:mcp-error-server-name
   ;; Tool adapter — bridge MCP tools into Sibyl registry
   #:register-mcp-server-tools
   #:unregister-mcp-server-tools
   #:connect-and-register-mcp-server
   #:disconnect-mcp-server
   ;; Server registry
   #:*mcp-servers*
   #:list-mcp-servers
   #:find-mcp-server
   ;; Config-based auto-connect
   #:connect-configured-mcp-servers
   #:disconnect-all-mcp-servers))

(defpackage #:sibyl.repl.spinner
  (:use #:cl)
  (:export
   #:start-spinner
   #:stop-spinner
   #:spinner-active-p
   #:update-spinner-message))

(defpackage #:sibyl.repl
  (:use #:cl #:sibyl.agent #:sibyl.config #:sibyl.logging)
  (:export
   #:start-repl
   #:repl-command-p
   #:handle-repl-command
   #:readline-available-p))

(defpackage #:sibyl.cache
  (:use #:cl #:sibyl.util #:sibyl.logging)
  (:import-from #:sibyl.llm
                #:llm-client
                #:complete
                #:complete-with-tools
                #:*streaming-text-callback*
                #:client-model
                #:client-max-tokens
                #:client-temperature
                #:message
                #:message-role
                #:message-content
                #:message-tool-calls
                #:message-tool-call-id
                #:message-thinking
                #:tool-call
                #:tool-call-id
                #:tool-call-name
                #:tool-call-arguments
                #:assistant-message)
  (:export
   ;; Config
   #:*cache-enabled*
   #:*cache-max-entries*
   #:*cache-ttl-seconds*
   ;; Key generation
   #:make-cache-key
   ;; Store protocol
   #:cache-get
   #:cache-put
   #:cache-evict-expired
   #:cache-flush
   #:cache-stats
   ;; LRU store
   #:lru-cache
   #:make-lru-cache
   ;; Telemetry
   #:record-cache-hit
   #:record-cache-miss
   #:record-server-cache-tokens
   #:get-cache-telemetry
   #:reset-cache-telemetry
   ;; Anthropic adapter
   #:anthropic-normalize-request
   #:anthropic-wrap-response
   #:anthropic-no-cache-p
   #:anthropic-extract-server-cache-tokens
   ;; OpenAI adapter
   #:openai-normalize-request
   #:openai-wrap-response
   #:openai-no-cache-p
   #:openai-extract-server-cache-tokens
   ;; Integration
   #:*response-cache*
   #:ensure-cache
   #:flush-response-cache
   #:response-cache-stats))

(defpackage #:sibyl
  (:use #:cl)
  (:import-from #:sibyl.config
                #:load-config #:config-value #:config-set #:with-config)
   (:import-from #:sibyl.llm
                  #:make-anthropic-client #:make-openai-client
                  #:make-ollama-client #:ollama-client
                  #:ollama-health-check #:ollama-ensure-warm)
   (:import-from #:sibyl.tools
                #:deftool #:list-tools #:execute-tool)
  (:import-from #:sibyl.agent
                #:make-agent #:agent-run #:agent-step #:agent-reset)
  (:import-from #:sibyl.repl
                #:start-repl)
  (:import-from #:sibyl.mcp
                #:connect-configured-mcp-servers
                #:disconnect-all-mcp-servers
                #:list-mcp-servers)
  (:export
   ;; Top-level API
   #:load-config
   #:config-value
   #:config-set
   #:with-config
    #:make-anthropic-client
    #:make-openai-client
    #:make-ollama-client
    #:ollama-client
    #:ollama-health-check
    #:ollama-ensure-warm
     #:deftool
   #:list-tools
   #:make-agent
   #:agent-run
   #:agent-step
   #:agent-reset
   #:start-repl
   ;; MCP
   #:connect-configured-mcp-servers
   #:disconnect-all-mcp-servers
   #:list-mcp-servers))
