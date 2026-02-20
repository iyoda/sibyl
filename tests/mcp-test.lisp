;;;; mcp-test.lisp — Tests for MCP client and tool adapter
;;;; Tests JSON-RPC message construction, schema conversion, config parsing,
;;;; and server registry management without requiring a live MCP server.

(in-package #:sibyl.tests)

;;; ============================================================
;;; MCP test suite
;;; ============================================================

(def-suite mcp-tests
  :description "Tests for MCP client protocol and tool adapter."
  :in sibyl-tests)

(in-suite mcp-tests)

;;; ============================================================
;;; JSON-RPC message construction
;;; ============================================================

(test mcp-jsonrpc-request-basic
  "make-jsonrpc-request creates a proper JSON-RPC 2.0 request."
  (let ((req (sibyl.mcp::make-jsonrpc-request 1 "initialize")))
    (is (string= "2.0" (gethash "jsonrpc" req)))
    (is (= 1 (gethash "id" req)))
    (is (string= "initialize" (gethash "method" req)))
    (is (null (gethash "params" req)))))

(test mcp-jsonrpc-request-with-params
  "make-jsonrpc-request includes params when provided."
  (let* ((params (let ((ht (make-hash-table :test 'equal)))
                   (setf (gethash "cursor" ht) "abc123")
                   ht))
         (req (sibyl.mcp::make-jsonrpc-request 42 "tools/list" params)))
    (is (= 42 (gethash "id" req)))
    (is (string= "tools/list" (gethash "method" req)))
    (let ((p (gethash "params" req)))
      (is (hash-table-p p))
      (is (string= "abc123" (gethash "cursor" p))))))

(test mcp-jsonrpc-notification
  "make-jsonrpc-notification has no id field."
  (let ((notif (sibyl.mcp::make-jsonrpc-notification "notifications/initialized")))
    (is (string= "2.0" (gethash "jsonrpc" notif)))
    (is (string= "notifications/initialized" (gethash "method" notif)))
    ;; Notifications must NOT have an id
    (multiple-value-bind (val found) (gethash "id" notif)
      (declare (ignore val))
      (is (not found)))))

;;; ============================================================
;;; MCP client creation
;;; ============================================================

(test mcp-client-creation
  "make-mcp-client creates a properly initialized client."
  (let ((client (sibyl.mcp:make-mcp-client
                 :name "test-server"
                 :url "https://example.com/mcp"
                 :headers '(("x-api-key" . "test-key")))))
    (is (string= "test-server" (sibyl.mcp:mcp-client-name client)))
    (is (string= "https://example.com/mcp" (sibyl.mcp:mcp-client-url client)))
    (is (eq :disconnected (sibyl.mcp:mcp-client-status client)))
    (is (null (sibyl.mcp:mcp-client-session-id client)))
    (is (null (sibyl.mcp:mcp-client-server-info client)))))

(test mcp-client-request-id-increments
  "Request IDs increment monotonically."
  (let ((client (sibyl.mcp:make-mcp-client :name "test" :url "http://x")))
    (is (= 1 (sibyl.mcp::%next-request-id client)))
    (is (= 2 (sibyl.mcp::%next-request-id client)))
    (is (= 3 (sibyl.mcp::%next-request-id client)))))

;;; ============================================================
;;; JSON-RPC error checking
;;; ============================================================

(test mcp-check-jsonrpc-error-clean
  "No error is signaled for a response without error field."
  (let ((response (let ((ht (make-hash-table :test 'equal)))
                    (setf (gethash "result" ht) "ok")
                    ht)))
    ;; Should not signal — finishes-p confirms no error
    (finishes (sibyl.mcp::%check-jsonrpc-error response "test"))))

(test mcp-check-jsonrpc-error-signals
  "mcp-protocol-error is signaled for error responses."
  (let ((response (let ((ht (make-hash-table :test 'equal)))
                    (let ((err (make-hash-table :test 'equal)))
                      (setf (gethash "code" err) -32601)
                      (setf (gethash "message" err) "Method not found")
                      (setf (gethash "error" ht) err))
                    ht)))
    (signals sibyl.mcp::mcp-protocol-error
      (sibyl.mcp::%check-jsonrpc-error response "test"))
    ;; Verify error details
    (handler-case
        (sibyl.mcp::%check-jsonrpc-error response "srv")
      (sibyl.mcp::mcp-protocol-error (e)
        (is (= -32601 (sibyl.mcp::mcp-error-code e)))
        (is (string= "srv" (sibyl.mcp::mcp-error-server-name e)))))))

;;; ============================================================
;;; MCP tool schema → Sibyl parameter conversion
;;; ============================================================

(test mcp-json-schema-to-parameters-basic
  "Converts a JSON Schema object with properties to Sibyl parameter specs."
  (let* ((schema (let ((ht (make-hash-table :test 'equal)))
                   (let ((props (make-hash-table :test 'equal)))
                     (let ((query-prop (make-hash-table :test 'equal)))
                       (setf (gethash "type" query-prop) "string")
                       (setf (gethash "description" query-prop) "Search query")
                       (setf (gethash "query" props) query-prop))
                     (let ((num-prop (make-hash-table :test 'equal)))
                       (setf (gethash "type" num-prop) "number")
                       (setf (gethash "description" num-prop) "Number of results")
                       (setf (gethash "numResults" props) num-prop))
                     (setf (gethash "properties" ht) props))
                   (setf (gethash "required" ht) (vector "query"))
                   (setf (gethash "type" ht) "object")
                   ht))
         (params (sibyl.mcp::%json-schema-to-parameters schema)))
    ;; Should have 2 parameters, sorted by name
    (is (= 2 (length params)))
    ;; numResults comes first alphabetically
    (let ((first-param (first params)))
      (is (string= "numResults" (getf first-param :name)))
      (is (string= "number" (getf first-param :type)))
      (is (null (getf first-param :required))))
    ;; query comes second
    (let ((second-param (second params)))
      (is (string= "query" (getf second-param :name)))
      (is (string= "string" (getf second-param :type)))
      (is (eq t (getf second-param :required))))))

(test mcp-json-schema-to-parameters-empty
  "Returns NIL for nil or empty input schema."
  (is (null (sibyl.mcp::%json-schema-to-parameters nil)))
  (is (null (sibyl.mcp::%json-schema-to-parameters (make-hash-table :test 'equal)))))

;;; ============================================================
;;; Tool name prefixing
;;; ============================================================

(test mcp-tool-name-prefixed
  "MCP tool names are prefixed with server name."
  (is (string= "exa--web_search_exa"
                 (sibyl.mcp::%mcp-tool-name "exa" "web_search_exa")))
   (is (string= "github--search"
                 (sibyl.mcp::%mcp-tool-name "github" "search"))))

;;; ============================================================
;;; MCP tool → Sibyl tool conversion
;;; ============================================================

(test mcp-tool-to-sibyl-tool-registers
  "mcp-tool-to-sibyl-tool creates and registers a Sibyl tool."
  (let ((client (sibyl.mcp:make-mcp-client :name "test" :url "http://x"))
        (mcp-tool (let ((ht (make-hash-table :test 'equal)))
                    (setf (gethash "name" ht) "my_search")
                    (setf (gethash "description" ht) "Search something")
                    (let ((schema (make-hash-table :test 'equal)))
                      (setf (gethash "type" schema) "object")
                      (let ((props (make-hash-table :test 'equal)))
                        (let ((q (make-hash-table :test 'equal)))
                          (setf (gethash "type" q) "string")
                          (setf (gethash "description" q) "Query")
                          (setf (gethash "q" props) q))
                        (setf (gethash "properties" schema) props))
                      (setf (gethash "required" schema) (vector "q"))
                      (setf (gethash "inputSchema" ht) schema))
                    ht)))
    (unwind-protect
         (progn
           (sibyl.mcp::mcp-tool-to-sibyl-tool client "tsrv" mcp-tool)
           ;; Tool should be in registry with prefixed name
            (let ((tool (sibyl.tools:find-tool "tsrv--my_search")))
              (is (not (null tool)))
              (is (string= "tsrv--my_search" (sibyl.tools:tool-name tool)))
             (is (string= "Search something" (sibyl.tools:tool-description tool)))
             ;; Check parameters
             (let ((params (sibyl.tools:tool-parameters tool)))
               (is (= 1 (length params)))
               (is (string= "q" (getf (first params) :name)))
               (is (eq t (getf (first params) :required))))))
      ;; Cleanup
       (sibyl.tools::unregister-tool "tsrv--my_search"))))

;;; ============================================================
;;; Server registry
;;; ============================================================

(test mcp-server-registry-operations
  "Server registry add/find/list/remove work correctly."
  (let ((sibyl.mcp::*mcp-servers* (make-hash-table :test 'equal)))
    ;; Initially empty
    (is (null (sibyl.mcp:list-mcp-servers)))
    ;; Add a server
    (let ((client (sibyl.mcp:make-mcp-client :name "test" :url "http://x")))
      (setf (gethash "test" sibyl.mcp::*mcp-servers*) client)
      (is (not (null (sibyl.mcp:find-mcp-server "test"))))
      (is (= 1 (length (sibyl.mcp:list-mcp-servers))))
      ;; Remove
      (remhash "test" sibyl.mcp::*mcp-servers*)
      (is (null (sibyl.mcp:find-mcp-server "test"))))))

;;; ============================================================
;;; Unregister MCP server tools
;;; ============================================================

(test mcp-unregister-server-tools
  "unregister-mcp-server-tools removes only tools from the named server."
  (let ((tool-a (sibyl.tools::make-tool
                 :name "srv1--tool-a"
                 :description "Tool A"
                 :handler (lambda (args) (declare (ignore args)) "a")))
        (tool-b (sibyl.tools::make-tool
                 :name "srv1--tool-b"
                 :description "Tool B"
                 :handler (lambda (args) (declare (ignore args)) "b")))
        (tool-c (sibyl.tools::make-tool
                 :name "srv2--tool-c"
                 :description "Tool C from different server"
                 :handler (lambda (args) (declare (ignore args)) "c"))))
    (unwind-protect
         (progn
           (sibyl.tools:register-tool tool-a)
           (sibyl.tools:register-tool tool-b)
           (sibyl.tools:register-tool tool-c)
           ;; Remove only srv1 tools
           (let ((removed (sibyl.mcp:unregister-mcp-server-tools "srv1")))
             (is (= 2 removed))
             (is (null (sibyl.tools:find-tool "srv1--tool-a")))
             (is (null (sibyl.tools:find-tool "srv1--tool-b")))
             ;; srv2 tool should remain
             (is (not (null (sibyl.tools:find-tool "srv2--tool-c"))))))
      ;; Cleanup
      (sibyl.tools::unregister-tool "srv1--tool-a")
      (sibyl.tools::unregister-tool "srv1--tool-b")
      (sibyl.tools::unregister-tool "srv2--tool-c"))))

;;; ============================================================
;;; Config parsing
;;; ============================================================

(test mcp-collect-server-configs
  "Collects MCP server configs from flat config keys."
  (let ((sibyl.config:*config* (make-hash-table :test 'equal)))
    ;; Set up config entries
    (setf (gethash "mcp.servers.exa.url" sibyl.config:*config*)
          "https://mcp.exa.ai/mcp")
    (setf (gethash "mcp.servers.exa.api-key" sibyl.config:*config*)
          "test-key-123")
    (setf (gethash "mcp.servers.github.url" sibyl.config:*config*)
          "https://mcp.github.com/mcp")
    ;; Non-MCP config entries should be ignored
    (setf (gethash "llm.model" sibyl.config:*config*) "claude-sonnet-4-5-20250929")
    (let ((configs (sibyl.mcp::%collect-mcp-server-configs)))
      ;; Should find 2 servers
      (is (= 2 (length configs)))
      ;; Exa should have API key in URL
      (let ((exa (find "exa" configs :key #'first :test #'string=)))
        (is (not (null exa)))
        (is (search "exaApiKey=test-key-123" (second exa))))
      ;; GitHub should have plain URL
      (let ((gh (find "github" configs :key #'first :test #'string=)))
        (is (not (null gh)))
        (is (string= "https://mcp.github.com/mcp" (second gh)))))))

(test mcp-collect-server-configs-with-tools-filter
  "Tool filter parameter is appended to URL."
  (let ((sibyl.config:*config* (make-hash-table :test 'equal)))
    (setf (gethash "mcp.servers.exa.url" sibyl.config:*config*)
          "https://mcp.exa.ai/mcp")
    (setf (gethash "mcp.servers.exa.tools" sibyl.config:*config*)
          "web_search_exa,crawling_exa")
    (let* ((configs (sibyl.mcp::%collect-mcp-server-configs))
           (exa (find "exa" configs :key #'first :test #'string=)))
      (is (not (null exa)))
      (is (search "tools=web_search_exa,crawling_exa" (second exa))))))

(test mcp-collect-server-configs-empty
  "Returns NIL when no MCP servers configured."
  (let ((sibyl.config:*config* (make-hash-table :test 'equal)))
    (setf (gethash "llm.model" sibyl.config:*config*) "claude")
    (is (null (sibyl.mcp::%collect-mcp-server-configs)))))

;;; ============================================================
;;; MCP condition hierarchy
;;; ============================================================

(test mcp-condition-hierarchy
  "MCP conditions follow the expected hierarchy."
  (is (subtypep 'sibyl.mcp::mcp-error 'sibyl.conditions:sibyl-error))
  (is (subtypep 'sibyl.mcp::mcp-protocol-error 'sibyl.mcp::mcp-error))
  (is (subtypep 'sibyl.mcp::mcp-transport-error 'sibyl.mcp::mcp-error)))

(test mcp-condition-reports
  "MCP conditions format properly."
  (let ((e (make-condition 'sibyl.mcp::mcp-protocol-error
                           :server-name "exa"
                           :code -32601
                           :message "Method not found")))
    (is (search "exa" (format nil "~a" e)))
    (is (search "-32601" (format nil "~a" e)))
    (is (search "Method not found" (format nil "~a" e)))))

;;; ============================================================
;;; MCP header building
;;; ============================================================

(test mcp-build-headers-basic
  "Headers include Content-Type and Accept."
  (let* ((client (sibyl.mcp:make-mcp-client :name "test" :url "http://x"))
         (headers (sibyl.mcp::%build-mcp-headers client)))
    (is (assoc "Content-Type" headers :test #'string=))
    (is (assoc "Accept" headers :test #'string=))
    ;; No session ID header for fresh client
    (is (not (assoc "Mcp-Session-Id" headers :test #'string=)))))

(test mcp-build-headers-with-session
  "Headers include Mcp-Session-Id when set."
  (let ((client (sibyl.mcp:make-mcp-client :name "test" :url "http://x")))
    (setf (sibyl.mcp:mcp-client-session-id client) "sess-abc-123")
    (let ((headers (sibyl.mcp::%build-mcp-headers client)))
      (let ((sid-entry (assoc "Mcp-Session-Id" headers :test #'string=)))
        (is (not (null sid-entry)))
        (is (string= "sess-abc-123" (cdr sid-entry)))))))

(test mcp-build-headers-with-custom
  "Custom headers (auth) are included."
  (let* ((client (sibyl.mcp:make-mcp-client
                  :name "test" :url "http://x"
                  :headers '(("x-api-key" . "my-key"))))
         (headers (sibyl.mcp::%build-mcp-headers client)))
    (let ((auth (assoc "x-api-key" headers :test #'string=)))
      (is (not (null auth)))
      (is (string= "my-key" (cdr auth))))))

;;; ============================================================
;;; /mcp REPL command registered
;;; ============================================================

(test mcp-repl-command-registered
  "The /mcp command is registered in REPL commands and handlers."
  ;; Check *repl-commands* mapping
  (let ((entry (assoc "/mcp" sibyl.repl::*repl-commands* :test #'string-equal)))
    (is (not (null entry)))
    (is (eq :mcp (cdr entry))))
  ;; Check *command-handlers* has handler
  (let ((handler (assoc :mcp sibyl.repl::*command-handlers*)))
    (is (not (null handler)))
    (is (functionp (sibyl.repl::command-entry-handler (cdr handler))))))
