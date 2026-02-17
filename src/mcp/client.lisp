;;;; client.lisp — MCP (Model Context Protocol) client
;;;; Implements the Streamable HTTP transport (spec 2025-03-26).
;;;; Provides JSON-RPC 2.0 communication with MCP servers for tool discovery and invocation.

(in-package #:sibyl.mcp)

;;; ============================================================
;;; MCP-specific conditions
;;; ============================================================

(define-condition mcp-error (sibyl-error)
  ((server-name :initarg :server-name
                :initform ""
                :reader mcp-error-server-name))
  (:report (lambda (c s)
             (format s "MCP error~@[ (server: ~a)~]: ~a"
                     (let ((name (mcp-error-server-name c)))
                       (when (plusp (length name)) name))
                     (slot-value c 'sibyl.conditions::message)))))

(define-condition mcp-protocol-error (mcp-error)
  ((code :initarg :code :initform 0 :reader mcp-error-code))
  (:report (lambda (c s)
             (format s "MCP protocol error ~a~@[ (server: ~a)~]: ~a"
                     (mcp-error-code c)
                     (let ((name (mcp-error-server-name c)))
                       (when (plusp (length name)) name))
                     (slot-value c 'sibyl.conditions::message)))))

(define-condition mcp-transport-error (mcp-error)
  ()
  (:report (lambda (c s)
             (format s "MCP transport error~@[ (server: ~a)~]: ~a"
                     (let ((name (mcp-error-server-name c)))
                       (when (plusp (length name)) name))
                     (slot-value c 'sibyl.conditions::message)))))

;;; ============================================================
;;; MCP client class
;;; ============================================================

(defclass mcp-client ()
  ((name       :initarg :name
               :accessor mcp-client-name
               :type string
               :documentation "Human-readable name for this MCP server connection.")
   (url        :initarg :url
               :accessor mcp-client-url
               :type string
               :documentation "MCP server endpoint URL.")
   (headers    :initarg :headers
               :accessor mcp-client-headers
               :initform nil
               :type list
               :documentation "Extra HTTP headers (alist) for auth, etc.")
   (session-id :initarg :session-id
               :accessor mcp-client-session-id
               :initform nil
               :type (or string null)
               :documentation "Mcp-Session-Id from server, set after initialize.")
   (request-id :initarg :request-id
               :accessor mcp-client-request-id
               :initform 0
               :type integer
               :documentation "Monotonically increasing JSON-RPC request ID.")
   (status     :initarg :status
               :accessor mcp-client-status
               :initform :disconnected
               :type keyword
               :documentation "Connection status: :disconnected, :connected, :error.")
   (server-info :initarg :server-info
                :accessor mcp-client-server-info
                :initform nil
                :documentation "Server info from initialize response.")
   (server-capabilities :initarg :server-capabilities
                        :accessor mcp-client-server-capabilities
                        :initform nil
                        :documentation "Server capabilities from initialize response.")
   (lock       :initform (bt:make-recursive-lock "mcp-client-lock")
               :reader mcp-client-lock
               :documentation "Protects request-id and session state."))
  (:documentation "Client for communicating with an MCP server via Streamable HTTP."))

(defun make-mcp-client (&key (name "mcp") url (headers nil))
  "Create a new MCP client for the given server URL."
  (make-instance 'mcp-client :name name :url url :headers headers))

(defmethod print-object ((client mcp-client) stream)
  (print-unreadable-object (client stream :type t)
    (format stream "~a (~a) ~a"
            (mcp-client-name client)
            (mcp-client-status client)
            (mcp-client-url client))))

;;; ============================================================
;;; JSON-RPC message construction
;;; ============================================================

(defun %next-request-id (client)
  "Get the next request ID, thread-safe."
  (bt:with-recursive-lock-held ((mcp-client-lock client))
    (incf (mcp-client-request-id client))))

(defun make-jsonrpc-request (id method &optional params)
  "Build a JSON-RPC 2.0 request hash-table."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "jsonrpc" ht) "2.0")
    (setf (gethash "id" ht) id)
    (setf (gethash "method" ht) method)
    (when params
      (setf (gethash "params" ht) params))
    ht))

(defun make-jsonrpc-notification (method &optional params)
  "Build a JSON-RPC 2.0 notification (no id field)."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "jsonrpc" ht) "2.0")
    (setf (gethash "method" ht) method)
    (when params
      (setf (gethash "params" ht) params))
    ht))

;;; ============================================================
;;; HTTP transport — Streamable HTTP (spec 2025-03-26)
;;; ============================================================

(defun %build-mcp-headers (client &key notification-p)
  "Build HTTP headers for an MCP request."
  (declare (ignore notification-p))
  (let ((headers (list (cons "Content-Type" "application/json")
                       (cons "Accept" "application/json, text/event-stream"))))
    ;; Add session ID if present
    (when (mcp-client-session-id client)
      (push (cons "Mcp-Session-Id" (mcp-client-session-id client)) headers))
    ;; Add custom headers (auth, etc.)
    (when (mcp-client-headers client)
      (setf headers (append headers (mcp-client-headers client))))
    headers))

(defun %encode-json (value)
  "Encode VALUE to a JSON string."
  (with-output-to-string (s)
    (yason:encode value s)))

(defun %parse-json (string)
  "Parse a JSON string to a hash-table."
  (yason:parse string :object-as :hash-table))

(defun %get-response-header (headers name)
  "Get a header value from dexador response headers.
   HEADERS may be a hash-table (keyword keys) or alist."
  (cond
    ((hash-table-p headers)
     ;; dexador returns headers as hash-table with keyword keys
     (or (gethash (intern (string-upcase name) :keyword) headers)
         (gethash name headers)))
    ((consp headers)
     ;; Alist format
     (loop for (key . value) in headers
           when (string-equal name
                              (if (keywordp key)
                                  (symbol-name key)
                                  (string key)))
             return value))
    (t nil)))

(defun %extract-session-id (response-headers)
  "Extract Mcp-Session-Id from dexador response headers."
  (%get-response-header response-headers "mcp-session-id"))

(defun %read-body-as-string (body)
  "Ensure body is a string, reading from stream if needed."
  (etypecase body
    (string body)
    (stream (prog1 (alexandria:read-stream-content-into-string body)
              (close body)))
    (null "")))

(defun %parse-sse-first-result (stream)
  "Parse SSE stream and return the first JSON-RPC result."
  (let ((result nil))
    (block sse-parse
      (sibyl.llm::parse-sse-stream
       stream
       (lambda (event-type data-str)
         (declare (ignore event-type))
         (setf result (%parse-json data-str))
         ;; We only need the first result for simple request/response
         (return-from sse-parse))
       (lambda () nil)
       :on-error (lambda (e) (declare (ignore e)) nil)))
    result))

(defun mcp-post (client message &key notification-p)
  "POST a JSON-RPC message to the MCP server.
   Returns (values parsed-response response-headers) for requests.
   Returns T for accepted notifications.
   Handles both JSON and SSE response content types."
  (let* ((url (mcp-client-url client))
         (headers (%build-mcp-headers client :notification-p notification-p))
         (json-body (%encode-json message)))
    (log-debug "mcp" "POST ~a: ~a" url (gethash "method" message "?"))
    (handler-case
        (multiple-value-bind (body status-code resp-headers)
            (dex:post url
                      :headers headers
                      :content json-body)
          ;; For notifications, server returns 202 Accepted (or 200)
          (when notification-p
            (return-from mcp-post (values t resp-headers)))
          ;; 202 with no useful body
          (when (= status-code 202)
            (return-from mcp-post (values t resp-headers)))
          ;; Check for Mcp-Session-Id header
          (let ((sid (%extract-session-id resp-headers)))
            (when sid
              (bt:with-recursive-lock-held ((mcp-client-lock client))
                (setf (mcp-client-session-id client) sid))))
          ;; Parse response — body is always a string from dexador (no :want-stream)
          (let* ((body-str (%read-body-as-string body))
                 (content-type (or (%get-response-header resp-headers "content-type")
                                   "application/json")))
            (cond
              ;; SSE text — parse events to find JSON-RPC response
              ((search "text/event-stream" content-type)
               (log-debug "mcp" "SSE response from ~a" (mcp-client-name client))
               (let ((result nil))
                 ;; Parse SSE from the string body
                 (with-input-from-string (stream body-str)
                   (sibyl.llm::parse-sse-stream
                    stream
                    (lambda (event-type data-str)
                      (declare (ignore event-type))
                      (unless result
                        (setf result (%parse-json data-str))))
                    (lambda () nil)))
                 (values result resp-headers)))
              ;; Plain JSON response (most common)
              (t
               (values (%parse-json body-str) resp-headers)))))
      (dex:http-request-failed (e)
        (let ((code (dex:response-status e))
              (body (handler-case
                        (%read-body-as-string (dex:response-body e))
                      (error () ""))))
          (log-error "mcp" "HTTP ~a from ~a: ~a" code (mcp-client-name client) body)
          (error 'mcp-transport-error
                 :server-name (mcp-client-name client)
                 :message (format nil "HTTP ~a: ~a" code
                                  (truncate-string body 500)))))
      (error (e)
        (log-error "mcp" "Transport error for ~a: ~a" (mcp-client-name client) e)
        (error 'mcp-transport-error
               :server-name (mcp-client-name client)
               :message (format nil "~a" e))))))

(defun %check-jsonrpc-error (response server-name)
  "Check a JSON-RPC response for errors. Signals mcp-protocol-error if present."
  (when (and response (gethash "error" response))
    (let* ((err (gethash "error" response))
           (code (or (gethash "code" err) 0))
           (msg (or (gethash "message" err) "Unknown error")))
      (error 'mcp-protocol-error
             :server-name server-name
             :code code
             :message msg))))

;;; ============================================================
;;; MCP protocol operations
;;; ============================================================

(defun mcp-initialize (client)
  "Perform the MCP initialization handshake.
   Sends initialize request, stores capabilities, sends initialized notification.
   Returns the server info."
  (log-info "mcp" "Initializing MCP connection to ~a (~a)"
            (mcp-client-name client) (mcp-client-url client))
  (let* ((params (let ((ht (make-hash-table :test 'equal)))
                   (setf (gethash "protocolVersion" ht) "2025-03-26")
                   (setf (gethash "capabilities" ht) (make-hash-table :test 'equal))
                   (let ((info (make-hash-table :test 'equal)))
                     (setf (gethash "name" info) "sibyl")
                     (setf (gethash "version" info) "0.1.0")
                     (setf (gethash "clientInfo" ht) info))
                   ht))
         (request (make-jsonrpc-request (%next-request-id client) "initialize" params))
         (response (mcp-post client request)))
    ;; Check for errors
    (%check-jsonrpc-error response (mcp-client-name client))
    ;; Extract result
    (let ((result (gethash "result" response)))
      (unless result
        (error 'mcp-protocol-error
               :server-name (mcp-client-name client)
               :code -1
               :message "No result in initialize response"))
      ;; Store server info and capabilities
      (setf (mcp-client-server-info client) (gethash "serverInfo" result))
      (setf (mcp-client-server-capabilities client) (gethash "capabilities" result))
      ;; Send initialized notification
      (let ((notification (make-jsonrpc-notification "notifications/initialized")))
        (mcp-post client notification :notification-p t))
      ;; Mark as connected
      (setf (mcp-client-status client) :connected)
      (log-info "mcp" "Connected to ~a (server: ~a)"
                (mcp-client-name client)
                (let ((info (mcp-client-server-info client)))
                  (if info (gethash "name" info "?") "?")))
      result)))

(defun mcp-list-tools (client)
  "List available tools from the MCP server.
   Returns a list of tool hash-tables, each with 'name', 'description', 'inputSchema'.
   Handles pagination (nextCursor)."
  (log-debug "mcp" "Listing tools from ~a" (mcp-client-name client))
  (let ((all-tools nil)
        (cursor nil))
    (loop
      (let* ((params (let ((ht (make-hash-table :test 'equal)))
                       (when cursor
                         (setf (gethash "cursor" ht) cursor))
                       ht))
             (request (make-jsonrpc-request (%next-request-id client)
                                           "tools/list" params))
             (response (mcp-post client request)))
        (%check-jsonrpc-error response (mcp-client-name client))
        (let* ((result (gethash "result" response))
               (tools (when result (gethash "tools" result)))
               (next-cursor (when result (gethash "nextCursor" result))))
          (when tools
            (setf all-tools (append all-tools (coerce tools 'list))))
          (if (and next-cursor (stringp next-cursor) (plusp (length next-cursor)))
              (setf cursor next-cursor)
              (return all-tools)))))))

(defun mcp-call-tool (client tool-name arguments)
  "Call a tool on the MCP server.
   TOOL-NAME — the tool name string.
   ARGUMENTS — a hash-table of arguments.
   Returns the result content as a string.
   Signals mcp-protocol-error for protocol errors.
   Returns error text (not signaling) for tool execution errors (isError=true)."
  (log-info "mcp" "Calling tool ~a on ~a" tool-name (mcp-client-name client))
  (let* ((params (let ((ht (make-hash-table :test 'equal)))
                   (setf (gethash "name" ht) tool-name)
                   (setf (gethash "arguments" ht)
                         (or arguments (make-hash-table :test 'equal)))
                   ht))
         (request (make-jsonrpc-request (%next-request-id client)
                                       "tools/call" params))
         (response (mcp-post client request)))
    ;; Check for JSON-RPC protocol error
    (%check-jsonrpc-error response (mcp-client-name client))
    (let ((result (gethash "result" response)))
      (unless result
        (error 'mcp-protocol-error
               :server-name (mcp-client-name client)
               :code -1
               :message (format nil "No result for tools/call ~a" tool-name)))
      ;; Extract content text
      (let* ((content (gethash "content" result))
             (is-error (gethash "isError" result))
             (text-parts
               (loop for block across (if (vectorp content) content
                                          (coerce (or content nil) 'vector))
                     when (and (hash-table-p block)
                               (string= "text" (gethash "type" block "")))
                       collect (gethash "text" block ""))))
        (let ((combined (format nil "~{~a~^~%~}" text-parts)))
          (when is-error
            (log-warn "mcp" "Tool ~a returned error: ~a"
                      tool-name (truncate-string combined 200)))
          combined)))))

(defun mcp-disconnect (client)
  "Disconnect from the MCP server. Sends DELETE if session active."
  (log-info "mcp" "Disconnecting from ~a" (mcp-client-name client))
  (when (and (mcp-client-session-id client)
             (eq (mcp-client-status client) :connected))
    (handler-case
        (dex:delete (mcp-client-url client)
                    :headers (list (cons "Mcp-Session-Id"
                                         (mcp-client-session-id client))))
      (error (e)
        (log-debug "mcp" "DELETE session failed (non-fatal): ~a" e))))
  (setf (mcp-client-status client) :disconnected)
  (setf (mcp-client-session-id client) nil)
  t)
