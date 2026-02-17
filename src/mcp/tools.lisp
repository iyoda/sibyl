;;;; tools.lisp — MCP-to-Sibyl tool adapter
;;;; Bridges MCP tool discovery into Sibyl's native tool registry.
;;;; MCP tools are registered with a server-name prefix to avoid collisions.

(in-package #:sibyl.mcp)

;;; ============================================================
;;; MCP server registry — tracks connected servers
;;; ============================================================

(defvar *mcp-servers* (make-hash-table :test 'equal)
  "Registry of connected MCP servers. Maps server-name → mcp-client.")

(defvar *mcp-servers-lock* (bt:make-recursive-lock "mcp-servers-lock")
  "Lock protecting *mcp-servers*.")

(defun list-mcp-servers ()
  "Return a list of (name . mcp-client) for all registered MCP servers."
  (bt:with-recursive-lock-held (*mcp-servers-lock*)
    (let ((result nil))
      (maphash (lambda (k v) (push (cons k v) result)) *mcp-servers*)
      (sort result #'string< :key #'car))))

(defun find-mcp-server (name)
  "Find a connected MCP server by name."
  (bt:with-recursive-lock-held (*mcp-servers-lock*)
    (gethash name *mcp-servers*)))

;;; ============================================================
;;; MCP tool schema → Sibyl tool conversion
;;; ============================================================

(defun %mcp-tool-name (server-name tool-name)
  "Build prefixed tool name: server-name:tool-name."
  (format nil "~a:~a" server-name tool-name))

(defun %json-schema-to-parameters (input-schema)
  "Convert a JSON Schema object to a list of Sibyl parameter specs.
   INPUT-SCHEMA is a hash-table with 'properties' and 'required' keys."
  (when (and input-schema (hash-table-p input-schema))
    (let* ((properties (gethash "properties" input-schema))
           (required-list (or (gethash "required" input-schema) #()))
           (required-names (coerce required-list 'list)))
      (when (and properties (hash-table-p properties))
        (let ((params nil))
          (maphash
           (lambda (name schema)
             (let ((type-str (if (hash-table-p schema)
                                 (or (gethash "type" schema) "string")
                                 "string"))
                   (desc (if (hash-table-p schema)
                             (or (gethash "description" schema) "")
                             ""))
                   (required-p (member name required-names :test #'string=)))
               (push (list :name name
                           :type type-str
                           :required (if required-p t nil)
                           :description desc)
                     params)))
           properties)
          ;; Sort for deterministic ordering
          (sort params #'string< :key (lambda (p) (getf p :name))))))))

(defun %make-mcp-tool-handler (client mcp-tool-name)
  "Create a tool handler function that calls an MCP tool via the client.
   The handler receives ARGS as a keyword plist and converts it to a hash-table."
  (lambda (args)
    (let ((arguments (make-hash-table :test 'equal)))
      ;; Convert keyword plist to hash-table for MCP
      (loop for (key value) on args by #'cddr
            do (setf (gethash (string-downcase (symbol-name key)) arguments)
                     value))
      (mcp-call-tool client mcp-tool-name arguments))))

(defun mcp-tool-to-sibyl-tool (client server-name mcp-tool)
  "Convert an MCP tool (hash-table) to a Sibyl tool struct and register it.
   MCP-TOOL has keys: 'name', 'description', 'inputSchema'.
   Returns the created Sibyl tool."
  (let* ((raw-name (gethash "name" mcp-tool))
         (prefixed-name (%mcp-tool-name server-name raw-name))
         (description (or (gethash "description" mcp-tool) ""))
         (input-schema (gethash "inputSchema" mcp-tool))
         (parameters (%json-schema-to-parameters input-schema))
         (handler (%make-mcp-tool-handler client raw-name)))
    (let ((tool (sibyl.tools::make-tool
                 :name prefixed-name
                 :description description
                 :parameters parameters
                 :handler handler)))
      (sibyl.tools:register-tool tool)
      (log-debug "mcp" "Registered MCP tool: ~a" prefixed-name)
      tool)))

;;; ============================================================
;;; Server connection lifecycle
;;; ============================================================

(defun register-mcp-server-tools (client server-name)
  "Discover tools from a connected MCP client and register them in Sibyl's registry.
   Returns the number of tools registered."
  (let* ((mcp-tools (mcp-list-tools client))
         (count 0))
    (dolist (mcp-tool mcp-tools)
      (handler-case
          (progn
            (mcp-tool-to-sibyl-tool client server-name mcp-tool)
            (incf count))
        (error (e)
          (log-warn "mcp" "Failed to register tool ~a from ~a: ~a"
                    (gethash "name" mcp-tool "?") server-name e))))
    (log-info "mcp" "Registered ~a tools from ~a" count server-name)
    count))

(defun unregister-mcp-server-tools (server-name)
  "Remove all tools from a specific MCP server from Sibyl's registry."
  (let ((prefix (format nil "~a:" server-name))
        (removed 0))
    (dolist (tool (sibyl.tools:list-tools))
      (when (and (>= (length (sibyl.tools:tool-name tool)) (length prefix))
                 (string= prefix (subseq (sibyl.tools:tool-name tool)
                                         0 (length prefix))))
        (sibyl.tools::unregister-tool (sibyl.tools:tool-name tool))
        (incf removed)))
    (log-debug "mcp" "Unregistered ~a tools from ~a" removed server-name)
    removed))

(defun connect-and-register-mcp-server (name url &key headers)
  "Connect to an MCP server, initialize it, discover tools, register them.
   NAME — human-readable server name (used as tool prefix).
   URL  — MCP server endpoint URL.
   HEADERS — optional extra HTTP headers (alist).
   Returns the mcp-client instance."
  (let ((client (make-mcp-client :name name :url url :headers headers)))
    (handler-case
        (progn
          (mcp-initialize client)
          (register-mcp-server-tools client name)
          ;; Store in server registry
          (bt:with-recursive-lock-held (*mcp-servers-lock*)
            (setf (gethash name *mcp-servers*) client))
          client)
      (error (e)
        (setf (mcp-client-status client) :error)
        (log-error "mcp" "Failed to connect to ~a (~a): ~a" name url e)
        (error e)))))

(defun disconnect-mcp-server (name)
  "Disconnect from a named MCP server and remove its tools."
  (let ((client (find-mcp-server name)))
    (when client
      (unregister-mcp-server-tools name)
      (handler-case (mcp-disconnect client)
        (error (e)
          (log-debug "mcp" "Disconnect error (non-fatal): ~a" e)))
      (bt:with-recursive-lock-held (*mcp-servers-lock*)
        (remhash name *mcp-servers*))
      t)))

;;; ============================================================
;;; Config-based auto-connect
;;; ============================================================

(defun %collect-mcp-server-configs ()
  "Scan config for mcp.servers.* entries. Returns a list of
   (name url headers-alist) tuples."
  (let ((servers (make-hash-table :test 'equal)))
    ;; Scan for mcp.servers.<name>.url pattern
    ;; Config keys are flat: "mcp.servers.exa.url", "mcp.servers.exa.api-key", etc.
    (when sibyl.config:*config*
      (maphash
       (lambda (key value)
         (when (and (stringp key)
                    (>= (length key) 13)
                    (string= "mcp.servers." (subseq key 0 12)))
           (let* ((rest (subseq key 12))
                  (dot-pos (position #\. rest)))
             (when dot-pos
               (let ((server-name (subseq rest 0 dot-pos))
                     (field (subseq rest (1+ dot-pos))))
                 ;; Ensure entry exists
                 (unless (gethash server-name servers)
                   (setf (gethash server-name servers)
                         (make-hash-table :test 'equal)))
                 (setf (gethash field (gethash server-name servers)) value))))))
       sibyl.config:*config*))
    ;; Convert to list of tuples
    (let ((result nil))
      (maphash
       (lambda (name fields)
         (let ((url (gethash "url" fields))
               (api-key (gethash "api-key" fields))
               (tools-filter (gethash "tools" fields)))
           (when url
             ;; Append tools filter to URL if specified
             (when (and tools-filter (plusp (length tools-filter))
                        (not (search "tools=" url)))
               (setf url (format nil "~a~atools=~a"
                                 url
                                 (if (search "?" url) "&" "?")
                                 tools-filter)))
             ;; Append API key to URL if provided (Exa-style)
             (when (and api-key (plusp (length api-key))
                        (not (search "exaApiKey=" url)))
               (setf url (format nil "~a~aexaApiKey=~a"
                                 url
                                 (if (search "?" url) "&" "?")
                                 api-key)))
             (push (list name url nil) result))))
       servers)
      (nreverse result))))

(defun connect-configured-mcp-servers ()
  "Read MCP server configs and connect to each one.
   Returns the number of successfully connected servers."
  (let ((configs (%collect-mcp-server-configs))
        (connected 0))
    (when configs
      (log-info "mcp" "Found ~a MCP server(s) in config" (length configs)))
    (dolist (cfg configs)
      (destructuring-bind (name url headers) cfg
        (handler-case
            (progn
              (connect-and-register-mcp-server name url :headers headers)
              (incf connected))
          (error (e)
            (log-warn "mcp" "Failed to connect MCP server ~a: ~a" name e)))))
    connected))

(defun disconnect-all-mcp-servers ()
  "Disconnect from all MCP servers and remove their tools."
  (let ((names nil))
    (bt:with-recursive-lock-held (*mcp-servers-lock*)
      (maphash (lambda (k v) (declare (ignore v)) (push k names)) *mcp-servers*))
    (dolist (name names)
      (handler-case (disconnect-mcp-server name)
        (error (e)
          (log-debug "mcp" "Error disconnecting ~a: ~a" name e))))
    (length names)))
