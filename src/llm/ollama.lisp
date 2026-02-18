;;;; ollama.lisp — Ollama LLM provider implementation
;;;; Local Ollama server client using the /api/chat endpoint.

(in-package #:sibyl.llm)

;;; ============================================================
;;; Ollama client
;;; ============================================================

(defclass ollama-client (llm-client)
  ()
  (:default-initargs
   :base-url "http://localhost:11434"
   :model "qwen3-coder:30b"
   :api-key "")
  (:documentation "Client for the local Ollama inference server."))

(defun make-ollama-client (&key (model "qwen3-coder:30b")
                                (host nil)
                                (max-tokens 4096)
                                (temperature 0.0))
  "Create an Ollama API client.
HOST overrides the server URL (default: http://localhost:11434).
Falls back to config key llm.ollama.host when HOST is nil."
  (log-debug "llm" "Creating Ollama client (model: ~a)" model)
  (make-instance 'ollama-client
                 :model model
                 :api-key ""
                 :max-tokens max-tokens
                 :temperature temperature
                 :base-url (or host
                               (config-value "llm.ollama.host")
                               "http://localhost:11434")))

;;; ============================================================
;;; URL helper
;;; ============================================================

(defun ollama-api-url (client path)
  "Build a full Ollama API URL by appending PATH to the client's base URL."
  (concatenate 'string (client-base-url client) path))

;;; ============================================================
;;; Message format conversion
;;; ============================================================

(defun ollama-recover-tool-name (id)
  "Recover tool name from a synthetic Ollama tool-call ID.
ID format: 'ollama-{tool-name}-{counter}'
Example: 'ollama-get_weather-0' => 'get_weather'"
  (if (and (>= (length id) 7)
           (string= "ollama-" (subseq id 0 7)))
      (let* ((without-prefix (subseq id 7))
             (dash-pos (position #\- without-prefix :from-end t)))
        (if dash-pos
            (subseq without-prefix 0 dash-pos)
            without-prefix))
      id))

(defun messages-to-ollama-format (messages)
  "Convert message structs to Ollama /api/chat message format."
  (mapcar
   (lambda (msg)
     (case (message-role msg)
       (:system
        `(("role"    . "system")
          ("content" . ,(message-content msg))))
       (:user
        `(("role"    . "user")
          ("content" . ,(message-content msg))))
       (:assistant
        (if (message-tool-calls msg)
            ;; Assistant with tool calls: content must be empty string
            `(("role"       . "assistant")
              ("content"    . "")
              ("tool_calls" . ,(mapcar
                                (lambda (tc)
                                  `(("function"
                                     . (("name"      . ,(tool-call-name tc))
                                        ("arguments" . ,(or (tool-call-arguments tc)
                                                            (make-hash-table
                                                             :test 'equal)))))))
                                (message-tool-calls msg))))
            ;; Plain assistant message
            `(("role"    . "assistant")
              ("content" . ,(or (message-content msg) "")))))
       (:tool
        ;; Tool result: recover tool name from synthetic ID
        `(("role"      . "tool")
          ("content"   . ,(message-content msg))
          ("tool_name" . ,(ollama-recover-tool-name
                           (or (message-tool-call-id msg) "")))))))
   messages))

;;; ============================================================
;;; Tool schema conversion
;;; ============================================================

(defun tools-to-ollama-format (tools)
  "Convert tool schemas to Ollama function-call format."
  (mapcar (lambda (ts)
            `(("type" . "function")
              ("function"
               . (("name"        . ,(getf ts :name))
                  ("description" . ,(getf ts :description))
                  ("parameters"  . ,(getf ts :parameters))))))
          tools))

;;; ============================================================
;;; Response parsing
;;; ============================================================

(defun parse-ollama-tool-calls (raw-tool-calls)
  "Parse tool_calls array from an Ollama response.
RAW-TOOL-CALLS is a vector or list as parsed from JSON.
Returns a list of tool-call structs with synthetic IDs.

IMPORTANT: Ollama delivers arguments as a hash-table (already
parsed from JSON), NOT a JSON string — do not double-parse."
  (let ((tcs (if (vectorp raw-tool-calls)
                 (coerce raw-tool-calls 'list)
                 raw-tool-calls))
        (counter 0))
    (mapcar (lambda (tc)
              (let* ((func     (gethash "function" tc))
                     (name     (gethash "name" func))
                     (raw-args (gethash "arguments" func))
                     ;; Convert hash-table to alist; already parsed, not a JSON string.
                     (args     (cond
                                 ((hash-table-p raw-args) (hash-to-alist raw-args))
                                 ((null raw-args)         nil)
                                 (t                       raw-args)))
                     (id       (format nil "ollama-~a-~a"
                                       name (prog1 counter (incf counter)))))
                (make-tool-call :id id :name name :arguments args)))
            tcs)))

(defun parse-ollama-response (response)
  "Parse a non-streaming Ollama /api/chat response.
Returns (values message usage-plist)."
  (let* ((msg           (gethash "message" response))
         (content       (gethash "content" msg))
         (raw-tcs       (gethash "tool_calls" msg))
         (tool-calls    (when raw-tcs
                          (parse-ollama-tool-calls raw-tcs)))
         (prompt-tokens (gethash "prompt_eval_count" response))
         (output-tokens (gethash "eval_count" response))
         (usage-plist   (list :input-tokens  (or prompt-tokens 0)
                              :output-tokens (or output-tokens 0))))
    (values
     (assistant-message (if (and content (not (string= content "")))
                            content
                            nil)
                        :tool-calls tool-calls)
     usage-plist)))

;;; ============================================================
;;; Request body builder (shared by streaming and non-streaming)
;;; ============================================================

(defun build-ollama-request (client messages &key tools stream)
  "Build the request body alist for POST /api/chat.
STREAM T => JSON true; STREAM NIL => JSON false (yason:false)."
  (let* ((api-messages (messages-to-ollama-format messages))
         (body `(("model"    . ,(client-model client))
                 ("messages" . ,api-messages)
                 ("stream"   . ,(if stream t yason:false))
                 ("options"  . (("temperature" . ,(client-temperature client))
                                ("num_predict" . ,(client-max-tokens client)))))))
    (if tools
        (append body `(("tools" . ,(tools-to-ollama-format tools))))
        body)))

;;; ============================================================
;;; Streaming implementation (NDJSON)
;;; ============================================================

(defun complete-ollama-streaming (client messages &key tools)
  "Stream Ollama NDJSON response, invoking *streaming-text-callback* per chunk.
Returns (values message usage-plist). Synchronous: completes when stream ends."
  (let ((content-chunks '())
        (tool-calls-raw nil)
        (final-usage    nil)
        (url            (ollama-api-url client "/api/chat"))
        (body           (build-ollama-request client messages
                                              :tools tools :stream t)))
    (http-post-ndjson-stream
     url
     '()                                ; Ollama is local — no auth headers
     body
     ;; on-chunk: called for each NDJSON line
     (lambda (chunk)
       (let* ((msg-obj (gethash "message" chunk))
              (text    (and msg-obj (gethash "content" msg-obj)))
              (done    (gethash "done" chunk)))
         (when (and text (not (string= text "")))
           (push text content-chunks)
           (when *streaming-text-callback*
             (funcall *streaming-text-callback* text)))
         (when done
           ;; Tool calls arrive in the final (done=true) chunk
           (let ((tc (and msg-obj (gethash "tool_calls" msg-obj))))
             (when tc (setf tool-calls-raw tc)))
           (setf final-usage
                 (list :input-tokens  (or (gethash "prompt_eval_count" chunk) 0)
                       :output-tokens (or (gethash "eval_count" chunk) 0))))))
     ;; on-done: http-post-ndjson-stream is synchronous; fires last
     (lambda () nil))
    ;; http-post-ndjson-stream returns synchronously after the stream ends
    (let ((content (format nil "~{~a~}" (nreverse content-chunks)))
          (tcs     (when tool-calls-raw
                     (parse-ollama-tool-calls tool-calls-raw))))
      (values (assistant-message (if (string= content "") nil content)
                                 :tool-calls tcs)
              final-usage))))

;;; ============================================================
;;; CLOS methods
;;; ============================================================

(defmethod complete ((client ollama-client) messages &key)
  "Send messages to the local Ollama server.
Returns (values message usage-plist)."
  (log-info "llm" "Ollama complete (model: ~a, streaming: ~a)"
            (client-model client)
            (if *streaming-text-callback* "yes" "no"))
  (if *streaming-text-callback*
      (complete-ollama-streaming client messages)
      (handler-case
          (let* ((body     (build-ollama-request client messages :stream nil))
                 (response (http-post-json
                            (ollama-api-url client "/api/chat")
                            '()                 ; no auth headers
                            body)))
            (parse-ollama-response response))
        (usocket:connection-refused-error ()
          (error 'llm-api-error
                 :message (format nil
                                  "Cannot connect to Ollama at ~a. Is it running?"
                                  (client-base-url client)))))))

(defmethod complete-with-tools ((client ollama-client) messages tools &key)
  "Send messages with tool schemas to the local Ollama server.
Returns (values message usage-plist)."
  (log-info "llm" "Ollama complete-with-tools (model: ~a, tools: ~a, streaming: ~a)"
            (client-model client)
            (length tools)
            (if *streaming-text-callback* "yes" "no"))
  (if *streaming-text-callback*
      (complete-ollama-streaming client messages :tools tools)
      (handler-case
          (let* ((body     (build-ollama-request client messages
                                                 :tools tools :stream nil))
                 (response (http-post-json
                            (ollama-api-url client "/api/chat")
                            '()                 ; no auth headers
                            body)))
            (parse-ollama-response response))
        (usocket:connection-refused-error ()
          (error 'llm-api-error
                 :message (format nil
                                  "Cannot connect to Ollama at ~a. Is it running?"
                                  (client-base-url client)))))))

(defmethod count-tokens ((client ollama-client) text)
  "Rough token estimate for Ollama models: ~4 characters per token."
  (ceiling (length text) 4))
