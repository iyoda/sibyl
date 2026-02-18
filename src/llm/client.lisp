;;;; client.lisp — Generic LLM client protocol
;;;; Uses CLOS generic functions for provider-agnostic LLM access.

(in-package #:sibyl.llm)

(defvar *streaming-text-callback* nil
  "When non-nil, a function (lambda (text) ...) called with each text chunk
   during streaming LLM responses. When nil (default), non-streaming mode.

   Providers check this variable and use streaming if non-nil:
     (when *streaming-text-callback*
       (funcall *streaming-text-callback* text-chunk))

   The REPL binds this around agent-run:
     (let ((*streaming-text-callback* #'write-token))
       (agent-run agent input))")

;;; ============================================================
;;; Client protocol (CLOS generic functions)
;;; ============================================================

(defclass llm-client ()
  ((api-key     :initarg :api-key
                :accessor client-api-key
                :type string)
   (model       :initarg :model
                :accessor client-model
                :type string)
   (max-tokens  :initarg :max-tokens
                :accessor client-max-tokens
                :initform 4096
                :type integer)
   (temperature :initarg :temperature
                :accessor client-temperature
                :initform 0.0
                :type number)
   (base-url    :initarg :base-url
                :accessor client-base-url
                :type string))
  (:documentation "Base class for LLM API clients."))

(defgeneric complete (client messages &key)
  (:documentation
   "Send MESSAGES to the LLM and return an assistant message.
    MESSAGES is a list of message structs.
    Returns: a message struct with :assistant role."))

(defgeneric complete-with-tools (client messages tools &key)
  (:documentation
   "Send MESSAGES with TOOLS schema to the LLM.
    TOOLS is a list of tool schema plists.
    Returns: a message struct, possibly containing tool-calls."))

(defgeneric count-tokens (client text)
  (:documentation
   "Estimate token count for TEXT. Provider-specific approximation."))

;;; ============================================================
;;; HTTP helpers (shared across providers)
;;; ============================================================

(defun %to-json-key (key)
  "Normalize JSON object keys to strings when possible."
  (cond
    ((stringp key) key)
    ((symbolp key) (string-downcase (symbol-name key)))
    (t key)))

(defun to-json-value (value)
  "Recursively convert nested alists to hash tables for yason encoding.
   - Alist (list of (string . val) conses) → hash-table (JSON object)
   - Other lists → list (JSON array)
   - Atoms → passed through"
  (cond
    ((eq value :null) :null)
    ((hash-table-p value)
      (let ((ht (make-hash-table :test 'equal)))
        (maphash (lambda (k v)
                  (setf (gethash (%to-json-key k) ht) (to-json-value v)))
                value)
        ht))
    ;; Alist: non-empty list where every element is a cons with a string key
    ((and (consp value)
          (every (lambda (x)
                   (and (consp x)
                        (or (stringp (car x)) (symbolp (car x)))))
                 value))
      (let ((ht (make-hash-table :test 'equal)))
        (dolist (pair value ht)
          (setf (gethash (%to-json-key (car pair)) ht)
                (to-json-value (cdr pair))))))
    ;; Regular list → JSON array
    ((listp value)
     (mapcar #'to-json-value value))
    ;; Atoms
    (t value)))

(defun read-response-body (body)
  "Ensure BODY is a string. Reads from stream if necessary, returns string or NIL."
  (handler-case
      (etypecase body
        (string body)
        (stream (let ((content (alexandria:read-stream-content-into-string body)))
                  (close body)
                  content))
        (null nil))
    (error () nil)))

;;; ============================================================
;;; Retry with exponential backoff
;;; ============================================================

(defparameter *default-max-retries* 3
  "Default number of retries for transient HTTP failures.")

(defparameter *default-retry-base-delay* 2.0
  "Base delay in seconds for exponential backoff (delay = base * 2^attempt).")

(defparameter *default-retry-max-delay* 60.0
  "Maximum delay in seconds between retries.")

(defun %jitter (delay)
  "Add ±25% jitter to DELAY to avoid thundering herd."
  (let ((jitter (* delay 0.25 (- (random 2.0) 1.0))))
    (max 0.1 (+ delay jitter))))

(defun %retryable-error-p (condition)
  "Return T if CONDITION represents a transient, retryable error.
Retryable: connection refused, timeout, 429, 500, 502, 503, 504."
  (or
   ;; Rate limit (429) — already signalled as llm-rate-limit-error
   (typep condition 'llm-rate-limit-error)
   ;; Server errors (5xx) or timeout errors wrapped as llm-api-error
   (and (typep condition 'llm-api-error)
        (let ((code (llm-error-status-code condition))
              (msg (format nil "~a" condition)))
          (or (and code (>= code 500) (<= code 504))
              (search "timeout" (string-downcase msg)))))
   ;; Connection refused / network errors (not our conditions)
   (and (typep condition 'error)
        (not (typep condition 'llm-api-error))
        (not (typep condition 'llm-error)))))

(defun call-with-retry (fn &key (max-retries *default-max-retries*)
                                (base-delay *default-retry-base-delay*)
                                (max-delay *default-retry-max-delay*)
                                (description "HTTP request"))
  "Call FN, retrying on transient errors with exponential backoff + jitter.
FN is a zero-argument function. Returns the result of FN on success.
Signals the last error if all retries are exhausted."
  (let ((last-error nil))
    (dotimes (attempt (1+ max-retries))
      (handler-case
          (return-from call-with-retry (funcall fn))
        (error (e)
          (setf last-error e)
          (if (and (< attempt max-retries)
                   (%retryable-error-p e))
              (let ((delay (min max-delay
                                (%jitter (* base-delay (expt 2 attempt))))))
                (log-warn "llm" "~a failed (attempt ~a/~a): ~a — retrying in ~,1fs"
                          description (1+ attempt) (1+ max-retries) e delay)
                (sleep delay))
              ;; Non-retryable or last attempt: propagate
              (error e)))))
    ;; Should not reach here, but just in case
    (when last-error (error last-error))))

;;; ============================================================
;;; Timeout configuration
;;; ============================================================

(defparameter *default-connect-timeout* 10
  "Default TCP connect timeout in seconds for LLM HTTP requests.")

(defparameter *default-read-timeout* 300
  "Default read timeout in seconds for LLM HTTP requests.
Covers the full response wait for non-streaming calls.")

(defun effective-connect-timeout (&optional override)
  "Return the connect timeout to use: OVERRIDE, config, or default."
  (or override
      (let ((cfg (config-value "http.connect-timeout")))
        (when cfg (if (stringp cfg) (parse-integer cfg :junk-allowed t) cfg)))
      *default-connect-timeout*))

(defun effective-read-timeout (&optional override)
  "Return the read timeout to use: OVERRIDE, config, or default."
  (or override
      (let ((cfg (config-value "http.read-timeout")))
        (when cfg (if (stringp cfg) (parse-integer cfg :junk-allowed t) cfg)))
      *default-read-timeout*))

;;; ============================================================
;;; HTTP POST helpers
;;; ============================================================

(defun http-post-json (url headers body &key connect-timeout read-timeout)
  "POST JSON BODY to URL with HEADERS. Returns parsed JSON as hash-table.
CONNECT-TIMEOUT and READ-TIMEOUT are in seconds (defaults from config or *default-*-timeout*)."
  (log-debug "llm" "HTTP POST JSON to ~a" url)
  (let ((json-body (with-output-to-string (s)
                     (yason:encode (to-json-value body) s)))
        (ct (effective-connect-timeout connect-timeout))
        (rt (effective-read-timeout read-timeout)))
    (multiple-value-bind (response-body status-code)
        (handler-case
            (dex:post url
                      :headers (append headers
                                       '(("Content-Type" . "application/json")))
                      :content json-body
                      :connect-timeout ct
                      :read-timeout rt)
          (dex:http-request-failed (e)
            (let ((code (dex:response-status e))
                  (body (read-response-body (dex:response-body e))))
              (log-error "llm" "HTTP error ~a for ~a: ~@[~a~]" code url body)
              (if (= code 429)
                  (error 'llm-rate-limit-error
                         :message "Rate limited"
                         :status-code code
                         :body body)
                  (error 'llm-api-error
                         :message (format nil "HTTP ~a" code)
                         :status-code code
                         :body body))))
          ;; Socket-level timeouts (dexador raises usocket conditions or
          ;; SB-SYS:IO-TIMEOUT for connect/read timeouts)
          (error (e)
            (if (or (search "timeout" (string-downcase (princ-to-string e)))
                    (search "timed out" (string-downcase (princ-to-string e))))
                (progn
                  (log-error "llm" "HTTP timeout for ~a (connect: ~as, read: ~as): ~a"
                             url ct rt e)
                  (error 'llm-api-error
                         :message (format nil "HTTP timeout (connect: ~as, read: ~as)" ct rt)))
                (error e))))
      (declare (ignore status-code))
      (yason:parse response-body :object-as :hash-table))))

(defun parse-sse-stream (stream on-event on-done &key (on-error nil))
  "Parse a Server-Sent Events (SSE) stream.
   STREAM   — readable stream from dexador :want-stream t
   ON-EVENT — (lambda (event-type data-string) ...) called per event
   ON-DONE  — (lambda () ...) called when stream ends
   ON-ERROR — optional (lambda (condition) ...) for errors

   SSE format:
     event: message_start    <- event type line (optional)
     data: {\"key\":\"val\"}   <- data line (JSON string)
                             <- empty line = event boundary"
  (let ((event-type nil)
        (data-parts nil))
    (loop
      (let ((line (handler-case
                      (read-line stream nil :eof)
                    (error (e)
                      (when on-error (funcall on-error e))
                      (return)))))
        (when (eq line :eof)
          (funcall on-done)
          (return))
        (cond
          ((and (>= (length line) 6)
                (string= "event:" (subseq line 0 6)))
           (setf event-type (string-trim '(#\Space #\Tab) (subseq line 6))))
          ((and (>= (length line) 5)
                (string= "data:" (subseq line 0 5)))
           (let ((data (string-trim '(#\Space #\Tab) (subseq line 5))))
             (push data data-parts)))
          ((string= line "")
           (when data-parts
             (let ((data-str (apply #'concatenate 'string (nreverse data-parts))))
               (unless (string= data-str "[DONE]")
                 (funcall on-event (or event-type "data") data-str)))
             (setf event-type nil
                   data-parts nil))))))))

(defun http-post-stream (url headers body on-event on-done
                         &key (on-error nil) connect-timeout read-timeout)
  "POST JSON BODY to URL, read response as SSE stream.
   ON-EVENT — called with (event-type data-string) per SSE event
   ON-DONE  — called when stream completes
   ON-ERROR — optional error handler
   CONNECT-TIMEOUT / READ-TIMEOUT — in seconds (defaults from config)."
  (log-debug "llm" "HTTP POST stream to ~a" url)
  (let ((json-body (with-output-to-string (s)
                     (yason:encode (to-json-value body) s)))
        (ct (effective-connect-timeout connect-timeout))
        (rt (effective-read-timeout read-timeout)))
    (handler-case
        (let ((stream (dex:post url
                                 :headers (append headers
                                                  '(("Content-Type" . "application/json")))
                                 :content json-body
                                 :want-stream t
                                 :connect-timeout ct
                                 :read-timeout rt)))
          (unwind-protect
               (parse-sse-stream stream on-event on-done :on-error on-error)
            (close stream)))
      (dex:http-request-failed (e)
        (let ((code (dex:response-status e))
              (body (read-response-body (dex:response-body e))))
          (log-error "llm" "HTTP error ~a for ~a: ~@[~a~]" code url body)
          (if (= code 429)
              (error 'llm-rate-limit-error
                     :message "Rate limited"
                     :status-code code
                     :body body)
               (error 'llm-api-error
                      :message (format nil "HTTP ~a" code)
                      :status-code code
                      :body body))))
      (error (e)
        (if (or (search "timeout" (string-downcase (princ-to-string e)))
                (search "timed out" (string-downcase (princ-to-string e))))
            (progn
              (log-error "llm" "HTTP stream timeout for ~a: ~a" url e)
              (error 'llm-api-error
                     :message (format nil "HTTP stream timeout (connect: ~as, read: ~as)" ct rt)))
            (error e))))))

(defun parse-ndjson-stream (stream on-chunk on-done &key (on-error nil))
  "Parse a newline-delimited JSON (NDJSON) stream.
   STREAM   — readable stream from dexador :want-stream t
   ON-CHUNK — (lambda (parsed-object) ...) called per JSON object
   ON-DONE  — (lambda () ...) called when stream ends
   ON-ERROR — optional (lambda (condition) ...) for parse errors
   
   NDJSON format: one JSON object per line, empty lines skipped."
  (loop
    (let ((line (handler-case
                    (read-line stream nil :eof)
                  (error (e)
                    (when on-error (funcall on-error e))
                    (return)))))
      (when (eq line :eof)
        (funcall on-done)
        (return))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
        (unless (string= trimmed "")
          (handler-case
              (let ((parsed (yason:parse trimmed :object-as :hash-table)))
                (funcall on-chunk parsed))
            (error (e)
              (if on-error
                  (funcall on-error e)
                  (warn "parse-ndjson-stream: failed to parse line: ~a" trimmed)))))))))

(defun http-post-ndjson-stream (url headers body on-chunk on-done
                                &key (on-error nil) connect-timeout read-timeout)
  "POST JSON BODY to URL, read response as NDJSON stream.
   ON-CHUNK — called with (parsed-object) per NDJSON line
   ON-DONE  — called when stream completes
   ON-ERROR — optional error handler
   CONNECT-TIMEOUT / READ-TIMEOUT — in seconds (defaults from config)."
  (log-debug "llm" "HTTP POST NDJSON stream to ~a" url)
  (let ((json-body (with-output-to-string (s)
                     (yason:encode (to-json-value body) s)))
        (ct (effective-connect-timeout connect-timeout))
        (rt (effective-read-timeout read-timeout)))
    (handler-case
         (let* ((raw-stream (dex:post url
                                       :headers (append headers
                                                        '(("Content-Type" . "application/json")))
                                       :content json-body
                                       :want-stream t
                                       :connect-timeout ct
                                       :read-timeout rt))
                (stream (flexi-streams:make-flexi-stream raw-stream :external-format :utf-8)))
           (unwind-protect
                (parse-ndjson-stream stream on-chunk on-done :on-error on-error)
             (close raw-stream)))
      (dex:http-request-failed (e)
        (let ((code (dex:response-status e))
              (body (read-response-body (dex:response-body e))))
          (log-error "llm" "HTTP error ~a for ~a: ~@[~a~]" code url body)
          (if (= code 429)
              (error 'llm-rate-limit-error
                     :message "Rate limited"
                     :status-code code
                     :body body)
              (error 'llm-api-error
                     :message (format nil "HTTP ~a" code)
                     :status-code code
                     :body body))))
      (error (e)
        (if (or (search "timeout" (string-downcase (princ-to-string e)))
                (search "timed out" (string-downcase (princ-to-string e))))
            (progn
              (log-error "llm" "HTTP NDJSON stream timeout for ~a: ~a" url e)
              (error 'llm-api-error
                     :message (format nil "HTTP NDJSON stream timeout (connect: ~as, read: ~as)"
                                      ct rt)))
            (error e))))))

(defun build-headers (client &rest extra-headers)
  "Build HTTP headers for CLIENT with optional EXTRA-HEADERS."
  (append extra-headers
          (list (cons "User-Agent" "sibyl/0.1.0"))))

;;; ============================================================
;;; Default token counting (rough estimate)
;;; ============================================================

(defmethod count-tokens ((client llm-client) text)
  "Rough token estimate: ~4 characters per token."
  (ceiling (length text) 4))

;;; ============================================================
;;; Convenience: call LLM from within tool execution context
;;; ============================================================

(defun call-llm (messages &key tools)
  "Call the LLM using the current agent's client (via *current-agent*).
   MESSAGES is a list of message plists.
   TOOLS, if supplied, is a JSON schema list for tool-calling.
   Returns (values response-message usage-plist).
   Signals an error if called outside an agent-step context."
  (let ((agent sibyl.agent::*current-agent*))
    (unless agent
      (error "call-llm: no current agent bound. Must be called within agent-step."))
    (let ((client (sibyl.agent:agent-client agent)))
      (if tools
          (complete-with-tools client messages tools)
          (complete client messages)))))
