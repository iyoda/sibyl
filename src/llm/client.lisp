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

(defun http-post-json (url headers body)
  "POST JSON BODY to URL with HEADERS. Returns parsed JSON as hash-table."
  (log-debug "llm" "HTTP POST JSON to ~a" url)
  (let ((json-body (with-output-to-string (s)
                     (yason:encode (to-json-value body) s))))
    (multiple-value-bind (response-body status-code)
        (handler-case
            (dex:post url
                      :headers (append headers
                                       '(("Content-Type" . "application/json")))
                      :content json-body)
          (dex:http-request-failed (e)
            (let ((code (dex:response-status e))
                  (body (dex:response-body e)))
              (log-error "llm" "HTTP error ~a for ~a" code url)
              (if (= code 429)
                  (error 'llm-rate-limit-error
                         :message "Rate limited"
                         :status-code code
                         :body body)
                  (error 'llm-api-error
                         :message (format nil "HTTP ~a" code)
                         :status-code code
                         :body body)))))
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

(defun http-post-stream (url headers body on-event on-done &key (on-error nil))
  "POST JSON BODY to URL, read response as SSE stream.
   ON-EVENT — called with (event-type data-string) per SSE event
   ON-DONE  — called when stream completes
   ON-ERROR — optional error handler"
  (log-debug "llm" "HTTP POST stream to ~a" url)
  (let ((json-body (with-output-to-string (s)
                     (yason:encode (to-json-value body) s))))
    (handler-case
        (let ((stream (dex:post url
                                :headers (append headers
                                                 '(("Content-Type" . "application/json")))
                                :content json-body
                                :want-stream t)))
          (unwind-protect
               (parse-sse-stream stream on-event on-done :on-error on-error)
            (close stream)))
      (dex:http-request-failed (e)
        (let ((code (dex:response-status e))
              (body (dex:response-body e)))
          (log-error "llm" "HTTP error ~a for ~a" code url)
          (if (= code 429)
              (error 'llm-rate-limit-error
                     :message "Rate limited"
                     :status-code code
                     :body body)
              (error 'llm-api-error
                     :message (format nil "HTTP ~a" code)
                     :status-code code
                     :body body)))))))

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
