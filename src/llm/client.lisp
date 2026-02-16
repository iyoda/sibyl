;;;; client.lisp — Generic LLM client protocol
;;;; Uses CLOS generic functions for provider-agnostic LLM access.

(in-package #:sibyl.llm)

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

(defun http-post-json (url headers body)
  "POST JSON BODY to URL with HEADERS. Returns parsed JSON as hash-table."
  (let ((json-body (with-output-to-string (s)
                     (yason:encode body s))))
    (multiple-value-bind (response-body status-code)
        (handler-case
            (dex:post url
                      :headers (append headers
                                       '(("Content-Type" . "application/json")))
                      :content json-body)
          (dex:http-request-failed (e)
            (let ((code (dex:response-status e))
                  (body (dex:response-body e)))
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
