;;;; message.lisp — Message and conversation data structures
;;;; The core data model for LLM interactions.

(in-package #:sibyl.llm)

;;; ============================================================
;;; Tool call representation
;;; ============================================================

(defclass tool-call ()
  ((id        :initarg :id        :accessor tool-call-id        :initform "")
   (name      :initarg :name      :accessor tool-call-name      :initform "")
   (arguments :initarg :arguments :accessor tool-call-arguments :initform nil))
  (:documentation "Represents a tool/function call requested by the LLM."))

(defun make-tool-call (&key (id "") (name "") (arguments nil))
  "Create a new tool-call instance."
  (make-instance 'tool-call :id id :name name :arguments arguments))

;;; ============================================================
;;; Message
;;; ============================================================

(defstruct (message (:constructor %make-message))
  "A single message in a conversation."
  (role         :user    :type keyword)     ; :system :user :assistant :tool
  (content      ""       :type (or string list null))  ; string for most roles; list of content-block alists for :system
  (tool-calls   nil      :type list)        ; list of tool-call structs
  (tool-call-id nil      :type (or string null))  ; for :tool role
  (timestamp    ""       :type string)
  (thinking     nil      :type (or string null))   ; extended thinking content
  (thinking-signature nil :type (or string null)))  ; signature for thinking block (required by Anthropic API)

(defun make-message (&key role content tool-calls tool-call-id thinking thinking-signature)
  "Create a message with automatic timestamp."
  (%make-message :role role
                 :content content
                 :tool-calls tool-calls
                 :tool-call-id tool-call-id
                 :timestamp (timestamp-now)
                 :thinking thinking
                 :thinking-signature thinking-signature))

;;; Convenience constructors

(defun system-message (content)
  "Create a system message.
CONTENT may be a plain string or a list of content-block alists
  (e.g. ((\"type\" . \"text\") (\"text\" . \"...\")))).
The Anthropic API accepts both forms for the top-level \"system\" field."
  (make-message :role :system :content content))

(defun user-message (content)
  "Create a user message."
  (make-message :role :user :content content))

(defun assistant-message (content &key tool-calls thinking thinking-signature)
  "Create an assistant message, optionally with tool calls and extended thinking."
  (make-message :role :assistant :content content :tool-calls tool-calls
                :thinking thinking :thinking-signature thinking-signature))

(defun tool-result-message (tool-call-id content)
  "Create a tool result message."
  (make-message :role :tool :content content :tool-call-id tool-call-id))

;;; ============================================================
;;; Conversation — ordered sequence of messages
;;; ============================================================

(defstruct (conversation (:constructor %make-conversation))
  "An ordered conversation of messages."
  (messages nil :type list)
  (lock (bt:make-lock "conversation-lock")))

(defun make-conversation (&optional initial-messages)
  "Create a new conversation, optionally with initial messages."
  (%make-conversation :messages (copy-list initial-messages)))

(defun conversation-push (conversation message)
  "Thread-safely append MESSAGE to CONVERSATION."
  (bt:with-lock-held ((conversation-lock conversation))
    (setf (conversation-messages conversation)
          (append (conversation-messages conversation) (list message))))
  message)

(defun conversation-clear (conversation)
  "Clear all messages from CONVERSATION."
  (bt:with-lock-held ((conversation-lock conversation))
    (setf (conversation-messages conversation) nil)))

(defun conversation-to-list (conversation)
  "Return a copy of the conversation messages as a list."
  (bt:with-lock-held ((conversation-lock conversation))
    (copy-list (conversation-messages conversation))))

(defun conversation-length (conversation)
  "Return the number of messages in CONVERSATION."
  (bt:with-lock-held ((conversation-lock conversation))
    (length (conversation-messages conversation))))
