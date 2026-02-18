;;;; message-test.lisp — Tests for message and conversation

(in-package #:sibyl.tests)

(def-suite message-tests
  :description "Tests for message and conversation utilities."
  :in sibyl-tests)

(in-suite message-tests)

(test message-creation
  "Messages are created with correct roles."
  (let ((sys (sibyl.llm:system-message "You are helpful."))
        (usr (sibyl.llm:user-message "Hello"))
        (ast (sibyl.llm:assistant-message "Hi there")))
    (is (eq :system (sibyl.llm:message-role sys)))
    (is (eq :user (sibyl.llm:message-role usr)))
    (is (eq :assistant (sibyl.llm:message-role ast)))
    (is (string= "Hello" (sibyl.llm:message-content usr)))))

(test conversation-operations
  "Conversations support push, length, and clear."
  (let ((conv (sibyl.llm:make-conversation)))
    ;; Empty
    (is (null (sibyl.llm:conversation-to-list conv)))

    ;; Push messages
    (sibyl.llm:conversation-push conv (sibyl.llm:user-message "First"))
    (sibyl.llm:conversation-push conv (sibyl.llm:assistant-message "Second"))

    (is (= 2 (length (sibyl.llm:conversation-to-list conv))))

    ;; Clear
    (sibyl.llm:conversation-clear conv)
    (is (null (sibyl.llm:conversation-to-list conv)))))

(test tool-call-creation
  "Tool calls are created with correct fields."
  (let ((tc (sibyl.llm:make-tool-call
             :id "call_123"
             :name "read-file"
             :arguments '(("path" . "/tmp/test.txt")))))
    (is (string= "call_123" (sibyl.llm:tool-call-id tc)))
    (is (string= "read-file" (sibyl.llm:tool-call-name tc)))
    (is (equal '(("path" . "/tmp/test.txt"))
               (sibyl.llm:tool-call-arguments tc)))))

(test tool-result-message
  "Tool result messages carry the tool-call-id."
  (let ((msg (sibyl.llm:tool-result-message "call_123" "file content here")))
    (is (eq :tool (sibyl.llm:message-role msg)))
    (is (string= "call_123" (sibyl.llm:message-tool-call-id msg)))
    (is (string= "file content here" (sibyl.llm:message-content msg)))))

(test anthropic-message-formatting
  "Anthropic payloads use content-block lists for user/assistant messages."
  (let ((messages (list (sibyl.llm:user-message "Hello")
                        (sibyl.llm:assistant-message "Hi there"))))
    (multiple-value-bind (system api-messages)
        (sibyl.llm::messages-to-anthropic-format messages)
      (declare (ignore system))
      (let* ((user-msg (first api-messages))
             (assistant-msg (second api-messages))
             (user-content (cdr (assoc "content" user-msg :test #'string=)))
             (assistant-content (cdr (assoc "content" assistant-msg :test #'string=))))
        (is (listp user-content))
        (is (string= "text"
                     (cdr (assoc "type" (first user-content) :test #'string=))))
        (is (string= "Hello"
                     (cdr (assoc "text" (first user-content) :test #'string=))))
        (is (listp assistant-content))
        (is (string= "text"
                     (cdr (assoc "type" (first assistant-content) :test #'string=))))
        (is (string= "Hi there"
                     (cdr (assoc "text" (first assistant-content) :test #'string=))))))))

(test anthropic-assistant-with-tool-calls
  "Assistant messages with tool calls produce text + tool_use blocks in order."
  (let* ((tc (sibyl.llm:make-tool-call
              :id "call_abc"
              :name "read-file"
              :arguments '(("path" . "/tmp/x"))))
         (ast (sibyl.llm:make-message :role :assistant
                                      :content "Sure, reading now."
                                      :tool-calls (list tc)))
         (api-messages nil))
    (multiple-value-bind (sys msgs)
        (sibyl.llm::messages-to-anthropic-format (list ast))
      (declare (ignore sys))
      (setf api-messages msgs))
    (let* ((api-msg (first api-messages))
           (content (cdr (assoc "content" api-msg :test #'string=))))
      ;; content must be a list
      (is (listp content))
      ;; first block is the text block
      (let ((text-block (first content)))
        (is (string= "text" (cdr (assoc "type" text-block :test #'string=))))
        (is (string= "Sure, reading now." (cdr (assoc "text" text-block :test #'string=)))))
      ;; second block is the tool_use block
      (let ((tool-block (second content)))
        (is (string= "tool_use" (cdr (assoc "type" tool-block :test #'string=))))
        (is (string= "call_abc" (cdr (assoc "id" tool-block :test #'string=))))
        (is (string= "read-file" (cdr (assoc "name" tool-block :test #'string=))))))))

(test anthropic-system-message-passthrough
  "System message content is passed through unchanged (not wrapped in blocks)."
  (let* ((messages (list (sibyl.llm:system-message "Be concise.")))
         (system nil))
    (multiple-value-bind (sys _msgs)
        (sibyl.llm::messages-to-anthropic-format messages)
      (declare (ignore _msgs))
      (setf system sys))
    ;; system must be the raw string, not a list
    (is (stringp system))
    (is (string= "Be concise." system))))

(test anthropic-tool-result-message
  "Tool result messages are wrapped in a user message with tool_result block."
  (let* ((messages (list (sibyl.llm:tool-result-message "call_xyz" "42")))
         (api-messages nil))
    (multiple-value-bind (_sys msgs)
        (sibyl.llm::messages-to-anthropic-format messages)
      (declare (ignore _sys))
      (setf api-messages msgs))
    (let* ((api-msg (first api-messages))
           (role (cdr (assoc "role" api-msg :test #'string=)))
           (content (cdr (assoc "content" api-msg :test #'string=)))
           (block (first content)))
      (is (string= "user" role))
      (is (listp content))
      (is (string= "tool_result" (cdr (assoc "type" block :test #'string=))))
      (is (string= "call_xyz" (cdr (assoc "tool_use_id" block :test #'string=))))
      (is (string= "42" (cdr (assoc "content" block :test #'string=)))))))

(test make-message-timestamp
  "make-message assigns an ISO-8601 timestamp."
  (let ((msg (sibyl.llm:make-message :role :user :content "hi")))
    (is (stringp (sibyl.llm:message-timestamp msg)))
    (is (search "T" (sibyl.llm:message-timestamp msg)))
    (is (char= #\Z (char (sibyl.llm:message-timestamp msg)
                         (1- (length (sibyl.llm:message-timestamp msg))))))))
