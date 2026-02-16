;;;; message-test.lisp — Tests for message and conversation

(in-package #:sibyl.tests)

(in-suite sibyl-tests)

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
