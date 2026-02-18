;;;; token-tracking-test.lisp — Tests for token usage tracking

(in-package #:sibyl.tests)

(def-suite token-tracking-suite
  :description "Token usage tracking tests"
  :in sibyl-tests)

(in-suite token-tracking-suite)

;; Test 1: parse-anthropic-response extracts usage as second value
(test parse-response-returns-usage
  "parse-anthropic-response returns (values message usage-plist)"
  (let* ((resp (make-hash-table :test 'equal))
         (content-block (make-hash-table :test 'equal))
         (usage-block (make-hash-table :test 'equal)))
    (setf (gethash "type" content-block) "text"
          (gethash "text" content-block) "Hello"
          (gethash "content" resp) (vector content-block)
          (gethash "input_tokens" usage-block) 150
          (gethash "output_tokens" usage-block) 50
          (gethash "usage" resp) usage-block)
    (multiple-value-bind (msg usage)
        (sibyl.llm::parse-anthropic-response resp)
      (is (string= "Hello" (sibyl.llm:message-content msg)))
      (is (= 150 (getf usage :input-tokens)))
      (is (= 50 (getf usage :output-tokens))))))

;; Test 2: cache tokens extracted when present
(test parse-response-extracts-cache-tokens
  "parse-anthropic-response extracts cache tokens from usage"
  (let* ((resp (make-hash-table :test 'equal))
         (content-block (make-hash-table :test 'equal))
         (usage-block (make-hash-table :test 'equal)))
    (setf (gethash "type" content-block) "text"
          (gethash "text" content-block) "ok"
          (gethash "content" resp) (vector content-block)
          (gethash "input_tokens" usage-block) 100
          (gethash "output_tokens" usage-block) 20
          (gethash "cache_read_input_tokens" usage-block) 900
          (gethash "cache_creation_input_tokens" usage-block) 200
          (gethash "usage" resp) usage-block)
    (multiple-value-bind (msg usage)
        (sibyl.llm::parse-anthropic-response resp)
      (declare (ignore msg))
      (is (= 900 (getf usage :cache-read-tokens)))
      (is (= 200 (getf usage :cache-write-tokens))))))

;; Test 3: usage is nil when usage field absent
(test parse-response-nil-usage-when-absent
  "parse-anthropic-response returns nil usage when no usage field"
  (let* ((resp (make-hash-table :test 'equal))
         (content-block (make-hash-table :test 'equal)))
    (setf (gethash "type" content-block) "text"
          (gethash "text" content-block) "hello"
          (gethash "content" resp) (vector content-block))
    (multiple-value-bind (msg usage)
        (sibyl.llm::parse-anthropic-response resp)
      (declare (ignore msg))
      (is (null usage)))))

;; Test 4: token-tracker accumulates correctly
(test token-tracker-accumulates
  "token-tracker correctly accumulates usage across multiple calls"
  (let ((tracker (sibyl.llm::make-token-tracker)))
    (sibyl.llm::tracker-add-usage tracker '(:input-tokens 100 :output-tokens 50))
    (sibyl.llm::tracker-add-usage tracker '(:input-tokens 200 :output-tokens 80))
    (sibyl.llm::tracker-add-usage tracker '(:input-tokens 150 :output-tokens 30))
    (is (= 450 (sibyl.llm::token-tracker-input-tokens tracker)))
    (is (= 160 (sibyl.llm::token-tracker-output-tokens tracker)))))

;; Test 5: token-tracker accumulates cache tokens
(test token-tracker-accumulates-cache
  "token-tracker accumulates cache read/write tokens"
  (let ((tracker (sibyl.llm::make-token-tracker)))
    (sibyl.llm::tracker-add-usage tracker '(:input-tokens 100 :output-tokens 20
                                             :cache-read-tokens 500 :cache-write-tokens 50))
    (sibyl.llm::tracker-add-usage tracker '(:input-tokens 50 :output-tokens 10
                                             :cache-read-tokens 800 :cache-write-tokens 0))
    (is (= 1300 (sibyl.llm::token-tracker-cache-read-tokens tracker)))
    (is (= 50 (sibyl.llm::token-tracker-cache-write-tokens tracker)))))

;; Test 6: nil usage is safe to add
(test token-tracker-handles-nil-usage
  "tracker-add-usage safely handles nil usage"
  (let ((tracker (sibyl.llm::make-token-tracker)))
    (sibyl.llm::tracker-add-usage tracker nil)  ; should not error
    (is (= 0 (sibyl.llm::token-tracker-input-tokens tracker)))))

;; Test 7: parse-anthropic-response handles thinking blocks
(test parse-response-handles-thinking-block
  "parse-anthropic-response extracts thinking content into message-thinking slot"
  (let* ((resp (make-hash-table :test 'equal))
         (thinking-block (make-hash-table :test 'equal))
         (text-block (make-hash-table :test 'equal)))
    (setf (gethash "type" thinking-block) "thinking"
          (gethash "thinking" thinking-block) "Let me reason about this..."
          (gethash "type" text-block) "text"
          (gethash "text" text-block) "The answer is 42"
          (gethash "content" resp) (vector thinking-block text-block))
    (multiple-value-bind (msg usage)
        (sibyl.llm::parse-anthropic-response resp)
      (declare (ignore usage))
      (is (string= "Let me reason about this..." (sibyl.llm:message-thinking msg)))
      (is (string= "The answer is 42" (sibyl.llm:message-content msg))))))

;; Test 8: no thinking in regular response → nil
(test parse-response-thinking-nil-when-absent
  "message-thinking is nil for responses without thinking blocks"
  (let* ((resp (make-hash-table :test 'equal))
         (text-block (make-hash-table :test 'equal)))
    (setf (gethash "type" text-block) "text"
          (gethash "text" text-block) "Normal response"
          (gethash "content" resp) (vector text-block))
    (multiple-value-bind (msg usage)
        (sibyl.llm::parse-anthropic-response resp)
      (declare (ignore usage))
      (is (null (sibyl.llm:message-thinking msg)))
      (is (string= "Normal response" (sibyl.llm:message-content msg))))))
