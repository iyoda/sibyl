(in-package :sibyl.tests)

;;; Test suite for Anthropic streaming cache token extraction

(def-suite anthropic-streaming-cache-tests
  :description "Tests for Anthropic streaming cache token extraction"
  :in sibyl-tests)

(in-suite anthropic-streaming-cache-tests)

;; Test 1: parse-anthropic-sse-events extracts cache tokens from message_start
(test parse-message-start-with-cache-tokens
  "parse-anthropic-sse-events extracts cache_read_input_tokens and cache_creation_input_tokens from message_start"
  (let* ((data-str "{\"type\":\"message_start\",\"message\":{\"id\":\"msg_123\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"model\":\"claude-3-5-sonnet-20241022\",\"stop_reason\":null,\"stop_sequence\":null,\"usage\":{\"input_tokens\":100,\"cache_creation_input_tokens\":50,\"cache_read_input_tokens\":30,\"output_tokens\":0}}}"))
    (let ((parsed (sibyl.llm::parse-anthropic-sse-events "message_start" data-str)))
      (is (equal "message_start" (getf parsed :event)))
      (is (= 100 (getf parsed :input-tokens)))
      (is (= 30 (getf parsed :cache-read-tokens)))
      (is (= 50 (getf parsed :cache-write-tokens))))))

;; Test 2: parse-anthropic-sse-events handles message_start without cache fields
(test parse-message-start-without-cache-tokens
  "parse-anthropic-sse-events defaults to nil when cache fields are missing"
  (let* ((data-str "{\"type\":\"message_start\",\"message\":{\"id\":\"msg_123\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"model\":\"claude-3-5-sonnet-20241022\",\"stop_reason\":null,\"stop_sequence\":null,\"usage\":{\"input_tokens\":100,\"output_tokens\":0}}}"))
    (let ((parsed (sibyl.llm::parse-anthropic-sse-events "message_start" data-str)))
      (is (equal "message_start" (getf parsed :event)))
      (is (= 100 (getf parsed :input-tokens)))
      (is (null (getf parsed :cache-read-tokens)))
      (is (null (getf parsed :cache-write-tokens))))))

;; Test 3: complete-anthropic-streaming returns actual cache values in usage-plist
(test complete-anthropic-streaming-returns-cache-tokens
  "complete-anthropic-streaming returns actual cache token values from message_start event"
  (let* ((client (make-instance 'sibyl.llm::anthropic-client
                                :model "claude-3-5-sonnet-20241022"
                                :api-key "test-key"
                                :max-tokens 1024
                                :temperature 1.0))
         (messages (list (sibyl.llm:user-message "Test")))
         (mock-response-events
          '(("message_start" . "{\"type\":\"message_start\",\"message\":{\"id\":\"msg_123\",\"usage\":{\"input_tokens\":100,\"cache_creation_input_tokens\":50,\"cache_read_input_tokens\":30,\"output_tokens\":0}}}")
            ("content_block_start" . "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}")
            ("content_block_delta" . "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Hello\"}}")
            ("content_block_stop" . "{\"type\":\"content_block_stop\",\"index\":0}")
            ("message_delta" . "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\",\"stop_sequence\":null},\"usage\":{\"output_tokens\":20}}")
            ("message_stop" . "{\"type\":\"message_stop\"}"))))
    ;; Mock http-post-stream to simulate SSE events
    (let ((original-http-post-stream (symbol-function 'sibyl.llm::http-post-stream)))
      (unwind-protect
           (progn
             (setf (symbol-function 'sibyl.llm::http-post-stream)
                   (lambda (url headers body event-callback completion-callback &key on-error connect-timeout read-timeout)
                     (declare (ignore url headers body on-error connect-timeout read-timeout))
                     ;; Simulate SSE events
                     (loop for (event-type . data-str) in mock-response-events
                           do (funcall event-callback event-type data-str))
                     ;; Call completion callback
                     (funcall completion-callback)))
             (let ((sibyl.llm::*streaming-text-callback* (lambda (text) (declare (ignore text)))))
               (multiple-value-bind (msg usage)
                   (sibyl.llm::complete-anthropic-streaming client messages nil)
                 (is (string= "Hello" (sibyl.llm:message-content msg)))
                 (is (= 100 (getf usage :input-tokens)))
                 (is (= 20 (getf usage :output-tokens)))
                 (is (= 30 (getf usage :cache-read-tokens)))
                 (is (= 50 (getf usage :cache-write-tokens))))))
        ;; Restore original function
        (setf (symbol-function 'sibyl.llm::http-post-stream) original-http-post-stream)))))

;;; ============================================================
;;; Test suite for thinking block serialization
;;; ============================================================

(def-suite anthropic-thinking-serialization-tests
  :description "Tests for thinking block serialization in messages-to-anthropic-format"
  :in sibyl-tests)

(in-suite anthropic-thinking-serialization-tests)

(test assistant-message-with-thinking-serializes-thinking-block
  "Assistant message with thinking slot should include thinking block in content array"
  (let* ((msg (sibyl.llm:assistant-message "Hello" :thinking "Let me think..."))
         (messages (list msg)))
    (multiple-value-bind (system api-msgs)
        (sibyl.llm::messages-to-anthropic-format messages)
      (declare (ignore system))
      (is (= 1 (length api-msgs)))
      (let* ((api-msg (first api-msgs))
             (content (cdr (assoc "content" api-msg :test #'string=))))
        ;; Content should be a list of blocks
        (is (listp content))
        ;; Should have 2 blocks: thinking + text
        (is (= 2 (length content)))
        ;; First block should be thinking
        (let ((thinking-block (first content)))
          (is (string= "thinking" (cdr (assoc "type" thinking-block :test #'string=))))
          (is (string= "Let me think..." (cdr (assoc "thinking" thinking-block :test #'string=)))))
        ;; Second block should be text
        (let ((text-block (second content)))
          (is (string= "text" (cdr (assoc "type" text-block :test #'string=))))
          (is (string= "Hello" (cdr (assoc "text" text-block :test #'string=)))))))))

(test assistant-message-without-thinking-has-no-thinking-block
  "Assistant message without thinking slot should not include thinking block"
  (let* ((msg (sibyl.llm:assistant-message "Hello"))
         (messages (list msg)))
    (multiple-value-bind (system api-msgs)
        (sibyl.llm::messages-to-anthropic-format messages)
      (declare (ignore system))
      (is (= 1 (length api-msgs)))
      (let* ((api-msg (first api-msgs))
             (content (cdr (assoc "content" api-msg :test #'string=))))
        ;; Content should be a list of blocks
        (is (listp content))
        ;; Should have only 1 block: text
        (is (= 1 (length content)))
        ;; Block should be text
        (let ((text-block (first content)))
          (is (string= "text" (cdr (assoc "type" text-block :test #'string=))))
          (is (string= "Hello" (cdr (assoc "text" text-block :test #'string=)))))))))

(test assistant-message-with-thinking-and-tool-calls-serializes-all-blocks
  "Assistant message with thinking, text, and tool calls should serialize all blocks in correct order"
  (let* ((tool-call (sibyl.llm::make-tool-call
                     :id "call_123"
                     :name "test-tool"
                     :arguments '(("arg1" . "value1"))))
         (msg (sibyl.llm:assistant-message "Using tool" 
                                           :thinking "I should use this tool"
                                           :tool-calls (list tool-call)))
         (messages (list msg)))
    (multiple-value-bind (system api-msgs)
        (sibyl.llm::messages-to-anthropic-format messages)
      (declare (ignore system))
      (is (= 1 (length api-msgs)))
      (let* ((api-msg (first api-msgs))
             (content (cdr (assoc "content" api-msg :test #'string=))))
        ;; Content should be a list of blocks
        (is (listp content))
        ;; Should have 3 blocks: thinking + text + tool_use
        (is (= 3 (length content)))
        ;; First block should be thinking
        (let ((thinking-block (first content)))
          (is (string= "thinking" (cdr (assoc "type" thinking-block :test #'string=))))
          (is (string= "I should use this tool" (cdr (assoc "thinking" thinking-block :test #'string=)))))
        ;; Second block should be text
        (let ((text-block (second content)))
          (is (string= "text" (cdr (assoc "type" text-block :test #'string=))))
          (is (string= "Using tool" (cdr (assoc "text" text-block :test #'string=)))))
        ;; Third block should be tool_use
        (let ((tool-block (third content)))
          (is (string= "tool_use" (cdr (assoc "type" tool-block :test #'string=))))
          (is (string= "call_123" (cdr (assoc "id" tool-block :test #'string=))))
          (is (string= "test-tool" (cdr (assoc "name" tool-block :test #'string=)))))))))

;;; ============================================================
;;; Test suite for thinking config and capabilities
;;; ============================================================

(def-suite anthropic-thinking-config-tests
  :description "Tests for thinking.effort config and model capabilities"
  :in sibyl-tests)

(in-suite anthropic-thinking-config-tests)

;; Test 1: thinking.effort config key defaults to "high"
(test thinking-effort-config-default
  "thinking.effort config key should default to 'high'"
  (sibyl:with-config ()
    (is (string= "high" (sibyl:config-value "thinking.effort")))))

;; Test 2: Opus 4-6 has :thinking capability
(test opus-4-6-has-thinking-capability
  "Claude Opus 4-6 should have :thinking in capabilities"
  (let ((opus-model (gethash "claude-opus-4-6" sibyl.llm::*model-registry*)))
    (is (not (null opus-model)))
    (is (member :thinking (sibyl.llm::model-capabilities opus-model)))))

;; Test 3: Sonnet 4-6 has :thinking capability
(test sonnet-4-6-has-thinking-capability
  "Claude Sonnet 4-6 should have :thinking in capabilities"
  (let ((sonnet-model (gethash "claude-sonnet-4-6" sibyl.llm::*model-registry*)))
    (is (not (null sonnet-model)))
    (is (member :thinking (sibyl.llm::model-capabilities sonnet-model)))))

;; Test 4: Haiku does NOT have :thinking capability
(test haiku-no-thinking-capability
  "Claude Haiku should NOT have :thinking in capabilities"
  (let ((haiku-model (gethash "claude-haiku-4-5-20251015" sibyl.llm::*model-registry*)))
    (is (not (null haiku-model)))
    (is (not (member :thinking (sibyl.llm::model-capabilities haiku-model))))))

;;; ============================================================
;;; Test suite for anthropic thinking params helper
;;; ============================================================

(def-suite anthropic-thinking-params-tests
  :description "Tests for Anthropic thinking params helper"
  :in sibyl-tests)

(in-suite anthropic-thinking-params-tests)

(test anthropic-thinking-params-opus-4-6
  "Opus 4-6 should include thinking params with high effort"
  (sibyl:with-config ()
    (let* ((params (sibyl.llm::anthropic-thinking-params "claude-opus-4-6"))
           (thinking (cdr (assoc "thinking" params :test #'string=)))
           (output-config (cdr (assoc "output_config" params :test #'string=))))
      (is (listp params))
      (is (string= "adaptive" (cdr (assoc "type" thinking :test #'string=))))
      (is (string= "high" (cdr (assoc "effort" output-config :test #'string=)))))))

(test anthropic-thinking-params-sonnet-4-6
  "Sonnet 4-6 should include thinking params with high effort"
  (sibyl:with-config ()
    (let* ((params (sibyl.llm::anthropic-thinking-params "claude-sonnet-4-6"))
           (thinking (cdr (assoc "thinking" params :test #'string=)))
           (output-config (cdr (assoc "output_config" params :test #'string=))))
      (is (listp params))
      (is (string= "adaptive" (cdr (assoc "type" thinking :test #'string=))))
      (is (string= "high" (cdr (assoc "effort" output-config :test #'string=)))))))

(test anthropic-thinking-params-haiku
  "Haiku should not include thinking params"
  (sibyl:with-config ()
    (is (null (sibyl.llm::anthropic-thinking-params "claude-haiku-4-5-20251015")))))

(test anthropic-thinking-params-sonnet-max-downgrades
  "Sonnet effort max should downgrade to high with warning"
  (let ((output (make-string-output-stream)))
    (let ((sibyl.logging:*log-level* :warn)
          (sibyl.logging:*log-stream* output))
      (sibyl:with-config ()
        (setf (sibyl:config-value "thinking.effort") "max")
        (let* ((params (sibyl.llm::anthropic-thinking-params "claude-sonnet-4-6"))
               (output-config (cdr (assoc "output_config" params :test #'string=))))
          (is (string= "high" (cdr (assoc "effort" output-config :test #'string=)))))))
    (let ((result (get-output-stream-string output)))
      (is (search "downgrading to high" result)))))

(test anthropic-thinking-params-effort-config-low
  "thinking.effort config should control effort level"
  (sibyl:with-config ()
    (setf (sibyl:config-value "thinking.effort") "low")
    (let* ((params (sibyl.llm::anthropic-thinking-params "claude-opus-4-6"))
           (output-config (cdr (assoc "output_config" params :test #'string=))))
      (is (string= "low" (cdr (assoc "effort" output-config :test #'string=)))))))
