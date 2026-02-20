(in-package :sibyl.tests)

;;; Test suite for OpenAI response parsing with usage extraction

(def-suite openai-usage-tests
  :description "Tests for parse-openai-response usage extraction"
  :in sibyl-tests)

(in-suite openai-usage-tests)

;; Test 1: Normal case - extract usage with all fields
(test parse-openai-response-returns-usage
  "parse-openai-response returns (values message usage-plist)"
  (let* ((resp (make-hash-table :test 'equal))
         (msg-block (make-hash-table :test 'equal))
         (choice (make-hash-table :test 'equal))
         (usage-block (make-hash-table :test 'equal)))
    (setf (gethash "content" msg-block) "Hello from OpenAI"
          (gethash "message" choice) msg-block
          (gethash "choices" resp) (vector choice)
          (gethash "prompt_tokens" usage-block) 100
          (gethash "completion_tokens" usage-block) 50
          (gethash "usage" resp) usage-block)
    (multiple-value-bind (msg usage)
        (sibyl.llm::parse-openai-response resp)
      (is (string= "Hello from OpenAI" (sibyl.llm:message-content msg)))
      (is (= 100 (getf usage :input-tokens)))
      (is (= 50 (getf usage :output-tokens)))
      (is (= 0 (getf usage :cache-read-tokens)))
      (is (= 0 (getf usage :cache-write-tokens))))))

;; Test 2: Extract cached_tokens from prompt_tokens_details
(test parse-openai-response-extracts-cache-tokens
  "parse-openai-response extracts cached_tokens from prompt_tokens_details"
  (let* ((resp (make-hash-table :test 'equal))
         (msg-block (make-hash-table :test 'equal))
         (choice (make-hash-table :test 'equal))
         (usage-block (make-hash-table :test 'equal))
         (prompt-details (make-hash-table :test 'equal)))
    (setf (gethash "content" msg-block) "Cached response"
          (gethash "message" choice) msg-block
          (gethash "choices" resp) (vector choice)
          (gethash "prompt_tokens" usage-block) 100
          (gethash "completion_tokens" usage-block) 20
          (gethash "cached_tokens" prompt-details) 80
          (gethash "prompt_tokens_details" usage-block) prompt-details
          (gethash "usage" resp) usage-block)
    (multiple-value-bind (msg usage)
        (sibyl.llm::parse-openai-response resp)
      (declare (ignore msg))
      (is (= 100 (getf usage :input-tokens)))
      (is (= 20 (getf usage :output-tokens)))
      (is (= 80 (getf usage :cache-read-tokens)))
      (is (= 0 (getf usage :cache-write-tokens))))))

;; Test 3: Missing usage field returns nil
(test parse-openai-response-missing-usage
  "parse-openai-response returns nil usage when usage field is missing"
  (let* ((resp (make-hash-table :test 'equal))
         (msg-block (make-hash-table :test 'equal))
         (choice (make-hash-table :test 'equal)))
    (setf (gethash "content" msg-block) "No usage"
          (gethash "message" choice) msg-block
          (gethash "choices" resp) (vector choice))
    (multiple-value-bind (msg usage)
        (sibyl.llm::parse-openai-response resp)
      (is (string= "No usage" (sibyl.llm:message-content msg)))
      (is (null usage)))))

;; Test 4: Missing prompt_tokens_details defaults cache-read-tokens to 0
(test parse-openai-response-missing-prompt-details
  "parse-openai-response defaults cache-read-tokens to 0 when prompt_tokens_details is missing"
  (let* ((resp (make-hash-table :test 'equal))
         (msg-block (make-hash-table :test 'equal))
         (choice (make-hash-table :test 'equal))
         (usage-block (make-hash-table :test 'equal)))
    (setf (gethash "content" msg-block) "No cache details"
          (gethash "message" choice) msg-block
          (gethash "choices" resp) (vector choice)
          (gethash "prompt_tokens" usage-block) 150
          (gethash "completion_tokens" usage-block) 75
          (gethash "usage" resp) usage-block)
    (multiple-value-bind (msg usage)
        (sibyl.llm::parse-openai-response resp)
      (declare (ignore msg))
      (is (= 150 (getf usage :input-tokens)))
      (is (= 75 (getf usage :output-tokens)))
      (is (= 0 (getf usage :cache-read-tokens)))
      (is (= 0 (getf usage :cache-write-tokens))))))

(test parse-openai-responses-response-returns-usage
  "parse-openai-responses-response returns (values message usage-plist)"
  (let* ((resp (make-hash-table :test 'equal))
         (message (make-hash-table :test 'equal))
         (content (make-hash-table :test 'equal))
         (usage (make-hash-table :test 'equal)))
    (setf (gethash "text" content) "Hello from responses"
          (gethash "content" message) (vector content)
          (gethash "output" resp) (vector message)
          (gethash "input_tokens" usage) 33
          (gethash "output_tokens" usage) 11
          (gethash "usage" resp) usage)
    (multiple-value-bind (msg usage-plist)
        (sibyl.llm::parse-openai-responses-response resp)
      (is (string= "Hello from responses" (sibyl.llm:message-content msg)))
      (is (= 33 (getf usage-plist :input-tokens)))
      (is (= 11 (getf usage-plist :output-tokens))))))

(test codex-model-uses-responses-endpoint
  "complete routes codex model to /responses endpoint."
  (let* ((client (make-instance 'sibyl.llm::openai-client
                                :api-key "mock-key"
                                :model "gpt-5.2-codex"
                                :max-tokens 128
                                :base-url "http://mock-openai"))
         (messages (list (sibyl.llm:user-message "hello")))
         (captured-url nil)
         (captured-body nil))
    (flet ((mock-http-post-json (url headers body &key connect-timeout read-timeout)
             (declare (ignore headers connect-timeout read-timeout))
             (setf captured-url url)
             (setf captured-body body)
             (let ((resp (make-hash-table :test 'equal))
                   (message (make-hash-table :test 'equal))
                   (content (make-hash-table :test 'equal))
                   (usage (make-hash-table :test 'equal)))
               (setf (gethash "text" content) "ok"
                     (gethash "content" message) (vector content)
                     (gethash "output" resp) (vector message)
                     (gethash "input_tokens" usage) 1
                     (gethash "output_tokens" usage) 1
                     (gethash "usage" resp) usage)
               resp)))
      (let ((original-fn (symbol-function 'sibyl.llm::http-post-json)))
        (unwind-protect
             (progn
               (setf (symbol-function 'sibyl.llm::http-post-json) #'mock-http-post-json)
               (sibyl.llm:complete client messages)
               (is (search "/responses" captured-url))
               (is (null (gethash "temperature" captured-body))))
          (setf (symbol-function 'sibyl.llm::http-post-json) original-fn))))))

;;; ============================================================
;;; GPT-5 mini pricing tests (TDD: RED phase)
;;; ============================================================

(def-suite openai-pricing-tests
  :description "Tests for OpenAI model pricing entries in the pricing table."
  :in sibyl-tests)

(in-suite openai-pricing-tests)

;;; Test 1: GPT-5 mini lookup returns non-NIL pricing
(test gpt5-mini-lookup-returns-pricing
  "lookup-model-pricing for 'gpt-5-mini' returns a non-NIL plist."
  (let ((pricing (sibyl.llm::lookup-model-pricing "gpt-5-mini")))
    (is (not (null pricing)))
    (is (listp pricing))))

;;; Test 2: GPT-5 mini has correct input price
(test gpt5-mini-input-price
  "GPT-5 mini input price is 0.25 USD per 1M tokens."
  (let ((pricing (sibyl.llm::lookup-model-pricing "gpt-5-mini")))
    (is (= 0.25 (getf pricing :input)))))

;;; Test 3: GPT-5 mini has correct output price
(test gpt5-mini-output-price
  "GPT-5 mini output price is 1.00 USD per 1M tokens."
  (let ((pricing (sibyl.llm::lookup-model-pricing "gpt-5-mini")))
    (is (= 1.00 (getf pricing :output)))))

;;; Test 4: GPT-5 mini has correct cache-write price
(test gpt5-mini-cache-write-price
  "GPT-5 mini cache-write price is 0.0 USD per 1M tokens."
  (let ((pricing (sibyl.llm::lookup-model-pricing "gpt-5-mini")))
    (is (= 0.0 (getf pricing :cache-write)))))

;;; Test 5: GPT-5 mini has correct cache-read price
(test gpt5-mini-cache-read-price
  "GPT-5 mini cache-read price is 0.025 USD per 1M tokens."
  (let ((pricing (sibyl.llm::lookup-model-pricing "gpt-5-mini")))
    (is (= 0.025 (getf pricing :cache-read)))))

;;; Test 6: Cache-read is cheaper than regular input
(test gpt5-mini-cache-read-cheaper-than-input
  "GPT-5 mini cache-read (0.025) is cheaper than input (0.25)."
  (let ((pricing (sibyl.llm::lookup-model-pricing "gpt-5-mini")))
    (is (< (getf pricing :cache-read) (getf pricing :input)))))

;;; Test 7: Cost estimation with cached tokens is lower than without caching
(test gpt5-mini-cost-with-cache-lower
  "Cost with cached tokens is lower than cost without caching for GPT-5 mini."
  (let* ((cost-no-cache (sibyl.llm::estimate-cost-usd "gpt-5-mini"
                                                       :input-tokens 1000000))
         (cost-with-cache (sibyl.llm::estimate-cost-usd "gpt-5-mini"
                                                        :input-tokens 500000
                                                        :cache-read-tokens 500000)))
    ;; cost-no-cache: 1M input @ 0.25 = $0.25
    ;; cost-with-cache: 500k input @ 0.25 + 500k cache-read @ 0.025 = $0.125 + $0.0125 = $0.1375
    (is (< (getf cost-with-cache :total) (getf cost-no-cache :total)))))

;;; Test 8: No fallback warning when looking up GPT-5 mini
(test gpt5-mini-no-fallback-warning
  "Looking up GPT-5 mini does not trigger a fallback warning."
  (let ((warnings nil))
    (handler-bind ((warning (lambda (w)
                              (push (format nil "~a" w) warnings)
                              (muffle-warning w))))
      (sibyl.llm::lookup-model-pricing "gpt-5-mini"))
    (is (null warnings))))

;;; ============================================================
;;; OpenAI Streaming Usage Tests (TDD: Task 3)
;;; ============================================================

(def-suite openai-streaming-usage-tests
  :description "Tests for OpenAI streaming usage extraction with stream_options"
  :in sibyl-tests)

(in-suite openai-streaming-usage-tests)

;;; Test 1: complete-openai-streaming returns both message and usage
(test streaming-returns-message-and-usage
  "complete-openai-streaming returns (values message usage-plist)"
  (let* ((mock-client (make-instance 'sibyl.llm::openai-client
                                     :api-key "mock-key"
                                     :model "gpt-4o-mini"
                                     :max-tokens 1000
                                     :base-url "http://mock-openai"))
         (messages (list (sibyl.llm:user-message "test")))
         (captured-body nil)
         (sibyl.llm::*streaming-text-callback* (lambda (text) (declare (ignore text)))))
    ;; Mock http-post-stream to capture request body and simulate SSE response
    (flet ((mock-http-post-stream (url headers body on-event on-complete)
             (declare (ignore url headers on-complete))
             ;; Capture the request body
             (setf captured-body body)
             ;; Simulate SSE events with usage in final chunk
             (funcall on-event "message"
                      "{\"choices\":[{\"delta\":{\"content\":\"Hello\"}}]}")
             (funcall on-event "message"
                      "{\"choices\":[{\"delta\":{\"content\":\" world\"}}]}")
             (funcall on-event "message"
                      "{\"usage\":{\"prompt_tokens\":100,\"completion_tokens\":50,\"prompt_tokens_details\":{\"cached_tokens\":80}}}")))
      ;; Temporarily override http-post-stream
      (let ((original-fn (symbol-function 'sibyl.llm::http-post-stream)))
        (unwind-protect
             (progn
               (setf (symbol-function 'sibyl.llm::http-post-stream) #'mock-http-post-stream)
               (multiple-value-bind (msg usage)
                   (sibyl.llm::complete-openai-streaming mock-client messages nil)
                 ;; Verify message content
                 (is (string= "Hello world" (sibyl.llm:message-content msg)))
                 ;; Verify usage extraction
                 (is (not (null usage)))
                 (is (= 100 (getf usage :input-tokens)))
                 (is (= 50 (getf usage :output-tokens)))
                 (is (= 80 (getf usage :cache-read-tokens)))
                 (is (= 0 (getf usage :cache-write-tokens)))
                  ;; Verify stream_options in request body
                  (is (not (null captured-body)))
                  (let ((stream-opts (gethash "stream_options" captured-body)))
                    (is (not (null stream-opts)))
                    (is (eq t (cdr (assoc "include_usage" stream-opts :test #'equal)))))))
          (setf (symbol-function 'sibyl.llm::http-post-stream) original-fn))))))

;;; Test 2: Usage chunk without cached_tokens defaults cache-read to 0
(test streaming-usage-without-cache-details
  "complete-openai-streaming handles usage without cached_tokens"
  (let* ((mock-client (make-instance 'sibyl.llm::openai-client
                                     :api-key "mock-key"
                                     :model "gpt-4o-mini"
                                     :max-tokens 1000
                                     :base-url "http://mock-openai"))
         (messages (list (sibyl.llm:user-message "test")))
         (sibyl.llm::*streaming-text-callback* (lambda (text) (declare (ignore text)))))
    (flet ((mock-http-post-stream (url headers body on-event on-complete)
             (declare (ignore url headers body on-complete))
             (funcall on-event "message"
                      "{\"choices\":[{\"delta\":{\"content\":\"Test\"}}]}")
             (funcall on-event "message"
                      "{\"usage\":{\"prompt_tokens\":200,\"completion_tokens\":100}}")))
      (let ((original-fn (symbol-function 'sibyl.llm::http-post-stream)))
        (unwind-protect
             (progn
               (setf (symbol-function 'sibyl.llm::http-post-stream) #'mock-http-post-stream)
               (multiple-value-bind (msg usage)
                   (sibyl.llm::complete-openai-streaming mock-client messages nil)
                 (is (string= "Test" (sibyl.llm:message-content msg)))
                 (is (= 200 (getf usage :input-tokens)))
                 (is (= 100 (getf usage :output-tokens)))
                 (is (= 0 (getf usage :cache-read-tokens)))
                 (is (= 0 (getf usage :cache-write-tokens)))))
          (setf (symbol-function 'sibyl.llm::http-post-stream) original-fn))))))

;;; Test 3: No usage chunk returns nil usage
(test streaming-no-usage-chunk-returns-nil
  "complete-openai-streaming returns nil usage when no usage chunk arrives"
  (let* ((mock-client (make-instance 'sibyl.llm::openai-client
                                     :api-key "mock-key"
                                     :model "gpt-4o-mini"
                                     :max-tokens 1000
                                     :base-url "http://mock-openai"))
         (messages (list (sibyl.llm:user-message "test")))
         (sibyl.llm::*streaming-text-callback* (lambda (text) (declare (ignore text)))))
    (flet ((mock-http-post-stream (url headers body on-event on-complete)
             (declare (ignore url headers body on-complete))
             ;; Only send content chunks, no usage chunk
             (funcall on-event "message"
                      "{\"choices\":[{\"delta\":{\"content\":\"Incomplete\"}}]}")))
      (let ((original-fn (symbol-function 'sibyl.llm::http-post-stream)))
        (unwind-protect
             (progn
               (setf (symbol-function 'sibyl.llm::http-post-stream) #'mock-http-post-stream)
               (multiple-value-bind (msg usage)
                   (sibyl.llm::complete-openai-streaming mock-client messages nil)
                 (is (string= "Incomplete" (sibyl.llm:message-content msg)))
                 (is (null usage))))
          (setf (symbol-function 'sibyl.llm::http-post-stream) original-fn))))))

;;; ============================================================
;;; OpenAI cache telemetry integration tests (TDD: Task 4)
;;; ============================================================

(def-suite openai-cache-telemetry-tests
  :description "Tests for OpenAI cache-read-tokens telemetry integration"
  :in sibyl-tests)

(in-suite openai-cache-telemetry-tests)

;;; Test 1: tracker-add-usage accumulates cache-read-tokens from OpenAI usage-plist
(test tracker-add-usage-accumulates-openai-cache-tokens
  "tracker-add-usage correctly accumulates :cache-read-tokens from OpenAI usage-plist."
  (let ((tracker (sibyl.llm::make-token-tracker))
        (usage '(:input-tokens 100 :output-tokens 50 :cache-read-tokens 500 :cache-write-tokens 0)))
    (sibyl.llm::tracker-add-usage tracker usage)
    (is (= 100 (sibyl.llm::token-tracker-input-tokens tracker)))
    (is (= 50 (sibyl.llm::token-tracker-output-tokens tracker)))
    (is (= 500 (sibyl.llm::token-tracker-cache-read-tokens tracker)))
    (is (= 0 (sibyl.llm::token-tracker-cache-write-tokens tracker)))
    (is (= 1 (sibyl.llm::token-tracker-request-count tracker)))))

;;; Test 2: tracker-cache-hit-rate calculates correctly with OpenAI cache-read-tokens
(test tracker-cache-hit-rate-with-openai-cache-tokens
  "tracker-cache-hit-rate returns > 0.0 when cache-read-tokens > 0."
  (let ((tracker (sibyl.llm::make-token-tracker))
        (usage '(:input-tokens 100 :output-tokens 50 :cache-read-tokens 500 :cache-write-tokens 0)))
    (sibyl.llm::tracker-add-usage tracker usage)
    (let ((hit-rate (sibyl.llm::tracker-cache-hit-rate tracker)))
      (is (> hit-rate 0.0))
      ;; Expected: 500 / (500 + 100) = 0.8333...
      (is (< (abs (- hit-rate (/ 500.0 600.0))) 1e-5)))))

;;; Test 3: Multiple OpenAI requests accumulate cache-read-tokens correctly
(test tracker-accumulates-multiple-openai-cache-requests
  "tracker-add-usage accumulates cache-read-tokens across multiple OpenAI requests."
  (let ((tracker (sibyl.llm::make-token-tracker))
        (usage1 '(:input-tokens 100 :output-tokens 50 :cache-read-tokens 200 :cache-write-tokens 0))
        (usage2 '(:input-tokens 150 :output-tokens 75 :cache-read-tokens 300 :cache-write-tokens 0)))
    (sibyl.llm::tracker-add-usage tracker usage1)
    (sibyl.llm::tracker-add-usage tracker usage2)
    (is (= 250 (sibyl.llm::token-tracker-input-tokens tracker)))
    (is (= 125 (sibyl.llm::token-tracker-output-tokens tracker)))
    (is (= 500 (sibyl.llm::token-tracker-cache-read-tokens tracker)))
    (is (= 2 (sibyl.llm::token-tracker-request-count tracker)))))

;;; Test 4: tracker-cache-hit-rate returns 0.0 when no cache-read-tokens
(test tracker-cache-hit-rate-zero-without-cache-tokens
  "tracker-cache-hit-rate returns 0.0 when cache-read-tokens is 0."
  (let ((tracker (sibyl.llm::make-token-tracker))
        (usage '(:input-tokens 100 :output-tokens 50 :cache-read-tokens 0 :cache-write-tokens 0)))
    (sibyl.llm::tracker-add-usage tracker usage)
    (is (= 0.0 (sibyl.llm::tracker-cache-hit-rate tracker)))))

;;; Test 5: parse-openai-response usage-plist integrates with tracker
(test parse-openai-response-usage-integrates-with-tracker
  "Usage-plist from parse-openai-response integrates correctly with tracker-add-usage."
  (let* ((resp (make-hash-table :test 'equal))
         (msg-block (make-hash-table :test 'equal))
         (choice (make-hash-table :test 'equal))
         (usage-block (make-hash-table :test 'equal))
         (prompt-details (make-hash-table :test 'equal))
         (tracker (sibyl.llm::make-token-tracker)))
    ;; Build OpenAI response with cached_tokens
    (setf (gethash "content" msg-block) "Cached response"
          (gethash "message" choice) msg-block
          (gethash "choices" resp) (vector choice)
          (gethash "prompt_tokens" usage-block) 100
          (gethash "completion_tokens" usage-block) 20
          (gethash "cached_tokens" prompt-details) 80
          (gethash "prompt_tokens_details" usage-block) prompt-details
          (gethash "usage" resp) usage-block)
    ;; Parse response and add usage to tracker
    (multiple-value-bind (msg usage)
        (sibyl.llm::parse-openai-response resp)
      (declare (ignore msg))
      (sibyl.llm::tracker-add-usage tracker usage)
      (is (= 100 (sibyl.llm::token-tracker-input-tokens tracker)))
      (is (= 20 (sibyl.llm::token-tracker-output-tokens tracker)))
      (is (= 80 (sibyl.llm::token-tracker-cache-read-tokens tracker)))
      (is (= 0 (sibyl.llm::token-tracker-cache-write-tokens tracker))))))

;;; Test 6: tracker-cache-hit-rate formula verification
(test tracker-cache-hit-rate-formula-verification
  "Verify cache hit rate formula: cache-read / (cache-read + input)."
  (let ((tracker (sibyl.llm::make-token-tracker)))
    ;; Scenario 1: 50% cache hit rate
    (sibyl.llm::tracker-add-usage tracker '(:input-tokens 500 :output-tokens 100 :cache-read-tokens 500 :cache-write-tokens 0))
    (is (< (abs (- (sibyl.llm::tracker-cache-hit-rate tracker) 0.5)) 1e-5))
    
    ;; Scenario 2: 80% cache hit rate
    (let ((tracker2 (sibyl.llm::make-token-tracker)))
      (sibyl.llm::tracker-add-usage tracker2 '(:input-tokens 200 :output-tokens 50 :cache-read-tokens 800 :cache-write-tokens 0))
      (is (< (abs (- (sibyl.llm::tracker-cache-hit-rate tracker2) 0.8)) 1e-5)))
    
    ;; Scenario 3: 100% cache hit rate (no regular input)
    (let ((tracker3 (sibyl.llm::make-token-tracker)))
      (sibyl.llm::tracker-add-usage tracker3 '(:input-tokens 0 :output-tokens 50 :cache-read-tokens 1000 :cache-write-tokens 0))
      (is (< (abs (- (sibyl.llm::tracker-cache-hit-rate tracker3) 1.0)) 1e-5)))))

;;; ============================================================
;;; OpenAI Cache Integration Tests (TDD: Task 5)
;;; ============================================================

(def-suite openai-cache-integration-tests
  :description "Tests for OpenAI cache integration with usage tracking"
  :in sibyl-tests)

(in-suite openai-cache-integration-tests)

;;; --- Mock OpenAI client for cache integration tests ---

(defvar *mock-openai-call-count* 0
  "Counter for mock OpenAI client calls.")

(defclass mock-openai-client (sibyl.llm:llm-client)
  ()
  (:default-initargs
   :api-key "test-openai-key"
   :model "gpt-5-mini"
   :base-url "http://localhost"
   :max-tokens 1024
   :temperature 0.0)
  (:documentation "Mock OpenAI client for cache integration tests."))

(defmethod sibyl.llm:complete ((client mock-openai-client) messages &key)
  "Mock OpenAI complete: increments counter, returns message + usage with cache tokens."
  (incf *mock-openai-call-count*)
  (values (sibyl.llm:assistant-message "Hello from OpenAI mock")
          (list :input-tokens 100
                :output-tokens 50
                :cache-read-tokens 80
                :cache-write-tokens 0)))

(defmethod sibyl.llm:complete-with-tools ((client mock-openai-client) messages tools &key)
  "Mock OpenAI complete-with-tools: increments counter, returns message + usage."
  (declare (ignore tools))
  (incf *mock-openai-call-count*)
  (values (sibyl.llm:assistant-message "Tool response from OpenAI mock")
          (list :input-tokens 150
                :output-tokens 75
                :cache-read-tokens 120
                :cache-write-tokens 0)))

;;; --- Helper to reset cache state for OpenAI tests ---

(defmacro with-clean-openai-cache-state (&body body)
  "Run BODY with fresh cache, telemetry, and mock counter."
  `(let ((sibyl.cache:*response-cache* nil)
         (sibyl.cache:*cache-enabled* t)
         (sibyl.llm:*streaming-text-callback* nil)
         (*mock-openai-call-count* 0))
     (sibyl.cache:reset-cache-telemetry)
     ,@body))

;;; Test 1: Cache stores usage-plist from OpenAI complete
(test openai-cache-stores-usage
  "Cache :around method stores usage-plist from OpenAI complete call."
  (with-clean-openai-cache-state
    (let ((client (make-instance 'mock-openai-client))
          (messages (list (sibyl.llm:user-message "Test message"))))
      ;; First call: cache MISS, should store message + usage
      (multiple-value-bind (msg1 usage1)
          (sibyl.llm:complete client messages)
        (is (string= "Hello from OpenAI mock" (sibyl.llm:message-content msg1)))
        (is (= 100 (getf usage1 :input-tokens)))
        (is (= 50 (getf usage1 :output-tokens)))
        (is (= 80 (getf usage1 :cache-read-tokens)))
        (is (= 0 (getf usage1 :cache-write-tokens)))
        (is (= 1 *mock-openai-call-count*))
        ;; Verify cache MISS was recorded
        (is (= 1 (getf (sibyl.cache:get-cache-telemetry) :client-misses)))))))

;;; Test 2: Cache HIT returns both cached message AND cached usage-plist
(test openai-cache-hit-returns-usage
  "Cache HIT returns both cached message and cached usage-plist."
  (with-clean-openai-cache-state
    (let ((client (make-instance 'mock-openai-client))
          (messages (list (sibyl.llm:user-message "Test message"))))
      ;; First call: cache MISS
      (multiple-value-bind (msg1 usage1)
          (sibyl.llm:complete client messages)
        (declare (ignore msg1))
        (is (= 100 (getf usage1 :input-tokens)))
        (is (= 1 *mock-openai-call-count*)))
      ;; Second call: cache HIT, should return cached message + usage
      (multiple-value-bind (msg2 usage2)
          (sibyl.llm:complete client messages)
        (is (string= "Hello from OpenAI mock" (sibyl.llm:message-content msg2)))
        (is (= 100 (getf usage2 :input-tokens)))
        (is (= 50 (getf usage2 :output-tokens)))
        (is (= 80 (getf usage2 :cache-read-tokens)))
        (is (= 0 (getf usage2 :cache-write-tokens)))
        ;; Mock should NOT be called again
        (is (= 1 *mock-openai-call-count*))
        ;; Verify cache HIT was recorded
        (is (= 1 (getf (sibyl.cache:get-cache-telemetry) :client-hits)))))))

;;; Test 3: Cache works with complete-with-tools
(test openai-cache-with-tools-stores-usage
  "Cache :around method stores usage-plist from OpenAI complete-with-tools."
  (with-clean-openai-cache-state
    (let ((client (make-instance 'mock-openai-client))
          (messages (list (sibyl.llm:user-message "Tool test")))
          (tools (list (list :name "test-tool" :description "Test"))))
      ;; First call: cache MISS
      (multiple-value-bind (msg1 usage1)
          (sibyl.llm:complete-with-tools client messages tools)
        (is (string= "Tool response from OpenAI mock" (sibyl.llm:message-content msg1)))
        (is (= 150 (getf usage1 :input-tokens)))
        (is (= 75 (getf usage1 :output-tokens)))
        (is (= 120 (getf usage1 :cache-read-tokens)))
        (is (= 1 *mock-openai-call-count*)))
      ;; Second call: cache HIT
      (multiple-value-bind (msg2 usage2)
          (sibyl.llm:complete-with-tools client messages tools)
        (is (string= "Tool response from OpenAI mock" (sibyl.llm:message-content msg2)))
        (is (= 150 (getf usage2 :input-tokens)))
        (is (= 75 (getf usage2 :output-tokens)))
        (is (= 120 (getf usage2 :cache-read-tokens)))
        ;; Mock should NOT be called again
        (is (= 1 *mock-openai-call-count*))
        (is (= 1 (getf (sibyl.cache:get-cache-telemetry) :client-hits)))))))

;;; Test 4: Cost estimation with cached tokens is lower than without caching
(test openai-cache-cost-estimation
  "Cost with cache-read-tokens is lower than cost without caching for gpt-5-mini."
  (with-clean-openai-cache-state
    (let ((client (make-instance 'mock-openai-client))
          (messages (list (sibyl.llm:user-message "Cost test"))))
      ;; First call: get usage with cache tokens
      (multiple-value-bind (msg usage)
          (sibyl.llm:complete client messages)
        (declare (ignore msg))
        ;; usage: input=100, output=50, cache-read=80, cache-write=0
        ;; Calculate cost WITH cache-read
        (let* ((cost-with-cache (sibyl.llm::estimate-cost-usd "gpt-5-mini"
                                                               :input-tokens (getf usage :input-tokens)
                                                               :output-tokens (getf usage :output-tokens)
                                                               :cache-read-tokens (getf usage :cache-read-tokens)))
               ;; Calculate cost WITHOUT cache (all input tokens)
               (total-input (+ (getf usage :input-tokens) (getf usage :cache-read-tokens)))
               (cost-no-cache (sibyl.llm::estimate-cost-usd "gpt-5-mini"
                                                            :input-tokens total-input
                                                            :output-tokens (getf usage :output-tokens))))
          ;; Verify cache-read cost is lower
          ;; cost-with-cache: 100*0.25 + 50*1.00 + 80*0.025 = 0.025 + 0.05 + 0.002 = 0.077 USD per 1M
          ;; cost-no-cache:   180*0.25 + 50*1.00 = 0.045 + 0.05 = 0.095 USD per 1M
          (is (< (getf cost-with-cache :total) (getf cost-no-cache :total)))
          ;; Verify cache-read component is present
          (is (> (getf cost-with-cache :cache-read) 0.0)))))))

;;; Test 5: Cache respects *cache-enabled* flag
(test openai-cache-respects-enabled-flag
  "Cache is bypassed when *cache-enabled* is NIL."
  (let ((sibyl.cache:*response-cache* nil)
        (sibyl.cache:*cache-enabled* nil)  ; Disable cache
        (sibyl.llm:*streaming-text-callback* nil)
        (*mock-openai-call-count* 0))
    (sibyl.cache:reset-cache-telemetry)
    (let ((client (make-instance 'mock-openai-client))
          (messages (list (sibyl.llm:user-message "No cache test"))))
      ;; First call
      (sibyl.llm:complete client messages)
      (is (= 1 *mock-openai-call-count*))
      ;; Second call: should call mock again (cache disabled)
      (sibyl.llm:complete client messages)
      (is (= 2 *mock-openai-call-count*))
      ;; No cache hits/misses recorded
      (is (= 0 (getf (sibyl.cache:get-cache-telemetry) :client-hits)))
      (is (= 0 (getf (sibyl.cache:get-cache-telemetry) :client-misses))))))
