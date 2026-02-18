;;;; cache-test.lisp — Tests for sibyl.cache module

(in-package #:sibyl.tests)

;;; ============================================================
;;; Suite definitions
;;; ============================================================

(def-suite cache-key-tests
  :description "Cache key generation tests (pure, safe)."
  :in sibyl-tests)

(def-suite cache-lru-tests
  :description "LRU cache store tests (thread + sleep, unsafe)."
  :in sibyl-tests)

(def-suite cache-adapter-tests
  :description "Cache adapter tests for Anthropic/OpenAI (pure, safe)."
  :in sibyl-tests)

(def-suite cache-telemetry-tests
  :description "Cache telemetry tests (thread, unsafe)."
  :in sibyl-tests)

;;; ============================================================
;;; Cache key tests
;;; ============================================================

(in-suite cache-key-tests)

(test cache-key/identical-requests-same-key
  "Identical alists produce the same SHA-256 key."
  (let ((req '(("model" . "claude-opus-4")
               ("max_tokens" . 1024)
               ("messages" . ((("role" . "user") ("content" . "Hello")))))))
    (is (string= (sibyl.cache:make-cache-key req)
                 (sibyl.cache:make-cache-key req)))))

(test cache-key/field-order-independent
  "Alist field ordering must not affect the cache key."
  (let ((a '(("model" . "claude-opus-4") ("max_tokens" . 512) ("messages" . ())))
        (b '(("max_tokens" . 512) ("model" . "claude-opus-4") ("messages" . ()))))
    (is (string= (sibyl.cache:make-cache-key a)
                 (sibyl.cache:make-cache-key b)))))

(test cache-key/different-model-different-key
  "Requests differing only in model produce different keys."
  (let ((a '(("model" . "claude-opus-4")    ("max_tokens" . 1024) ("messages" . ())))
        (b '(("model" . "claude-haiku-3-5") ("max_tokens" . 1024) ("messages" . ()))))
    (is (not (string= (sibyl.cache:make-cache-key a)
                      (sibyl.cache:make-cache-key b))))))

(test cache-key/different-param-different-key
  "Requests differing in max_tokens produce different keys."
  (let ((a '(("model" . "claude-opus-4") ("max_tokens" . 256) ("messages" . ())))
        (b '(("model" . "claude-opus-4") ("max_tokens" . 512) ("messages" . ()))))
    (is (not (string= (sibyl.cache:make-cache-key a)
                      (sibyl.cache:make-cache-key b))))))

(test cache-key/empty-messages
  "Empty messages list is handled without error."
  (let ((key (sibyl.cache:make-cache-key
              '(("model" . "claude-opus-4") ("messages" . ())))))
    (is (stringp key))
    (is (= 64 (length key)))))

(test cache-key/output-format
  "make-cache-key always returns a 64-character lowercase hex string."
  (let ((key (sibyl.cache:make-cache-key
              '(("model" . "claude-opus-4") ("max_tokens" . 1024)
                ("messages" . ((("role" . "user") ("content" . "test"))))))))
    (is (stringp key))
    (is (= 64 (length key)))
    (is (every (lambda (c) (or (digit-char-p c)
                               (member c '(#\a #\b #\c #\d #\e #\f))))
               key))))

(test cache-key/nested-alist-normalized
  "Nested alists with different field order produce the same key."
  (let ((a '(("model" . "claude-opus-4")
             ("messages" . ((("role" . "user") ("content" . "Hi"))))))
        (b '(("messages" . ((("content" . "Hi") ("role" . "user"))))
             ("model" . "claude-opus-4"))))
    (is (string= (sibyl.cache:make-cache-key a)
                 (sibyl.cache:make-cache-key b)))))

(test cache-key/hash-table-input
  "make-cache-key works with hash-table input (yason-parsed)."
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "model" ht) "claude-opus-4"
          (gethash "max_tokens" ht) 1024
          (gethash "messages" ht) nil)
    (let ((key (sibyl.cache:make-cache-key ht)))
      (is (stringp key))
      (is (= 64 (length key))))))

;;; ============================================================
;;; LRU cache tests
;;; ============================================================

(in-suite cache-lru-tests)

;;; helpers
(defun %fresh-cache (&key (max-size 4))
  (sibyl.cache:make-lru-cache :max-size max-size))

(defun %cache-put (cache key value &key (ttl 86400))
  (sibyl.cache:cache-put cache key value :ttl ttl))

(defun %cache-get (cache key)
  (sibyl.cache:cache-get cache key))

(test lru/miss-on-empty
  "cache-get on an empty cache returns (values nil nil)."
  (let ((c (%fresh-cache)))
    (multiple-value-bind (entry found) (%cache-get c "k1")
      (is (null entry))
      (is (null found)))))

(test lru/put-then-get
  "A stored entry is retrievable."
  (let ((c (%fresh-cache)))
    (%cache-put c "k1" "response-body")
    (multiple-value-bind (entry found) (%cache-get c "k1")
      (is-true found)
      (is (string= "response-body" (getf entry :response))))))

(test lru/hit-count-increments
  "hit-count increases on each successful cache-get."
  (let ((c (%fresh-cache)))
    (%cache-put c "k1" "r")
    (%cache-get c "k1")
    (%cache-get c "k1")
    (multiple-value-bind (entry found) (%cache-get c "k1")
      (is-true found)
      (is (= 3 (getf entry :hit-count))))))

(test lru/update-existing-key
  "cache-put on an existing key updates the response."
  (let ((c (%fresh-cache)))
    (%cache-put c "k1" "old")
    (%cache-put c "k1" "new")
    (multiple-value-bind (entry found) (%cache-get c "k1")
      (is-true found)
      (is (string= "new" (getf entry :response))))))

(test lru/eviction-removes-lru
  "When capacity is exceeded the least-recently-used entry is evicted."
  (let ((c (%fresh-cache :max-size 3)))
    (%cache-put c "k1" "v1")
    (%cache-put c "k2" "v2")
    (%cache-put c "k3" "v3")
    ;; access k1 -> MRU; k2 is now LRU
    (%cache-get c "k1")
    ;; insert k4 -> k2 should be evicted
    (%cache-put c "k4" "v4")
    (multiple-value-bind (_ found-k2) (%cache-get c "k2")
      (declare (ignore _))
      (is (null found-k2) "k2 should have been evicted"))
    (multiple-value-bind (_ found-k1) (%cache-get c "k1")
      (declare (ignore _))
      (is-true found-k1 "k1 should still be present"))))

(test lru/stats-reflect-size
  "cache-stats :size matches the number of stored entries."
  (let ((c (%fresh-cache :max-size 10)))
    (%cache-put c "a" "1")
    (%cache-put c "b" "2")
    (%cache-put c "c" "3")
    (let ((stats (sibyl.cache:cache-stats c)))
      (is (= 3 (getf stats :size)))
      (is (= 10 (getf stats :max-size))))))

(test lru/flush-clears-all
  "cache-flush removes all entries."
  (let ((c (%fresh-cache)))
    (%cache-put c "k1" "v1")
    (%cache-put c "k2" "v2")
    (sibyl.cache:cache-flush c)
    (is (= 0 (getf (sibyl.cache:cache-stats c) :size)))
    (multiple-value-bind (_ found) (%cache-get c "k1")
      (declare (ignore _))
      (is (null found)))))

(test lru/ttl-expired-entry-is-miss
  "An entry whose TTL has elapsed is treated as a miss."
  (let ((c (%fresh-cache)))
    ;; TTL=0 with >= means immediately expired
    (%cache-put c "k1" "v1" :ttl 0)
    (multiple-value-bind (_ found) (%cache-get c "k1")
      (declare (ignore _))
      (is (null found) "Expired entry must not be returned"))))

(test lru/evict-expired-batch
  "cache-evict-expired removes all TTL-expired entries and returns count."
  (let ((c (%fresh-cache :max-size 10)))
    (%cache-put c "exp1" "v" :ttl 0)
    (%cache-put c "exp2" "v" :ttl 0)
    (%cache-put c "live" "v" :ttl 86400)
    (let ((n (sibyl.cache:cache-evict-expired c)))
      (is (= 2 n))
      (is (= 1 (getf (sibyl.cache:cache-stats c) :size))))))

(test lru/concurrent-put-get
  "Concurrent puts and gets do not corrupt the cache."
  (let ((c (%fresh-cache :max-size 64))
        (threads '()))
    (dotimes (t-id 8)
      (push (bt:make-thread
             (lambda ()
               (dotimes (i 8)
                 (let ((k (format nil "t~d-k~d" t-id i)))
                   (%cache-put c k (format nil "v~d" i))
                   (%cache-get c k))))
             :name (format nil "writer-~d" t-id))
            threads))
    (mapc #'bt:join-thread threads)
    (let ((stats (sibyl.cache:cache-stats c)))
      (is (<= (getf stats :size) 64)))))

;;; ============================================================
;;; Anthropic adapter tests
;;; ============================================================

(in-suite cache-adapter-tests)

;;; fixtures

(defparameter +anthropic-request+
  '(("model"      . "claude-opus-4")
    ("max_tokens" . 1024)
    ("messages"   . ((("role" . "user") ("content" . "Hello")))))
  "Minimal valid Anthropic request alist.")

(defparameter +anthropic-response-json+
  "{\"id\":\"msg_01XFDUDYJgAACzvnptvVoYEL\",
    \"type\":\"message\",
    \"role\":\"assistant\",
    \"content\":[{\"type\":\"text\",\"text\":\"Hi there!\"}],
    \"model\":\"claude-opus-4\",
    \"stop_reason\":\"end_turn\",
    \"usage\":{\"input_tokens\":10,\"output_tokens\":5,
               \"cache_read_input_tokens\":0,
               \"cache_creation_input_tokens\":0}}"
  "Minimal valid Anthropic response JSON string.")

(defparameter +anthropic-response-with-cache-tokens+
  "{\"id\":\"msg_abc\",\"type\":\"message\",\"role\":\"assistant\",
    \"content\":[],\"model\":\"claude-opus-4\",\"stop_reason\":\"end_turn\",
    \"usage\":{\"input_tokens\":100,\"output_tokens\":20,
               \"cache_read_input_tokens\":80,
               \"cache_creation_input_tokens\":40}}"
  "Response with non-zero server-side cache token counts.")

;;; normalize-request

(test anthropic/normalize-strips-metadata
  "normalize-request removes metadata and user fields."
  (let* ((req (append '(("metadata" . (("user_id" . "u123")))
                        ("user" . "alice"))
                      +anthropic-request+))
         (norm (sibyl.cache:anthropic-normalize-request req)))
    (is (null (assoc "metadata" norm :test #'string=))
        "metadata must be stripped")
    (is (null (assoc "user" norm :test #'string=))
        "user must be stripped")))

(test anthropic/normalize-preserves-core-fields
  "normalize-request keeps model, max_tokens, messages."
  (let ((norm (sibyl.cache:anthropic-normalize-request +anthropic-request+)))
    (is (string= "claude-opus-4" (cdr (assoc "model" norm :test #'string=))))
    (is (= 1024 (cdr (assoc "max_tokens" norm :test #'string=))))
    (is (not (null (cdr (assoc "messages" norm :test #'string=)))))))

(test anthropic/normalize-idempotent
  "Normalizing twice yields the same result."
  (let* ((norm1 (sibyl.cache:anthropic-normalize-request +anthropic-request+))
         (norm2 (sibyl.cache:anthropic-normalize-request norm1)))
    (is (equal norm1 norm2))))

;;; wrap-response

(test anthropic/wrap-response-fresh-id
  "wrap-response replaces the original message id with a fresh one."
  (let* ((wrapped (sibyl.cache:anthropic-wrap-response +anthropic-response-json+))
         (parsed  (yason:parse wrapped :object-as :hash-table)))
    (is (stringp (gethash "id" parsed)))
    (is (not (string= "msg_01XFDUDYJgAACzvnptvVoYEL" (gethash "id" parsed)))
        "Wrapped response must have a different id")))

(test anthropic/wrap-response-preserves-content
  "wrap-response keeps the original content and usage fields intact."
  (let* ((wrapped (sibyl.cache:anthropic-wrap-response +anthropic-response-json+))
         (parsed  (yason:parse wrapped :object-as :hash-table)))
    (is (string= "message"   (gethash "type" parsed)))
    (is (string= "assistant" (gethash "role" parsed)))
    (is (not (null (gethash "content" parsed))))
    (is (not (null (gethash "usage" parsed))))))

;;; no-cache-p

(test anthropic/no-cache-text-only-request
  "A plain text request with no special headers is cacheable."
  (is (null (sibyl.cache:anthropic-no-cache-p +anthropic-request+ '()))))

(test anthropic/no-cache-cache-control-no-store
  "Cache-Control: no-store header forces bypass."
  (is (sibyl.cache:anthropic-no-cache-p
       +anthropic-request+
       '(("cache-control" . "no-store")))))

(test anthropic/no-cache-x-no-cache-header
  "x-no-cache header forces bypass."
  (is (sibyl.cache:anthropic-no-cache-p
       +anthropic-request+
       '(("x-no-cache" . "1")))))

(test anthropic/no-cache-bash-tool
  "A request containing a bash tool must bypass the cache."
  (let ((req (append '(("tools" . ((("type" . "bash") ("name" . "bash")))))
                     +anthropic-request+)))
    (is (sibyl.cache:anthropic-no-cache-p req '()))))

(test anthropic/no-cache-computer-tool
  "A request containing a computer tool must bypass the cache."
  (let ((req (append '(("tools" . ((("type" . "computer") ("name" . "computer")))))
                     +anthropic-request+)))
    (is (sibyl.cache:anthropic-no-cache-p req '()))))

(test anthropic/no-cache-safe-custom-tool
  "A request with a non-side-effectful custom tool is cacheable."
  (let ((req (append '(("tools" . ((("type" . "custom") ("name" . "lookup")))))
                     +anthropic-request+)))
    (is (null (sibyl.cache:anthropic-no-cache-p req '())))))

;;; extract-server-cache-tokens

(test anthropic/extract-cache-tokens-zero
  "extract-server-cache-tokens returns (0 0) when no cache tokens present."
  (let ((parsed (yason:parse +anthropic-response-json+ :object-as :hash-table)))
    (multiple-value-bind (read created)
        (sibyl.cache:anthropic-extract-server-cache-tokens parsed)
      (is (= 0 read))
      (is (= 0 created)))))

(test anthropic/extract-cache-tokens-nonzero
  "extract-server-cache-tokens reads cache_read and cache_creation token counts."
  (let ((parsed (yason:parse +anthropic-response-with-cache-tokens+ :object-as :hash-table)))
    (multiple-value-bind (read created)
        (sibyl.cache:anthropic-extract-server-cache-tokens parsed)
      (is (= 80 read))
      (is (= 40 created)))))

;;; ============================================================
;;; Telemetry tests
;;; ============================================================

(in-suite cache-telemetry-tests)

(defmacro with-clean-cache-stats (&body body)
  `(progn
     (sibyl.cache:reset-cache-telemetry)
     ,@body))

(test telemetry/initial-state
  "All counters start at zero after reset."
  (with-clean-cache-stats
    (let ((s (sibyl.cache:get-cache-telemetry)))
      (is (= 0 (getf s :total-requests)))
      (is (= 0 (getf s :client-hits)))
      (is (= 0 (getf s :client-misses)))
      (is (= 0.0 (getf s :hit-rate)))
      (is (= 0 (getf s :tokens-saved)))
      (is (= 0 (getf s :server-cache-tokens))))))

(test telemetry/record-hit-increments
  "record-cache-hit increments total-requests and client-hits."
  (with-clean-cache-stats
    (sibyl.cache:record-cache-hit)
    (sibyl.cache:record-cache-hit)
    (let ((s (sibyl.cache:get-cache-telemetry)))
      (is (= 2 (getf s :total-requests)))
      (is (= 2 (getf s :client-hits)))
      (is (= 0 (getf s :client-misses))))))

(test telemetry/record-miss-increments
  "record-cache-miss increments total-requests and client-misses."
  (with-clean-cache-stats
    (sibyl.cache:record-cache-miss)
    (let ((s (sibyl.cache:get-cache-telemetry)))
      (is (= 1 (getf s :total-requests)))
      (is (= 0 (getf s :client-hits)))
      (is (= 1 (getf s :client-misses))))))

(test telemetry/hit-rate-calculation
  "hit-rate = hits / total-requests."
  (with-clean-cache-stats
    (sibyl.cache:record-cache-hit)
    (sibyl.cache:record-cache-hit)
    (sibyl.cache:record-cache-miss)
    (let ((s (sibyl.cache:get-cache-telemetry)))
      (is (= 3 (getf s :total-requests)))
      (is (< (abs (- (/ 2.0 3.0) (getf s :hit-rate))) 1e-5)))))

(test telemetry/hit-rate-zero-when-no-requests
  "hit-rate is 0.0 when no requests have been recorded."
  (with-clean-cache-stats
    (is (= 0.0 (getf (sibyl.cache:get-cache-telemetry) :hit-rate)))))

(test telemetry/record-server-cache-tokens
  "record-server-cache-tokens accumulates across calls."
  (with-clean-cache-stats
    (sibyl.cache:record-server-cache-tokens 80)
    (sibyl.cache:record-server-cache-tokens 40)
    (is (= 120 (getf (sibyl.cache:get-cache-telemetry) :server-cache-tokens)))))

(test telemetry/reset-clears-all
  "reset-cache-telemetry zeroes every counter."
  (with-clean-cache-stats
    (sibyl.cache:record-cache-hit)
    (sibyl.cache:record-cache-miss)
    (sibyl.cache:record-server-cache-tokens 100)
    (sibyl.cache:reset-cache-telemetry)
    (let ((s (sibyl.cache:get-cache-telemetry)))
      (is (= 0 (getf s :total-requests)))
      (is (= 0 (getf s :client-hits)))
      (is (= 0 (getf s :client-misses)))
      (is (= 0 (getf s :server-cache-tokens))))))

(test telemetry/get-stats-is-snapshot
  "Modifying stats after get-cache-telemetry does not alter the returned plist."
  (with-clean-cache-stats
    (let ((snap (sibyl.cache:get-cache-telemetry)))
      (sibyl.cache:record-cache-hit)
      (is (= 0 (getf snap :total-requests))))))

(test telemetry/concurrent-record
  "Concurrent record-cache-hit calls produce a consistent total."
  (with-clean-cache-stats
    (let ((threads (loop repeat 10
                         collect (bt:make-thread
                                  (lambda ()
                                    (dotimes (_ 100)
                                      (sibyl.cache:record-cache-hit)))))))
      (mapc #'bt:join-thread threads))
    (is (= 1000 (getf (sibyl.cache:get-cache-telemetry) :total-requests)))))

;;; ============================================================
;;; Integration tests — cache intercept on complete/complete-with-tools
;;; ============================================================

(def-suite cache-integration-tests
  :description "Cache integration tests (global state, unsafe)."
  :in sibyl-tests)

(in-suite cache-integration-tests)

;;; --- Mock LLM client ---

(defvar *mock-call-count* 0
  "Counter for mock LLM API calls.")

(defclass mock-llm-client (sibyl.llm:llm-client)
  ()
  (:default-initargs
   :api-key "test-key"
   :model "mock-model"
   :base-url "http://localhost"
   :max-tokens 1024
   :temperature 0.0)
  (:documentation "Mock LLM client for cache integration tests."))

(defmethod sibyl.llm:complete ((client mock-llm-client) messages &key)
  "Mock complete: increments counter, returns a deterministic message + usage."
  (incf *mock-call-count*)
  (values (sibyl.llm:assistant-message "Hello from mock")
          (list :input-tokens 10 :output-tokens 5)))

(defmethod sibyl.llm:complete-with-tools ((client mock-llm-client) messages tools &key)
  "Mock complete-with-tools: increments counter, returns a deterministic message + usage."
  (declare (ignore tools))
  (incf *mock-call-count*)
  (values (sibyl.llm:assistant-message "Tool response from mock")
          (list :input-tokens 15 :output-tokens 8)))

;;; --- Helper to reset all cache state ---

(defmacro with-clean-integration-state (&body body)
  "Run BODY with fresh cache, telemetry, and mock counter."
  `(let ((sibyl.cache:*response-cache* nil)
         (sibyl.cache:*cache-enabled* t)
         (sibyl.llm:*streaming-text-callback* nil)
         (*mock-call-count* 0))
     (sibyl.cache:reset-cache-telemetry)
     ,@body))

;;; --- Tests ---

(test integration/complete-caches-response
  "Calling complete twice with same messages hits cache on second call."
  (with-clean-integration-state
    (let* ((client (make-instance 'mock-llm-client))
           (msgs (list (sibyl.llm:user-message "Hello"))))
      ;; First call: cache miss
      (multiple-value-bind (msg1 usage1) (sibyl.llm:complete client msgs)
        (is (string= "Hello from mock" (sibyl.llm:message-content msg1)))
        (is (= 10 (getf usage1 :input-tokens)))
        (is (= 1 *mock-call-count*)))
      ;; Second call: cache hit — no additional API call
      (multiple-value-bind (msg2 usage2) (sibyl.llm:complete client msgs)
        (is (string= "Hello from mock" (sibyl.llm:message-content msg2)))
        (is (= 10 (getf usage2 :input-tokens)))
        (is (= 1 *mock-call-count*) "Mock should only be called once")))))

(test integration/complete-with-tools-caches-response
  "Calling complete-with-tools twice with same args hits cache on second call."
  (with-clean-integration-state
    (let* ((client (make-instance 'mock-llm-client))
           (msgs   (list (sibyl.llm:user-message "Use the tool")))
           (tools  '((("name" . "my-tool")
                      ("description" . "A test tool")
                      ("input_schema" . (("type" . "object")))))))
      ;; First call: miss
      (sibyl.llm:complete-with-tools client msgs tools)
      (is (= 1 *mock-call-count*))
      ;; Second call: hit
      (sibyl.llm:complete-with-tools client msgs tools)
      (is (= 1 *mock-call-count*) "Mock should only be called once"))))

(test integration/different-messages-different-cache-entries
  "Different messages produce different cache keys, both hitting the API."
  (with-clean-integration-state
    (let* ((client (make-instance 'mock-llm-client))
           (msgs-a (list (sibyl.llm:user-message "Hello")))
           (msgs-b (list (sibyl.llm:user-message "Goodbye"))))
      (sibyl.llm:complete client msgs-a)
      (sibyl.llm:complete client msgs-b)
      (is (= 2 *mock-call-count*) "Different messages should each call the API"))))

(test integration/streaming-bypasses-cache
  "When *streaming-text-callback* is bound, cache is bypassed."
  (with-clean-integration-state
    (let* ((client (make-instance 'mock-llm-client))
           (msgs   (list (sibyl.llm:user-message "Hello")))
           (sibyl.llm:*streaming-text-callback* (lambda (text) (declare (ignore text)))))
      ;; Call twice with streaming active
      (sibyl.llm:complete client msgs)
      (sibyl.llm:complete client msgs)
      (is (= 2 *mock-call-count*) "Streaming should bypass cache"))))

(test integration/cache-disabled-bypasses-cache
  "When *cache-enabled* is NIL, cache is bypassed."
  (with-clean-integration-state
    (let* ((client (make-instance 'mock-llm-client))
           (msgs   (list (sibyl.llm:user-message "Hello")))
           (sibyl.cache:*cache-enabled* nil))
      (sibyl.llm:complete client msgs)
      (sibyl.llm:complete client msgs)
      (is (= 2 *mock-call-count*) "Disabled cache should not intercept"))))

(test integration/telemetry-tracks-hits-and-misses
  "Telemetry correctly records cache hits and misses."
  (with-clean-integration-state
    (let* ((client (make-instance 'mock-llm-client))
           (msgs   (list (sibyl.llm:user-message "Hello"))))
      ;; First call: miss
      (sibyl.llm:complete client msgs)
      ;; Second call: hit
      (sibyl.llm:complete client msgs)
      ;; Third call with different message: miss
      (sibyl.llm:complete client (list (sibyl.llm:user-message "World")))
      (let ((stats (sibyl.cache:get-cache-telemetry)))
        (is (= 3 (getf stats :total-requests)))
        (is (= 1 (getf stats :client-hits)))
        (is (= 2 (getf stats :client-misses)))))))

(test integration/flush-invalidates-cache
  "Flushing the cache causes subsequent calls to miss."
  (with-clean-integration-state
    (let* ((client (make-instance 'mock-llm-client))
           (msgs   (list (sibyl.llm:user-message "Hello"))))
      ;; Populate
      (sibyl.llm:complete client msgs)
      (is (= 1 *mock-call-count*))
      ;; Flush
      (sibyl.cache:flush-response-cache)
      ;; Should miss now
      (sibyl.llm:complete client msgs)
      (is (= 2 *mock-call-count*) "After flush, API should be called again"))))

(test integration/different-temperature-different-cache
  "Different client temperatures produce different cache keys."
  (with-clean-integration-state
    (let* ((client-a (make-instance 'mock-llm-client :temperature 0.0))
           (client-b (make-instance 'mock-llm-client :temperature 0.7))
           (msgs (list (sibyl.llm:user-message "Hello"))))
      (sibyl.llm:complete client-a msgs)
      (sibyl.llm:complete client-b msgs)
      (is (= 2 *mock-call-count*) "Different temperatures should produce different cache entries"))))

(test integration/cache-stats-reflect-entries
  "response-cache-stats reflects the number of cached entries."
  (with-clean-integration-state
    (let* ((client (make-instance 'mock-llm-client))
           (msgs-a (list (sibyl.llm:user-message "Alpha")))
           (msgs-b (list (sibyl.llm:user-message "Beta"))))
      (sibyl.llm:complete client msgs-a)
      (sibyl.llm:complete client msgs-b)
      (let ((stats (sibyl.cache:response-cache-stats)))
        (is (= 2 (getf stats :size)))))))
