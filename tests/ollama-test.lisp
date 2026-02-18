;;;; ollama-test.lisp — Tests for the Ollama LLM provider
;;;; Pure logic tests (no live network calls, no file I/O).

(in-package #:sibyl.tests)

(def-suite ollama-tests
  :description "Tests for the Ollama LLM provider (pure logic, no network)"
  :in sibyl-tests)

(in-suite ollama-tests)

;;; ============================================================
;;; Mock helpers
;;; ============================================================

(defun %make-ollama-text-response (content &key (prompt-tokens 10) (eval-tokens 5))
  "Build a mock Ollama text response hash-table."
  (let ((ht  (make-hash-table :test 'equal))
        (msg (make-hash-table :test 'equal)))
    (setf (gethash "role"    msg) "assistant")
    (setf (gethash "content" msg) content)
    (setf (gethash "message"          ht) msg)
    (setf (gethash "done"             ht) t)
    (setf (gethash "prompt_eval_count" ht) prompt-tokens)
    (setf (gethash "eval_count"        ht) eval-tokens)
    ht))

(defun %make-ollama-tool-call-response (tool-name args-ht
                                        &key (prompt-tokens 20) (eval-tokens 15))
  "Build a mock Ollama tool-call response hash-table."
  (let ((ht   (make-hash-table :test 'equal))
        (msg  (make-hash-table :test 'equal))
        (func (make-hash-table :test 'equal))
        (tc   (make-hash-table :test 'equal)))
    (setf (gethash "name"      func) tool-name)
    (setf (gethash "arguments" func) args-ht)
    (setf (gethash "function"  tc)   func)
    (setf (gethash "role"       msg) "assistant")
    (setf (gethash "content"    msg) "")
    (setf (gethash "tool_calls" msg) (vector tc))
    (setf (gethash "message"           ht) msg)
    (setf (gethash "done"              ht) t)
    (setf (gethash "prompt_eval_count" ht) prompt-tokens)
    (setf (gethash "eval_count"        ht) eval-tokens)
    ht))

;;; ============================================================
;;; Constructor tests
;;; ============================================================

(test make-ollama-client-defaults
  "make-ollama-client returns correct default model, base-url and api-key"
  (let ((c (sibyl:make-ollama-client)))
    (is (string= "glm-4.7-flash:q8_0"    (sibyl.llm::client-model    c)))
    (is (string= "http://localhost:11434" (sibyl.llm::client-base-url c)))
    (is (string= ""                       (sibyl.llm::client-api-key  c)))))

(test make-ollama-client-custom-model
  "make-ollama-client accepts a custom model name"
  (let ((c (sibyl:make-ollama-client :model "llama3:8b")))
    (is (string= "llama3:8b" (sibyl.llm::client-model c)))))

(test make-ollama-client-custom-host
  "make-ollama-client :host overrides the default base-url"
  (let ((c (sibyl:make-ollama-client :host "http://gpu-box:11434")))
    (is (string= "http://gpu-box:11434" (sibyl.llm::client-base-url c)))))

;;; ============================================================
;;; ollama-recover-tool-name tests
;;; ============================================================

(test ollama-recover-tool-name-basic
  "ollama-recover-tool-name strips 'ollama-' prefix and '-N' counter suffix"
  (is (string= "get_weather"
               (sibyl.llm::ollama-recover-tool-name "ollama-get_weather-0"))))

(test ollama-recover-tool-name-multi-digit-counter
  "ollama-recover-tool-name handles multi-digit counter suffix"
  (is (string= "func"
               (sibyl.llm::ollama-recover-tool-name "ollama-func-42"))))

(test ollama-recover-tool-name-no-prefix
  "ollama-recover-tool-name returns the ID unchanged when there is no 'ollama-' prefix"
  (is (string= "some_tool"
               (sibyl.llm::ollama-recover-tool-name "some_tool"))))

(test ollama-recover-tool-name-underscores
  "ollama-recover-tool-name handles underscores in the tool name itself"
  (is (string= "search_the_web"
               (sibyl.llm::ollama-recover-tool-name "ollama-search_the_web-3"))))

;;; ============================================================
;;; messages-to-ollama-format tests
;;; ============================================================

(test messages-to-ollama-format-user-message
  "messages-to-ollama-format converts a user message to role/content alist"
  (let* ((result (sibyl.llm::messages-to-ollama-format
                  (list (sibyl.llm::user-message "Hello"))))
         (m (first result)))
    (is (= 1 (length result)))
    (is (string= "user"  (cdr (assoc "role"    m :test #'string=))))
    (is (string= "Hello" (cdr (assoc "content" m :test #'string=))))))

(test messages-to-ollama-format-content-blocks
  "messages-to-ollama-format normalizes content blocks to a string"
  (let* ((blocks (list '(("type" . "text") ("text" . "Hello "))
                       '(("type" . "text") ("text" . "world"))))
         (result (sibyl.llm::messages-to-ollama-format
                  (list (sibyl.llm::system-message blocks))))
         (m (first result)))
    (is (= 1 (length result)))
    (is (string= "system" (cdr (assoc "role"    m :test #'string=))))
    (is (string= "Hello world" (cdr (assoc "content" m :test #'string=))))))

(test messages-to-ollama-format-assistant-plain
  "messages-to-ollama-format converts a plain assistant message"
  (let* ((result (sibyl.llm::messages-to-ollama-format
                  (list (sibyl.llm::assistant-message "Hi there"))))
         (m (first result)))
    (is (string= "assistant" (cdr (assoc "role"    m :test #'string=))))
    (is (string= "Hi there"  (cdr (assoc "content" m :test #'string=))))))

(test messages-to-ollama-format-assistant-with-tool-calls
  "messages-to-ollama-format emits tool_calls and empty content for assistant tool use"
  (let* ((tc (sibyl.llm::make-tool-call
              :id "ollama-get_weather-0"
              :name "get_weather"
              :arguments '(("location" . "Tokyo"))))
         (result (sibyl.llm::messages-to-ollama-format
                  (list (sibyl.llm::assistant-message nil :tool-calls (list tc)))))
         (m (first result)))
    (is (string= "assistant" (cdr (assoc "role"    m :test #'string=))))
    (is (string= ""          (cdr (assoc "content" m :test #'string=))))
    ;; tool_calls key must be present and non-empty
    (let ((tcs (cdr (assoc "tool_calls" m :test #'string=))))
      (is (not (null tcs)))
      (is (= 1 (length tcs))))))

(test messages-to-ollama-format-tool-result
  "messages-to-ollama-format converts a tool-result message, recovering the tool name"
  (let* ((result (sibyl.llm::messages-to-ollama-format
                  (list (sibyl.llm::tool-result-message
                         "ollama-get_weather-0" "Sunny, 25°C"))))
         (m (first result)))
    (is (string= "tool"        (cdr (assoc "role"      m :test #'string=))))
    (is (string= "Sunny, 25°C" (cdr (assoc "content"   m :test #'string=))))
    (is (string= "get_weather" (cdr (assoc "tool_name" m :test #'string=))))))

;;; ============================================================
;;; tools-to-ollama-format tests
;;; ============================================================

(test tools-to-ollama-format-basic
  "tools-to-ollama-format wraps tool specs in Ollama function-call format"
  (let* ((tools  (list (list :name "get_weather"
                             :description "Get weather for a location"
                             :parameters  '(("type" . "object")))))
         (result (sibyl.llm::tools-to-ollama-format tools))
         (first-tool (first result)))
    (is (= 1 (length result)))
    (is (string= "function" (cdr (assoc "type" first-tool :test #'string=))))
    (let ((func (cdr (assoc "function" first-tool :test #'string=))))
      (is (not (null func)))
      (is (string= "get_weather"
                   (cdr (assoc "name" func :test #'string=))))
      (is (string= "Get weather for a location"
                   (cdr (assoc "description" func :test #'string=)))))))

;;; ============================================================
;;; parse-ollama-tool-calls tests
;;; ============================================================

(test parse-ollama-tool-calls-basic
  "parse-ollama-tool-calls produces tool-call structs with synthetic IDs"
  (let* ((args-ht (let ((ht (make-hash-table :test 'equal)))
                    (setf (gethash "location" ht) "Tokyo")
                    ht))
         (func-ht (let ((ht (make-hash-table :test 'equal)))
                    (setf (gethash "name"      ht) "get_weather")
                    (setf (gethash "arguments" ht) args-ht)
                    ht))
         (tc-ht   (let ((ht (make-hash-table :test 'equal)))
                    (setf (gethash "function" ht) func-ht)
                    ht))
         (result  (sibyl.llm::parse-ollama-tool-calls (vector tc-ht))))
    (is (= 1 (length result)))
    (let ((parsed-tc (first result)))
      (is (string= "get_weather"       (sibyl.llm::tool-call-name parsed-tc)))
      (is (string= "ollama-get_weather-0" (sibyl.llm::tool-call-id  parsed-tc)))
      (let ((args-alist (sibyl.llm::tool-call-arguments parsed-tc)))
        (is (equal "Tokyo"
                   (cdr (assoc "location" args-alist :test #'string=))))))))

;;; ============================================================
;;; parse-ollama-response tests
;;; ============================================================

(test parse-ollama-response-text
  "parse-ollama-response returns assistant message and usage plist for text reply"
  (multiple-value-bind (msg usage)
      (sibyl.llm::parse-ollama-response
       (%make-ollama-text-response "Hello world" :prompt-tokens 10 :eval-tokens 5))
    (is (eq :assistant  (sibyl.llm::message-role msg)))
    (is (string= "Hello world" (sibyl.llm::message-content msg)))
    (is (null (sibyl.llm::message-tool-calls msg)))
    (is (= 10 (getf usage :input-tokens)))
    (is (= 5  (getf usage :output-tokens)))))

(test parse-ollama-response-empty-content-becomes-nil
  "parse-ollama-response coerces empty content string to nil"
  (multiple-value-bind (msg _)
      (sibyl.llm::parse-ollama-response
       (%make-ollama-text-response ""))
    (declare (ignore _))
    (is (null (sibyl.llm::message-content msg)))))

(test parse-ollama-response-tool-calls
  "parse-ollama-response returns tool-calls and usage plist for tool-use reply"
  (let* ((args-ht (let ((ht (make-hash-table :test 'equal)))
                    (setf (gethash "location" ht) "Tokyo")
                    ht)))
    (multiple-value-bind (msg usage)
        (sibyl.llm::parse-ollama-response
         (%make-ollama-tool-call-response "get_weather" args-ht
                                          :prompt-tokens 20 :eval-tokens 15))
      (is (eq :assistant (sibyl.llm::message-role msg)))
      (is (not (null (sibyl.llm::message-tool-calls msg))))
      (let ((tc (first (sibyl.llm::message-tool-calls msg))))
        (is (string= "get_weather"          (sibyl.llm::tool-call-name tc)))
        (is (string= "ollama-get_weather-0" (sibyl.llm::tool-call-id  tc))))
      (is (= 20 (getf usage :input-tokens)))
      (is (= 15 (getf usage :output-tokens))))))

;;; ============================================================
;;; parse-ndjson-stream tests
;;; ============================================================

(test parse-ndjson-stream-chunk-count
  "parse-ndjson-stream invokes on-chunk once per NDJSON line"
  (let* ((ndjson (concatenate 'string
                   "{\"message\":{\"role\":\"assistant\",\"content\":\"Hello\"},\"done\":false}"
                   (string #\Newline)
                   "{\"message\":{\"role\":\"assistant\",\"content\":\" world\"},\"done\":false}"
                   (string #\Newline)
                   "{\"message\":{\"role\":\"assistant\",\"content\":\"\"},\"done\":true,"
                   "\"prompt_eval_count\":10,\"eval_count\":5}"
                   (string #\Newline)))
         (stream (make-string-input-stream ndjson))
         (count  0))
    (sibyl.llm::parse-ndjson-stream
     stream
     (lambda (chunk) (declare (ignore chunk)) (incf count))
     (lambda () nil))
    (is (= 3 count))))

(test parse-ndjson-stream-content-extraction
  "parse-ndjson-stream delivers the content of each chunk in order"
  (let* ((ndjson (concatenate 'string
                   "{\"message\":{\"role\":\"assistant\",\"content\":\"Hello\"},\"done\":false}"
                   (string #\Newline)
                   "{\"message\":{\"role\":\"assistant\",\"content\":\" world\"},\"done\":true,"
                   "\"prompt_eval_count\":5,\"eval_count\":3}"
                   (string #\Newline)))
         (stream   (make-string-input-stream ndjson))
         (contents '()))
    (sibyl.llm::parse-ndjson-stream
     stream
     (lambda (chunk)
       (let* ((msg  (gethash "message" chunk))
              (text (and msg (gethash "content" msg))))
         (when (and text (not (string= text "")))
           (push text contents))))
     (lambda () nil))
    (is (= 2 (length contents)))
    (is (member "Hello"  contents :test #'string=))
    (is (member " world" contents :test #'string=))))

;;; ============================================================
;;; Tool-call ID round-trip test
;;; ============================================================

(test tool-call-id-round-trip
  "ID survives parse-ollama-response → tool-result-message → messages-to-ollama-format"
  (let* ((args-ht (let ((ht (make-hash-table :test 'equal)))
                    (setf (gethash "location" ht) "Tokyo")
                    ht))
         (response (%make-ollama-tool-call-response "get_weather" args-ht)))
    ;; Step 1: parse
    (multiple-value-bind (msg _)
        (sibyl.llm::parse-ollama-response response)
      (declare (ignore _))
      ;; Step 2: extract tool-call ID
      (let* ((tc      (first (sibyl.llm::message-tool-calls msg)))
             (tc-id   (sibyl.llm::tool-call-id tc))
             ;; Step 3: create tool result message with that ID
             (tr-msg  (sibyl.llm::tool-result-message tc-id "Sunny, 25°C"))
             ;; Step 4: convert back to Ollama format
             (fmtd    (sibyl.llm::messages-to-ollama-format (list tr-msg)))
             (fmt-msg (first fmtd)))
        ;; Step 5: verify tool_name and content survive the round-trip
        (is (string= "get_weather"
                     (cdr (assoc "tool_name" fmt-msg :test #'string=))))
        (is (string= "Sunny, 25°C"
                     (cdr (assoc "content"   fmt-msg :test #'string=))))))))

;;; ============================================================
;;; URL construction (proxy for connection-error test)
;;; ============================================================

(test ollama-api-url-construction
  "ollama-api-url concatenates client base-url and path correctly"
  (let ((c (sibyl:make-ollama-client :host "http://localhost:11434")))
    (is (string= "http://localhost:11434/api/chat"
                 (sibyl.llm::ollama-api-url c "/api/chat")))
    (is (string= "http://localhost:11434/api/tags"
                 (sibyl.llm::ollama-api-url c "/api/tags")))))

(test ollama-api-url-custom-host
  "ollama-api-url uses the custom host when client is constructed with :host"
  (let ((c (sibyl:make-ollama-client :host "http://gpu-box:11434")))
    (is (string= "http://gpu-box:11434/api/chat"
                 (sibyl.llm::ollama-api-url c "/api/chat")))))

;;; ============================================================
;;; count-tokens test
;;; ============================================================

(test count-tokens-ollama-ceiling-division
  "count-tokens for Ollama client returns ceiling(length/4)"
  (let ((c (sibyl:make-ollama-client)))
    ;; "" → 0 tokens
    (is (= 0 (sibyl.llm::count-tokens c "")))
    ;; "Hello" (5 chars) → ceil(5/4) = 2
    (is (= 2 (sibyl.llm::count-tokens c "Hello")))
    ;; 16 chars → ceil(16/4) = 4 (exact)
    (is (= 4 (sibyl.llm::count-tokens c "1234567890123456")))
    ;; 17 chars → ceil(17/4) = 5
    (is (= 5 (sibyl.llm::count-tokens c "12345678901234567")))))

;;; ============================================================
;;; Model profile tests
;;; ============================================================

(test lookup-model-profile-qwen3-coder
  "lookup-model-profile returns qwen3-coder profile for 'qwen3-coder:30b'"
  (let ((profile (sibyl.llm::lookup-model-profile "qwen3-coder:30b")))
    (is (not (null profile)))
    (is (= 0.6 (getf profile :temperature)))
    (is (eq t (getf profile :thinking)))))

(test lookup-model-profile-glm4
  "lookup-model-profile returns glm-4 profile for 'glm-4.7-flash:q8_0'"
  (let ((profile (sibyl.llm::lookup-model-profile "glm-4.7-flash:q8_0")))
    (is (not (null profile)))
    (is (= 0.1 (getf profile :temperature)))
    (is (null (getf profile :thinking)))))

(test lookup-model-profile-deepseek-r1
  "lookup-model-profile matches deepseek-r1 prefix"
  (let ((profile (sibyl.llm::lookup-model-profile "deepseek-r1:70b")))
    (is (not (null profile)))
    (is (eq t (getf profile :thinking)))))

(test lookup-model-profile-unknown-falls-back-to-default
  "lookup-model-profile returns *default* for unknown model"
  (let ((profile (sibyl.llm::lookup-model-profile "some-unknown-model:latest")))
    (is (not (null profile)))
    (is (= 0.0 (getf profile :temperature)))
    (is (null (getf profile :thinking)))))

;;; ============================================================
;;; Thinking block extraction tests
;;; ============================================================

(test extract-thinking-blocks-no-thinking
  "extract-thinking-blocks passes through text without think tags"
  (multiple-value-bind (clean thinking)
      (sibyl.llm::extract-thinking-blocks "Hello world")
    (is (string= "Hello world" clean))
    (is (null thinking))))

(test extract-thinking-blocks-single-block
  "extract-thinking-blocks separates a single thinking block"
  (multiple-value-bind (clean thinking)
      (sibyl.llm::extract-thinking-blocks
       "<think>Let me analyze this</think>The answer is 42.")
    (is (string= "The answer is 42." clean))
    (is (string= "Let me analyze this" thinking))))

(test extract-thinking-blocks-multiple-blocks
  "extract-thinking-blocks handles multiple thinking blocks"
  (multiple-value-bind (clean thinking)
      (sibyl.llm::extract-thinking-blocks
       "<think>step 1</think>First part.<think>step 2</think>Second part.")
    (is (string= "First part.Second part." clean))
    (is (string= "step 1step 2" thinking))))

(test extract-thinking-blocks-empty-content
  "extract-thinking-blocks handles empty/nil content"
  (multiple-value-bind (clean thinking)
      (sibyl.llm::extract-thinking-blocks "")
    (is (string= "" clean))
    (is (null thinking)))
  (multiple-value-bind (clean thinking)
      (sibyl.llm::extract-thinking-blocks nil)
    (is (null clean))
    (is (null thinking))))

(test extract-thinking-blocks-unclosed-tag
  "extract-thinking-blocks handles unclosed think tag gracefully"
  (multiple-value-bind (clean thinking)
      (sibyl.llm::extract-thinking-blocks
       "Prefix <think>reasoning without closing tag")
    (is (string= "Prefix" clean))
    (is (string= "reasoning without closing tag" thinking))))

;;; ============================================================
;;; Profile-aware client construction tests
;;; ============================================================

(test make-ollama-client-profile-temperature
  "make-ollama-client uses profile temperature when not explicitly provided"
  (let ((c (sibyl:make-ollama-client :model "qwen3-coder:30b")))
    (is (= 0.6 (sibyl.llm::client-temperature c)))))

(test make-ollama-client-explicit-temperature-overrides
  "make-ollama-client :temperature overrides profile default"
  (let ((c (sibyl:make-ollama-client :model "qwen3-coder:30b" :temperature 0.1)))
    (is (= 0.1 (sibyl.llm::client-temperature c)))))

(test make-ollama-client-glm-profile
  "make-ollama-client uses glm-4 profile defaults"
  (let ((c (sibyl:make-ollama-client :model "glm-4.7-flash:q8_0")))
    (is (= 0.1 (sibyl.llm::client-temperature c)))))

;;; ============================================================
;;; Parse response with thinking blocks
;;; ============================================================

(test parse-ollama-response-with-thinking
  "parse-ollama-response extracts thinking blocks into message-thinking"
  (multiple-value-bind (msg usage)
      (sibyl.llm::parse-ollama-response
       (%make-ollama-text-response
        "<think>Let me reason about this</think>The answer is 42."))
    (declare (ignore usage))
    (is (string= "The answer is 42." (sibyl.llm::message-content msg)))
    (is (string= "Let me reason about this" (sibyl.llm::message-thinking msg)))))

(test parse-ollama-response-without-thinking
  "parse-ollama-response returns nil thinking when no think blocks"
  (multiple-value-bind (msg usage)
      (sibyl.llm::parse-ollama-response
       (%make-ollama-text-response "Just a normal response"))
    (declare (ignore usage))
    (is (string= "Just a normal response" (sibyl.llm::message-content msg)))
    (is (null (sibyl.llm::message-thinking msg)))))

;;; ============================================================
;;; Native thinking field tests (Ollama think API)
;;; ============================================================

(test parse-ollama-response-native-thinking
  "parse-ollama-response extracts native thinking from message.thinking field"
  (let ((ht  (make-hash-table :test 'equal))
        (msg (make-hash-table :test 'equal)))
    (setf (gethash "role"     msg) "assistant")
    (setf (gethash "content"  msg) "The answer is 42.")
    (setf (gethash "thinking" msg) "Let me reason about this carefully")
    (setf (gethash "message"          ht) msg)
    (setf (gethash "done"             ht) t)
    (setf (gethash "prompt_eval_count" ht) 10)
    (setf (gethash "eval_count"        ht) 5)
    (multiple-value-bind (parsed-msg usage)
        (sibyl.llm::parse-ollama-response ht)
      (declare (ignore usage))
      (is (string= "The answer is 42." (sibyl.llm::message-content parsed-msg)))
      (is (string= "Let me reason about this carefully"
                    (sibyl.llm::message-thinking parsed-msg))))))

(test parse-ollama-response-native-thinking-takes-precedence
  "Native thinking field takes precedence over inline <think> tags"
  (let ((ht  (make-hash-table :test 'equal))
        (msg (make-hash-table :test 'equal)))
    (setf (gethash "role"     msg) "assistant")
    ;; Content has inline tags but native thinking field is also present
    (setf (gethash "content"  msg) "<think>inline reasoning</think>The answer.")
    (setf (gethash "thinking" msg) "Native API reasoning")
    (setf (gethash "message"          ht) msg)
    (setf (gethash "done"             ht) t)
    (setf (gethash "prompt_eval_count" ht) 10)
    (setf (gethash "eval_count"        ht) 5)
    (multiple-value-bind (parsed-msg usage)
        (sibyl.llm::parse-ollama-response ht)
      (declare (ignore usage))
      ;; Native thinking wins; content is passed through as-is (including tags)
      (is (string= "<think>inline reasoning</think>The answer."
                    (sibyl.llm::message-content parsed-msg)))
      (is (string= "Native API reasoning"
                    (sibyl.llm::message-thinking parsed-msg))))))

(test parse-ollama-response-empty-native-thinking-falls-back
  "Empty native thinking field falls back to inline <think> extraction"
  (let ((ht  (make-hash-table :test 'equal))
        (msg (make-hash-table :test 'equal)))
    (setf (gethash "role"     msg) "assistant")
    (setf (gethash "content"  msg) "<think>inline reasoning</think>The answer.")
    (setf (gethash "thinking" msg) "")   ; empty native thinking
    (setf (gethash "message"          ht) msg)
    (setf (gethash "done"             ht) t)
    (setf (gethash "prompt_eval_count" ht) 10)
    (setf (gethash "eval_count"        ht) 5)
    (multiple-value-bind (parsed-msg usage)
        (sibyl.llm::parse-ollama-response ht)
      (declare (ignore usage))
      ;; Falls back to inline extraction
      (is (string= "The answer." (sibyl.llm::message-content parsed-msg)))
      (is (string= "inline reasoning"
                    (sibyl.llm::message-thinking parsed-msg))))))

;;; ============================================================
;;; gpt-oss model profile tests
;;; ============================================================

(test lookup-model-profile-gpt-oss
  "lookup-model-profile returns gpt-oss profile for 'gpt-oss:120b'"
  (let ((profile (sibyl.llm::lookup-model-profile "gpt-oss:120b")))
    (is (not (null profile)))
    (is (= 1.0 (getf profile :temperature)))
    (is (= 131072 (getf profile :num-ctx)))
    (is (= 8192 (getf profile :num-predict)))
    (is (eq t (getf profile :large-model)))
    (is (= 600 (getf profile :load-timeout)))))

(test lookup-model-profile-gpt-oss-variant
  "lookup-model-profile matches gpt-oss prefix for any variant"
  (let ((profile (sibyl.llm::lookup-model-profile "gpt-oss:7b-q4")))
    (is (not (null profile)))
    (is (eq t (getf profile :large-model)))))

;;; ============================================================
;;; keep_alive model-size-aware tests
;;; ============================================================

(test keep-alive-large-model-returns-minus-one
  "Large models (gpt-oss) default to keep_alive -1 (never unload)"
  (sibyl.config:with-config ()
    ;; Clear any config override so profile default is used
    (remhash "ollama.keep-alive" sibyl.config:*config*)
    (let ((result (sibyl.llm::%ollama-keep-alive "gpt-oss:120b")))
      (is (eql -1 result)))))

(test keep-alive-small-model-returns-30m
  "Small models fall back to 30m default"
  (sibyl.config:with-config ()
    (remhash "ollama.keep-alive" sibyl.config:*config*)
    (let ((result (sibyl.llm::%ollama-keep-alive "llama3:8b")))
      (is (string= "30m" result)))))

(test keep-alive-config-overrides-profile
  "Config ollama.keep-alive overrides profile-based default"
  (sibyl.config:with-config ()
    (setf (sibyl.config:config-value "ollama.keep-alive") "1h")
    (let ((result (sibyl.llm::%ollama-keep-alive "gpt-oss:120b")))
      (is (string= "1h" result)))))

;;; ============================================================
;;; HTTP timeout calculation tests
;;; ============================================================

(test ollama-read-timeout-large-model
  "Large models use :load-timeout from profile for read timeout"
  (let* ((c (sibyl:make-ollama-client :model "gpt-oss:120b"))
         (timeout (sibyl.llm::%ollama-read-timeout c)))
    (is (= 600 timeout))))

(test ollama-read-timeout-small-model
  "Small models use nil (global default) for read timeout"
  (let* ((c (sibyl:make-ollama-client :model "llama3:8b"))
         (timeout (sibyl.llm::%ollama-read-timeout c)))
    (is (null timeout))))

;;; ============================================================
;;; load_duration monitoring tests
;;; ============================================================

(test parse-ollama-response-with-load-duration
  "parse-ollama-response extracts load_duration into usage plist"
  (let ((ht  (make-hash-table :test 'equal))
        (msg (make-hash-table :test 'equal)))
    (setf (gethash "role"    msg) "assistant")
    (setf (gethash "content" msg) "Hello")
    (setf (gethash "message"          ht) msg)
    (setf (gethash "done"             ht) t)
    (setf (gethash "prompt_eval_count" ht) 10)
    (setf (gethash "eval_count"        ht) 5)
    ;; 500ms load, 2s total, 1.5s eval (in nanoseconds)
    (setf (gethash "load_duration"     ht) 500000000)
    (setf (gethash "total_duration"    ht) 2000000000)
    (setf (gethash "eval_duration"     ht) 1500000000)
    (multiple-value-bind (parsed-msg usage)
        (sibyl.llm::parse-ollama-response ht)
      (declare (ignore parsed-msg))
      (is (= 500 (getf usage :load-duration-ms)))
      (is (= 2000 (getf usage :total-duration-ms)))
      (is (= 1500 (getf usage :eval-duration-ms))))))

(test parse-ollama-response-cold-load-detection
  "parse-ollama-response logs warning for cold loads (>1s load_duration)"
  (let ((ht  (make-hash-table :test 'equal))
        (msg (make-hash-table :test 'equal)))
    (setf (gethash "role"    msg) "assistant")
    (setf (gethash "content" msg) "Hello")
    (setf (gethash "message"          ht) msg)
    (setf (gethash "done"             ht) t)
    (setf (gethash "prompt_eval_count" ht) 10)
    (setf (gethash "eval_count"        ht) 5)
    ;; 30s cold load (in nanoseconds)
    (setf (gethash "load_duration"     ht) 30000000000)
    (setf (gethash "total_duration"    ht) 35000000000)
    ;; Should not error — just logs
    (multiple-value-bind (parsed-msg usage)
        (sibyl.llm::parse-ollama-response ht)
      (declare (ignore parsed-msg))
      (is (= 30000 (getf usage :load-duration-ms))))))

;;; ============================================================
;;; Retry logic tests
;;; ============================================================

(test call-with-retry-success-on-first-try
  "call-with-retry returns value on first success"
  (let ((call-count 0))
    (is (= 42 (sibyl.llm::call-with-retry
               (lambda ()
                 (incf call-count)
                 42)
               :max-retries 3
               :description "test")))
    (is (= 1 call-count))))

(test call-with-retry-non-retryable-error-propagates
  "call-with-retry propagates non-retryable errors immediately"
  (let ((call-count 0))
    (signals sibyl.conditions:llm-api-error
      (sibyl.llm::call-with-retry
       (lambda ()
         (incf call-count)
         (error 'sibyl.conditions:llm-api-error
                :message "Not found"
                :status-code 404))
       :max-retries 3
       :base-delay 0.01
       :description "test"))
    ;; Should only be called once (404 is not retryable)
    (is (= 1 call-count))))

(test call-with-retry-retries-on-transient-error
  "call-with-retry retries on 500 errors then succeeds"
  (let ((call-count 0))
    (is (= 99 (sibyl.llm::call-with-retry
               (lambda ()
                 (incf call-count)
                 (if (<= call-count 2)
                     (error 'sibyl.conditions:llm-api-error
                            :message "Server error"
                            :status-code 500)
                     99))
               :max-retries 3
               :base-delay 0.01
               :description "test")))
    (is (= 3 call-count))))

;;; ============================================================
;;; make-ollama-client gpt-oss defaults test
;;; ============================================================

(test make-ollama-client-gpt-oss-profile
  "make-ollama-client uses gpt-oss profile for temperature"
  (let ((c (sibyl:make-ollama-client :model "gpt-oss:120b")))
    (is (= 1.0 (sibyl.llm::client-temperature c)))
    (is (= 8192 (sibyl.llm::client-max-tokens c)))))

;;; ============================================================
;;; Model selector — Ollama provider integration
;;; ============================================================

(test create-client-for-model-ollama-provider
  "create-client-for-model creates an ollama-client for :ollama provider"
  (let* ((config (make-instance 'sibyl.llm::model-config
                                :provider :ollama
                                :model-name "gpt-oss:120b"
                                :max-tokens 8192
                                :temperature 1.0))
         (client (sibyl.llm::create-client-for-model config)))
    (is (typep client 'sibyl.llm::ollama-client))
    (is (string= "gpt-oss:120b" (sibyl.llm::client-model client)))
    (is (= 8192 (sibyl.llm::client-max-tokens client)))))

(test latest-model-tiers-include-ollama
  "latest-model-tiers heavy tier includes an Ollama gpt-oss model"
  (let* ((heavy (find "heavy" sibyl.llm::*latest-model-tiers*
                       :key #'sibyl.llm::tier-name :test #'string=))
         (models (sibyl.llm::tier-models heavy))
         (ollama-model (find :ollama models :key #'sibyl.llm::model-provider)))
    (is (not (null ollama-model)))
    (is (string= "gpt-oss:120b" (sibyl.llm::model-name ollama-model)))))

;;; ============================================================
;;; Native think API integration tests
;;; ============================================================

(test gpt-oss-profile-has-think-api
  "gpt-oss profile includes :think-api :think for native Ollama think API"
  (let ((profile (sibyl.llm::lookup-model-profile "gpt-oss:120b")))
    (is (eq :think (getf profile :think-api)))))

(test build-request-includes-think-for-gpt-oss
  "build-ollama-request adds top-level think field for gpt-oss"
  (let* ((c (sibyl:make-ollama-client :model "gpt-oss:120b"))
         (msgs (list (sibyl.llm::user-message "Hello")))
         (body (sibyl.llm::build-ollama-request c msgs)))
    ;; "think" key should be present with value T
    (let ((think-pair (assoc "think" body :test #'string=)))
      (is (not (null think-pair)))
      (is (eq t (cdr think-pair))))))

(test build-request-no-think-for-non-thinking-model
  "build-ollama-request omits think field for models without :think-api"
  (let* ((c (sibyl:make-ollama-client :model "glm-4.7-flash:q8_0"))
         (msgs (list (sibyl.llm::user-message "Hello")))
         (body (sibyl.llm::build-ollama-request c msgs)))
    ;; "think" key should NOT be present
    (let ((think-pair (assoc "think" body :test #'string=)))
      (is (null think-pair)))))
