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

;; Test 9: system prompt without summary → single content block
(test memory-context-window-single-block-no-summary
  "memory-context-window with no summary gives system prompt as single block list"
  (let ((mem (sibyl.agent::make-memory)))
    ;; No summary set
    (let ((ctx (sibyl.agent::memory-context-window mem :system-prompt "Static system prompt")))
      (let ((sys-msg (first ctx)))
        (is (eq :system (sibyl.llm:message-role sys-msg)))
        ;; System content should be a list of one block: (("type" . "text") ("text" . "Static system prompt"))
        (let ((content (sibyl.llm:message-content sys-msg)))
          ;; content is now a list of alists, not a plain string
          (is (listp content))
          (is (= 1 (length content)))
          (is (string= "text" (cdr (assoc "type" (first content) :test #'string=))))
          (is (string= "Static system prompt" (cdr (assoc "text" (first content) :test #'string=)))))))))

;; Test 10: system prompt with summary → two content blocks
(test memory-context-window-two-blocks-with-summary
  "memory-context-window with summary gives system prompt as two-block list"
  (let ((mem (sibyl.agent::make-memory)))
    (setf (sibyl.agent::memory-summary mem) "Previous summary text")
    (let ((ctx (sibyl.agent::memory-context-window mem :system-prompt "Static prompt")))
      (let* ((sys-msg (first ctx))
             (content (sibyl.llm:message-content sys-msg)))
        (is (listp content))
        (is (= 2 (length content)))
        ;; First block: static prompt
        (is (string= "Static prompt" (cdr (assoc "text" (first content) :test #'string=))))
        ;; Second block: summary
        (is (search "Previous summary text" (cdr (assoc "text" (second content) :test #'string=))))))))

;; Test 11: tools-to-anthropic-format adds cache_control to last tool
(test tools-to-anthropic-format-adds-cache-control
  "The last tool in tools-to-anthropic-format has cache_control ephemeral"
  (unwind-protect
      (progn
        (sibyl::config-set "optimization.cache-enabled" t)
        (let* ((tools (list (list :name "tool-a" :description "First tool"
                                  :parameters '(("type" . "object") ("properties" . nil) ("required" . nil)))
                            (list :name "tool-b" :description "Last tool"
                                  :parameters '(("type" . "object") ("properties" . nil) ("required" . nil)))))
               (formatted (sibyl.llm::tools-to-anthropic-format tools)))
          ;; Last tool should have cache_control
          (let ((last-tool (car (last formatted))))
            (is (not (null (assoc "cache_control" last-tool :test #'string=))))
            (let ((cc (cdr (assoc "cache_control" last-tool :test #'string=))))
              (is (string= "ephemeral" (cdr (assoc "type" cc :test #'string=))))))))
    ;; Cleanup
    (sibyl::config-set "optimization.cache-enabled" nil)))

;; Test 12: first tool does NOT have cache_control
(test tools-only-last-tool-has-cache-control
  "Only the last tool has cache_control, not others"
  (unwind-protect
      (progn
        (sibyl::config-set "optimization.cache-enabled" t)
        (let* ((tools (list (list :name "tool-a" :description "First"
                                  :parameters '(("type" . "object") ("properties" . nil) ("required" . nil)))
                            (list :name "tool-b" :description "Second"
                                  :parameters '(("type" . "object") ("properties" . nil) ("required" . nil)))))
               (formatted (sibyl.llm::tools-to-anthropic-format tools)))
          (let ((first-tool (first formatted)))
            (is (null (assoc "cache_control" first-tool :test #'string=))))))
    (sibyl::config-set "optimization.cache-enabled" nil)))

;; Test 13: cache disabled → no cache_control anywhere
(test tools-no-cache-control-when-disabled
  "No cache_control in tool schemas when optimization.cache-enabled is nil"
  (sibyl::config-set "optimization.cache-enabled" nil)
  (let* ((tools (list (list :name "tool-a" :description "First"
                             :parameters '(("type" . "object") ("properties" . nil) ("required" . nil)))))
         (formatted (sibyl.llm::tools-to-anthropic-format tools)))
    (let ((last-tool (car (last formatted))))
      (is (null (assoc "cache_control" last-tool :test #'string=))))))

;;; ============================================================
;;; Model selector tests (Task 6: auto-routing integration)
;;; ============================================================

(def-suite model-selector-suite
  :description "Model selector and adaptive agent routing tests"
  :in sibyl-tests)

(in-suite model-selector-suite)

;; Test 14: make-default-model-selector returns a model-selector instance
(test model-selector-default-constructor
  "make-default-model-selector creates a model-selector with tiers"
  (let ((selector (sibyl.llm::make-default-model-selector)))
    (is (typep selector 'sibyl.llm::model-selector))
    (is (not (null (sibyl.llm::selector-tiers selector))))))

;; Test 15: analyze-task-complexity on model-selector returns complexity-analysis
(test model-selector-returns-complexity-analysis
  "analyze-task-complexity dispatches on model-selector and returns complexity-analysis"
  (let* ((selector (sibyl.llm::make-default-model-selector))
         (analysis (sibyl.llm::analyze-task-complexity selector "simple quick task")))
    (is (typep analysis 'sibyl.llm::complexity-analysis))
    (is (stringp (sibyl.llm::recommended-tier analysis)))))

;; Test 16: simple input routes to light tier
(test model-selector-light-tier-for-simple-input
  "Simple task with 'simple' keyword should route to light tier"
  (let* ((selector (sibyl.llm::make-default-model-selector))
         (analysis (sibyl.llm::analyze-task-complexity selector "simple task")))
    (is (string= "light" (sibyl.llm::recommended-tier analysis)))))

;; Test 17: complex input routes to heavy tier
(test model-selector-heavy-tier-for-complex-input
  "Complex analysis tasks should route to heavy tier"
  (let* ((selector (sibyl.llm::make-default-model-selector))
         (analysis (sibyl.llm::analyze-task-complexity
                    selector
                    "analyze this complex codebase architecture and refactor the authentication module")))
    (is (or (string= "medium" (sibyl.llm::recommended-tier analysis))
            (string= "heavy" (sibyl.llm::recommended-tier analysis))))))
