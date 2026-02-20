;;;; logging-test.lisp — Tests for logging enhancements (context propagation + component filtering)

(in-package #:sibyl.tests)

(def-suite logging-tests
  :description "Tests for logging context propagation and component-level filtering."
  :in sibyl-tests)

(in-suite logging-tests)

;;; ============================================================
;;; Context propagation tests
;;; ============================================================

(test context-propagation-standard-format
  "with-request-context injects context into standard format log output."
  (let ((out (make-string-output-stream)))
    (let ((sibyl.logging:*log-stream* out)
          (sibyl.logging:*log-level* :debug)
          (sibyl.logging:*log-format* :standard)
          (sibyl.logging:*enable-colors* nil))
      (sibyl.logging:with-request-context
          (:request-id "test-001" :agent-name "TestAgent" :step-number 3)
        (sibyl.logging:log-debug "agent" "Step started")))
    (let ((result (get-output-stream-string out)))
      (is (search "test-001" result)
          "request-id should appear in standard output")
      (is (search "step:3" result)
          "step-number should appear in standard output")
      (is (search "Step started" result)
          "message should appear in standard output"))))

(test context-propagation-absent
  "Without context, log output has no context markers (backward compatible)."
  (let ((out (make-string-output-stream)))
    (let ((sibyl.logging:*log-stream* out)
          (sibyl.logging:*log-level* :debug)
          (sibyl.logging:*log-format* :standard)
          (sibyl.logging:*enable-colors* nil)
          (sibyl.logging:*current-request-context* nil))
      (sibyl.logging:log-debug "test" "No context message"))
    (let ((result (get-output-stream-string out)))
      (is (search "No context message" result)
          "message should appear")
      (is (null (search "req-" result))
          "no req- prefix should appear without context")
      (is (null (search "[" result :start2 (1+ (position #\] result))))
          "no extra brackets from context should appear"))))

(test context-nested-override
  "Nested with-request-context overrides specified fields, inherits others."
  (let ((out (make-string-output-stream)))
    (let ((sibyl.logging:*log-stream* out)
          (sibyl.logging:*log-level* :debug)
          (sibyl.logging:*log-format* :standard)
          (sibyl.logging:*enable-colors* nil))
      (sibyl.logging:with-request-context
          (:request-id "outer-req" :agent-name "OuterAgent" :step-number 1)
        (sibyl.logging:with-request-context
            (:step-number 5)
          (sibyl.logging:log-debug "agent" "Nested step"))))
    (let ((result (get-output-stream-string out)))
      (is (search "outer-req" result)
          "request-id inherited from outer context")
      (is (search "step:5" result)
          "step-number overridden by inner context"))))

(test context-auto-request-id
  "request-id is auto-generated when omitted."
  (let ((out (make-string-output-stream)))
    (let ((sibyl.logging:*log-stream* out)
          (sibyl.logging:*log-level* :debug)
          (sibyl.logging:*log-format* :standard)
          (sibyl.logging:*enable-colors* nil))
      (sibyl.logging:with-request-context
          (:agent-name "AutoID")
        (sibyl.logging:log-debug "agent" "Auto ID test")))
    (let ((result (get-output-stream-string out)))
      (is (search "req-" result)
          "auto-generated request-id should contain req- prefix"))))

(test context-user-input-truncation
  "user-input is truncated to 50 characters."
  (let ((long-input (make-string 100 :initial-element #\x)))
    (sibyl.logging:with-request-context
        (:user-input long-input)
      (let ((prefix (getf sibyl.logging:*current-request-context* :user-input-prefix)))
        (is (= 50 (length prefix))
            "user-input-prefix should be truncated to 50 chars")))))

;;; ============================================================
;;; Component-level log level tests
;;; ============================================================

(test component-log-level-override
  "Setting a component log level allows debug output for that component only."
  (let ((out (make-string-output-stream)))
    (let ((sibyl.logging:*log-stream* out)
          (sibyl.logging:*log-level* :info)
          (sibyl.logging:*component-log-levels* (make-hash-table :test 'equal)))
      (setf (sibyl.logging:component-log-level "llm") :debug)
      (sibyl.logging:log-debug "llm" "Should appear")
      (sibyl.logging:log-debug "mcp" "Should NOT appear"))
    (let ((result (get-output-stream-string out)))
      (is (search "Should appear" result)
          "llm debug should be visible when llm level is :debug")
      (is (null (search "Should NOT appear" result))
          "mcp debug should be filtered when global level is :info"))))

(test component-log-level-fallback
  "Unset components fall back to *log-level*."
  (let ((sibyl.logging:*component-log-levels* (make-hash-table :test 'equal)))
    (let ((sibyl.logging:*log-level* :warn))
      (is (eq :warn (sibyl.logging:component-log-level "tools"))
          "unset component should return global *log-level*"))))

(test component-log-level-reset
  "reset-component-log-levels clears all overrides."
  (let ((sibyl.logging:*component-log-levels* (make-hash-table :test 'equal)))
    (setf (sibyl.logging:component-log-level "llm") :debug)
    (setf (sibyl.logging:component-log-level "mcp") :trace)
    (sibyl.logging:reset-component-log-levels)
    (is (= 0 (hash-table-count sibyl.logging:*component-log-levels*))
        "all component overrides should be cleared")))

(test component-name-case-normalization
  "Component names are case-normalized (\"LLM\" and \"llm\" are equivalent)."
  (let ((sibyl.logging:*component-log-levels* (make-hash-table :test 'equal)))
    (setf (sibyl.logging:component-log-level "LLM") :debug)
    (is (eq :debug (sibyl.logging:component-log-level "llm"))
        "setting LLM should be readable as llm")
    (is (eq :debug (sibyl.logging:component-log-level "Llm"))
        "setting LLM should be readable as Llm")))

;;; ============================================================
;;; JSON format tests
;;; ============================================================

(test json-format-with-context
  "JSON output includes context fields as top-level keys."
  (let ((out (make-string-output-stream)))
    (let ((sibyl.logging:*log-stream* out)
          (sibyl.logging:*log-level* :debug)
          (sibyl.logging:*log-format* :json))
      (sibyl.logging:with-request-context
          (:request-id "json-test" :agent-name "JsonAgent")
        (sibyl.logging:log-debug "llm" "Test msg")))
    (let ((result (get-output-stream-string out)))
      (is (search "\"request-id\":\"json-test\"" result)
          "JSON should contain request-id field")
      (is (search "\"agent-name\":\"JsonAgent\"" result)
          "JSON should contain agent-name field")
      (is (search "\"message\":\"Test msg\"" result)
          "JSON should contain message field"))))

(test json-format-special-char-escape
  "JSON output escapes special characters in context values."
  (let ((out (make-string-output-stream)))
    (let ((sibyl.logging:*log-stream* out)
          (sibyl.logging:*log-level* :debug)
          (sibyl.logging:*log-format* :json))
      (sibyl.logging:with-request-context
          (:request-id "test-\"quote\"")
        (sibyl.logging:log-debug "test" "msg")))
    (let ((result (get-output-stream-string out)))
      (is (search "\\\"quote\\\"" result)
          "double quotes in request-id should be escaped"))))

(test json-format-newline-escape
  "JSON output escapes newlines in messages."
  (let ((out (make-string-output-stream)))
    (let ((sibyl.logging:*log-stream* out)
          (sibyl.logging:*log-level* :debug)
          (sibyl.logging:*log-format* :json))
      (sibyl.logging:log-debug "test" (format nil "line1~%line2")))
    (let ((result (get-output-stream-string out)))
      (is (search "\\n" result)
          "newlines in message should be escaped to \\n"))))

;;; ============================================================
;;; Structured format tests
;;; ============================================================

(test structured-format-with-context
  "Structured format includes agent name and step number in context."
  (let ((out (make-string-output-stream)))
    (let ((sibyl.logging:*log-stream* out)
          (sibyl.logging:*log-level* :debug)
          (sibyl.logging:*log-format* :structured)
          (sibyl.logging:*enable-colors* nil))
      (sibyl.logging:with-request-context
          (:request-id "struct-001" :agent-name "StructAgent" :step-number 7)
        (sibyl.logging:log-debug "agent" "Structured test")))
    (let ((result (get-output-stream-string out)))
      (is (search "agent:StructAgent" result)
          "structured format should show agent name")
      (is (search "step:7" result)
          "structured format should show step number"))))

;;; ============================================================
;;; Backward compatibility tests
;;; ============================================================

(test should-log-p-backward-compatible
  "should-log-p still works with existing (level) signature."
  (let ((sibyl.logging:*log-level* :info))
    (is (sibyl.logging:should-log-p :info)
        "info should pass at info level")
    (is (sibyl.logging:should-log-p :warn)
        "warn should pass at info level")
    (is (not (sibyl.logging:should-log-p :debug))
        "debug should not pass at info level")))

(test log-message-existing-pattern
  "Existing log-message call pattern works unchanged."
  (let ((out (make-string-output-stream)))
    (let ((sibyl.logging:*log-stream* out)
          (sibyl.logging:*log-level* :info)
          (sibyl.logging:*log-format* :standard)
          (sibyl.logging:*enable-colors* nil)
          (sibyl.logging:*current-request-context* nil))
      (sibyl.logging:log-info "tools" "Registered tool ~a" "test-tool"))
    (let ((result (get-output-stream-string out)))
      (is (search "Registered tool test-tool" result)
          "format args should work as before")
      (is (search "tools" result)
          "component name should appear"))))
