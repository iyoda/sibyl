;;;; agent-test.lisp — Tests for agent core functionality

(defpackage #:sibyl.agent.tests
  (:use #:cl #:fiveam)
  (:import-from #:sibyl.agent
                #:*default-system-prompt*
                #:agent-hooks
                #:make-agent
                #:agent-system-prompt
                #:run-hook
                #:make-memory
                #:memory-push
                #:memory-compact
                #:memory-conversation)
  (:import-from #:sibyl.llm
                #:user-message
                #:assistant-message
                #:tool-result-message
                #:make-tool-call
                #:message-role
                #:conversation-to-list)
  (:export #:agent-tests
            #:tdd-orchestration-tests
            #:memory-compact-tests))

(in-package #:sibyl.agent.tests)

(def-suite agent-tests
  :description "Tests for agent core functionality"
  :in sibyl.tests:sibyl-tests)

(def-suite tdd-orchestration-tests
  :description "TDD workflow system prompt tests"
  :in sibyl.tests:sibyl-tests)

(def-suite run-hook-tests
  :description "Tests for run-hook function."
  :in sibyl.tests:sibyl-tests)

(in-suite agent-tests)

;;; ============================================================
;;; System Prompt Self-Awareness Tests
;;; ============================================================

(test system-prompt-contains-self-awareness
  "System prompt should mention self-awareness and self-modification capabilities."
  (let ((prompt (string-downcase *default-system-prompt*)))
    (is (search "self" prompt)
        "System prompt should mention 'self' (self-awareness)")
    (is (or (search "source code" prompt)
            (search "introspect" prompt)
            (search "modify" prompt))
        "System prompt should mention ability to access/modify source code")))

(test system-prompt-lists-lisp-tools
  "System prompt should list the 7 Lisp-aware tools."
  (let ((prompt (string-downcase *default-system-prompt*)))
    ;; Check for all 7 Lisp-aware tools
    (is (search "read-sexp" prompt)
        "System prompt should mention read-sexp tool")
    (is (search "describe-symbol" prompt)
        "System prompt should mention describe-symbol tool")
    (is (search "eval-form" prompt)
        "System prompt should mention eval-form tool")
    (is (search "macroexpand-form" prompt)
        "System prompt should mention macroexpand-form tool")
    (is (search "package-symbols" prompt)
        "System prompt should mention package-symbols tool")
    (is (search "who-calls" prompt)
        "System prompt should mention who-calls tool")
    (is (search "codebase-map" prompt)
        "System prompt should mention codebase-map tool")))

(test system-prompt-mentions-lisp-awareness
  "System prompt should indicate Lisp-specific capabilities."
  (let ((prompt (string-downcase *default-system-prompt*)))
    (is (or (search "lisp" prompt)
            (search "s-expression" prompt)
            (search "sexp" prompt))
        "System prompt should mention Lisp or S-expressions")))

;;; ============================================================
;;; TDD Workflow Tests
;;; ============================================================

(in-suite tdd-orchestration-tests)

(test system-prompt-mentions-tdd-workflow
  "System prompt should include RED/GREEN/REFACTOR workflow steps."
  (let ((prompt (string-downcase *default-system-prompt*)))
    (is (search "**red**" prompt)
        "System prompt should mention RED step")
    (is (search "**green**" prompt)
        "System prompt should mention GREEN step")
    (is (search "**refactor**" prompt)
        "System prompt should mention REFACTOR step")
    (is (search "**persist**" prompt)
        "System prompt should mention PERSIST step")))

(test system-prompt-lists-tdd-tools
  "System prompt should list all TDD workflow tools."
  (let ((prompt (string-downcase *default-system-prompt*)))
    (is (search "write-test" prompt)
        "System prompt should mention write-test tool")
    (is (search "run-tests" prompt)
        "System prompt should mention run-tests tool")
    (is (search "safe-redefine" prompt)
        "System prompt should mention safe-redefine tool")
    (is (search "sync-to-file" prompt)
        "System prompt should mention sync-to-file tool")
    (is (search "sibyl.system:unprotect-file" prompt)
        "System prompt should mention sibyl.system:unprotect-file")))

(test system-prompt-emphasizes-test-first
  "System prompt should emphasize writing tests first."
  (let ((prompt (string-downcase *default-system-prompt*)))
    (is (or (search "write the test first" prompt)
            (search "writing tests first" prompt))
        "System prompt should state the write-test-first principle")))

(in-suite agent-tests)

(test agent-uses-system-prompt
  "Agent created with make-agent should use the default system prompt."
  (let* ((mock-client nil) ; We don't need a real client for this test
         (agent (make-agent :client mock-client)))
    (is (string= *default-system-prompt* (agent-system-prompt agent))
        "Agent should use *default-system-prompt* by default")))

(test agent-custom-system-prompt
  "Agent should accept custom system prompt."
  (let* ((mock-client nil)
         (custom-prompt "Custom prompt for testing")
         (agent (make-agent :client mock-client :system-prompt custom-prompt)))
    (is (string= custom-prompt (agent-system-prompt agent))
        "Agent should use custom system prompt when provided")))

;;; ============================================================
;;; run-hook Tests
;;; ============================================================

(in-suite run-hook-tests)

(test run-hook-executes-registered-hook
  "run-hook executes a registered hook with arguments."
  (let* ((agent (make-agent :client nil))
         (called nil)
         (received-args nil))
    (setf (agent-hooks agent)
          (list (cons :before-step
                      (lambda (&rest args)
                        (setf called t
                              received-args args)
                        :ok))))
    (is (eq :ok (run-hook agent :before-step 1 "context"))
        "run-hook should return hook result")
    (is (not (null called)) "Hook should be called")
    (is (equal '(1 "context") received-args)
        "Hook should receive provided args")))

(test run-hook-ignores-missing-hook
  "run-hook returns nil when hook is not registered."
  (let ((agent (make-agent :client nil)))
    (setf (agent-hooks agent)
          (list (cons :after-step (lambda (&rest args)
                                    (declare (ignore args))
                                    :after))))
    (is (null (run-hook agent :on-tool-call :tool-call))
        "Missing hook should return NIL without errors")))

(test run-hook-handles-hook-errors
  "run-hook should warn and continue if hook signals an error."
  (let* ((agent (make-agent :client nil))
         (warned nil))
    (setf (agent-hooks agent)
          (list (cons :on-error
                      (lambda (&rest args)
                        (declare (ignore args))
                        (error "boom")))))
    (handler-bind ((warning (lambda (condition)
                              (declare (ignore condition))
                              (setf warned t)
                              (muffle-warning condition))))
      (is (null (run-hook agent :on-error :tool-error))
          "Errors should not propagate from hooks"))
    (is (not (null warned)) "Hook errors should emit a warning")))

(test run-hook-selects-hook-by-name
  "run-hook should execute the hook matching the requested name."
  (let* ((agent (make-agent :client nil))
         (selected nil))
    (setf (agent-hooks agent)
          (list (cons :before-step (lambda (&rest args)
                                     (declare (ignore args))
                                     :before))
                (cons :after-step (lambda (&rest args)
                                    (declare (ignore args))
                                    (setf selected :after)
                                    :after))))
    (is (eq :after (run-hook agent :after-step "response"))
        "run-hook should call the hook matching the name")
    (is (eq :after selected)
        "Correct hook should be selected")))

;;; ============================================================
;;; Memory Compact Tests — tool_use/tool_result pair integrity
;;; ============================================================

(def-suite memory-compact-tests
  :description "Tests for memory-compact tool_use/tool_result boundary handling."
  :in sibyl.tests:sibyl-tests)

(in-suite memory-compact-tests)

(test compact-skips-orphaned-tool-results
  "memory-compact must not leave orphaned tool_result messages at the
   start of the kept portion. When the split point lands on a :tool
   message, it should advance past all consecutive :tool messages."
  ;; max-messages=6 → keep-count=3 → split-idx = total - 3
  ;; We push 7 messages so compaction triggers. We arrange the messages
  ;; so that the naive split-idx (4) lands on a :tool message.
  (let ((mem (make-memory :max-messages 6)))
    ;; msg 0: user
    (memory-push mem (user-message "hello"))
    ;; msg 1: assistant (plain)
    (memory-push mem (assistant-message "hi"))
    ;; msg 2: user
    (memory-push mem (user-message "read file"))
    ;; msg 3: assistant with tool_calls — will be summarized
    (memory-push mem (assistant-message "let me check"
                       :tool-calls (list (make-tool-call
                                          :id "toolu_orphan_test"
                                          :name "read-file"
                                          :arguments '(("path" . "/tmp/x"))))))
    ;; msg 4: tool result — naive split lands HERE (orphaned!)
    (memory-push mem (tool-result-message "toolu_orphan_test" "file contents"))
    ;; msg 5: assistant
    (memory-push mem (assistant-message "the file contains..."))
    ;; msg 6: user — triggers compaction (7 > 6)
    (memory-push mem (user-message "thanks"))

    ;; After compaction, the first remaining message must NOT be :tool
    (let* ((remaining (conversation-to-list (memory-conversation mem)))
           (first-role (message-role (first remaining))))
      (is (not (eq :tool first-role))
          "First message after compaction must not be an orphaned :tool (tool_result)"))))

(test compact-skips-multiple-consecutive-tool-results
  "When multiple consecutive :tool messages sit at the split boundary,
   all of them must be moved into the summarized portion."
  ;; max-messages=6 → keep-count=3 → split-idx = total - 3
  ;; 8 messages → split-idx = 5
  (let ((mem (make-memory :max-messages 6)))
    ;; msg 0: user
    (memory-push mem (user-message "hello"))
    ;; msg 1: assistant (plain)
    (memory-push mem (assistant-message "hi"))
    ;; msg 2: user
    (memory-push mem (user-message "do two things"))
    ;; msg 3: assistant with 2 tool calls
    (memory-push mem (assistant-message nil
                       :tool-calls (list (make-tool-call
                                          :id "tc_a" :name "tool-a"
                                          :arguments nil)
                                         (make-tool-call
                                          :id "tc_b" :name "tool-b"
                                          :arguments nil))))
    ;; msg 4: tool result for tc_a
    (memory-push mem (tool-result-message "tc_a" "result a"))
    ;; msg 5: tool result for tc_b — split lands HERE for 8 msgs
    (memory-push mem (tool-result-message "tc_b" "result b"))
    ;; msg 6: assistant
    (memory-push mem (assistant-message "both done"))
    ;; msg 7: user — triggers compaction (8 > 6)
    (memory-push mem (user-message "great"))

    (let* ((remaining (conversation-to-list (memory-conversation mem)))
           (roles (mapcar #'message-role remaining)))
      ;; No :tool message should appear at the start of remaining
      (is (not (eq :tool (first roles)))
          "First remaining message must not be :tool")
      ;; Verify the remaining conversation starts cleanly
      (is (member (first roles) '(:user :assistant))
          "Remaining conversation should start with :user or :assistant"))))

(test compact-no-adjustment-when-split-is-clean
  "When the split point does not land on a :tool message, compaction
   should behave identically to the original algorithm."
  ;; max-messages=6 → keep-count=3 → split-idx = 4
  ;; All messages are user/assistant, no tool results at boundary
  (let ((mem (make-memory :max-messages 6)))
    (memory-push mem (user-message "m0"))
    (memory-push mem (assistant-message "m1"))
    (memory-push mem (user-message "m2"))
    (memory-push mem (assistant-message "m3"))
    (memory-push mem (user-message "m4"))
    (memory-push mem (assistant-message "m5"))
    ;; msg 6 triggers compaction
    (memory-push mem (user-message "m6"))

    (let ((remaining (conversation-to-list (memory-conversation mem))))
      ;; keep-count=3, so exactly 3 messages should remain
      (is (= 3 (length remaining))
          "Clean split should keep exactly ceiling(max/2) messages"))))
