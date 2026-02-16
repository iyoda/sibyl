;;;; agent-test.lisp — Tests for agent core functionality

(defpackage #:sibyl.agent.tests
  (:use #:cl #:fiveam)
  (:import-from #:sibyl.agent
                #:*default-system-prompt*
                #:make-agent
                #:agent-system-prompt)
  (:export #:agent-tests
           #:tdd-orchestration-tests))

(in-package #:sibyl.agent.tests)

(def-suite agent-tests
  :description "Tests for agent core functionality"
  :in sibyl.tests:sibyl-tests)

(def-suite tdd-orchestration-tests
  :description "TDD workflow system prompt tests"
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
