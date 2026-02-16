;;;; agent-test.lisp — Tests for agent core functionality

(defpackage #:sibyl.agent.tests
  (:use #:cl #:fiveam)
  (:import-from #:sibyl.agent
                #:*default-system-prompt*
                #:make-agent
                #:agent-system-prompt)
  (:export #:agent-tests))

(in-package #:sibyl.agent.tests)

(def-suite agent-tests
  :description "Tests for agent core functionality"
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
