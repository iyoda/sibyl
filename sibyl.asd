;;;; sibyl.asd — System definition for Sibyl
;;;; A Lisp-based coding agent

(asdf:defsystem #:sibyl
  :description "A Lisp-based coding agent — research prototype"
  :author "Atikoro"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:alexandria
               #:dexador
               #:yason
               #:cl-ppcre
               #:bordeaux-threads
               #:uiop
               #:local-time
               #:ironclad)
  :serial t
  :components
  ((:module "src"
    :components
    ((:file "packages")
     (:module "system"
      :components
      ((:file "asdf-protection")))
     (:file "conditions")
     (:file "config")
     (:file "util")
     (:file "logging")
     (:module "llm"
      :components
      ((:file "message")
       (:file "client")
       (:file "token-tracker")
       (:file "pricing")
       (:file "metrics")
       (:file "providers")
       (:file "model-selector")
       (:file "ollama")))
     (:module "plan"
       :components
       ((:file "core")))
     (:module "tools"
       :components
       ((:file "protocol")
        (:file "builtin")
        (:file "lisp-tools")
        (:file "analysis-tools")
        (:file "self-tools")
        (:file "evolution-tools")
        (:file "refactor-tools")
        (:file "planning-tools")))
     (:module "mcp"
       :components
       ((:file "client")
        (:file "tools")))
     (:module "agent"
       :components
       ((:file "memory")
        (:file "multi-agent")
        (:file "core")))
     (:module "cache"
       :components
       ((:file "config")
        (:file "key")
        (:file "protocol")
        (:file "lru")
        (:file "telemetry")
        (:file "anthropic")
        (:file "openai")
        (:file "integration")))
     (:module "repl-module"
      :pathname "repl"
      :components
      ((:file "spinner")
       (:file "display" :depends-on ("spinner"))))
     (:file "repl"))))
  :in-order-to ((test-op (test-op #:sibyl/tests))))

;;; Optional subsystem: ensures cl-readline is available at ASDF load time.
;;; Since v0.1.0+, start-repl auto-loads cl-readline via Quicklisp at
;;; startup (%ensure-readline), so this subsystem is no longer required
;;; for normal use.  It remains useful for CI or reproducible builds
;;; where you want an explicit ASDF dependency on cl-readline.
(asdf:defsystem #:sibyl/readline
  :description "Optional cl-readline integration for Sibyl REPL history"
  :depends-on (#:sibyl #:cl-readline)
  :components ())

(asdf:defsystem #:sibyl/tests
  :depends-on (#:sibyl #:fiveam)
  :serial t
  :components
  ((:module "tests"
    :components
    ((:file "suite")
     (:file "tools-test")
      (:file "sexp-tools-test")
      (:file "analysis-tools-test")
      (:file "creation-tools-test")
      (:file "message-test")
     (:file "planning-test")
     (:file "client-test")
     (:file "agent-test")
     (:file "asdf-protection-test")
      (:file "repl-test")
      (:file "repl-display-test")
      (:file "turn-footer-test")
      (:file "rich-repl-test")
     (:file "parallel-runner-test")
     (:file "parallel-agent-test")
      (:file "mcp-test")
       (:file "token-tracking-test")
       (:file "token-tracker-enhancement-test")
        (:file "ollama-test")
        (:file "openai-test")
        (:file "anthropic-streaming-test")
        (:file "provisioning-test")
         (:file "phase6-test")
         (:file "cache-test")
         (:file "tool-timing-test")
         (:file "session-summary-test"))))
  :perform (test-op (o c)
             (uiop:symbol-call '#:sibyl.tests '#:run-sibyl-tests)))
