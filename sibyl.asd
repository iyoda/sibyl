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
               #:local-time)
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
     (:module "llm"
      :components
      ((:file "message")
       (:file "client")
       (:file "providers")))
      (:module "tools"
       :components
       ((:file "protocol")
        (:file "builtin")
        (:file "lisp-tools")))
     (:module "agent"
      :components
      ((:file "memory")
       (:file "core")))
     (:file "repl"))))
  :in-order-to ((test-op (test-op #:sibyl/tests))))

(asdf:defsystem #:sibyl/tests
  :depends-on (#:sibyl #:fiveam)
  :serial t
  :components
   ((:module "tests"
     :components
     ((:file "suite")
      (:file "tools-test")
      (:file "sexp-tools-test")
      (:file "message-test")
      (:file "client-test")
      (:file "agent-test")
       (:file "asdf-protection-test")
       (:file "repl-test")
       (:file "evolution-state-test"))))
  :perform (test-op (o c)
              (uiop:symbol-call '#:sibyl.tests '#:run-sibyl-tests)))
