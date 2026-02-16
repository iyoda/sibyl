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
       (:file "builtin")))
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
     (:file "message-test"))))
  :perform (test-op (o c)
             (uiop:symbol-call :fiveam :run!
               (uiop:find-symbol* '#:sibyl-tests '#:sibyl.tests))))
