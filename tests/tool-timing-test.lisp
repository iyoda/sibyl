;;;; tool-timing-test.lisp — Tests for tool execution timing and :on-tool-result hook

(defpackage #:sibyl.tests.tool-timing
  (:use #:cl #:fiveam)
  (:import-from #:sibyl.agent
                #:make-agent
                #:agent-step
                #:agent-hooks)
  (:import-from #:sibyl.llm
                #:make-tool-call
                #:make-message
                #:user-message
                #:assistant-message)
  (:import-from #:sibyl.tools
                #:deftool
                #:unregister-tool))

(in-package #:sibyl.tests.tool-timing)

(def-suite tool-timing-tests
  :description "Tests for tool execution timing measurement and :on-tool-result hook"
  :in sibyl.tests:sibyl-tests)

(in-suite tool-timing-tests)

;;; ============================================================
;;; Mock LLM client for testing
;;; ============================================================

(defclass mock-timing-client ()
  ((response-sequence :initarg :responses
                      :accessor mock-responses
                      :initform nil
                      :documentation "List of responses to return in sequence."))
  (:documentation "Mock LLM client that returns pre-programmed responses."))

(defmethod sibyl.llm:complete ((client mock-timing-client) messages &key)
  (declare (ignore messages))
  (let ((response (pop (mock-responses client))))
    (if response
        (values response nil)
        (values (make-message :role :assistant :content "No more responses") nil))))

(defmethod sibyl.llm:complete-with-tools ((client mock-timing-client) messages tools &key)
  (declare (ignore messages tools))
  (let ((response (pop (mock-responses client))))
    (if response
        (values response nil)
        (values (make-message :role :assistant :content "No more responses") nil))))

;;; ============================================================
;;; Test: Hook fires after tool execution
;;; ============================================================

(test hook-fires-after-tool-execution
  "Verify :on-tool-result hook fires after tool execution with correct arguments."
  (let ((hook-called nil)
        (hook-tool-call nil)
        (hook-result nil)
        (hook-elapsed nil))
    ;; Define a test tool
    (deftool "test-timing-tool"
        (:description "A test tool for timing measurement"
         :parameters ((:name "input" :type "string" :required t
                       :description "Test input")))
      (sleep 0.01)  ; Small delay to ensure measurable timing
      (format nil "Result: ~a" (getf args :input)))
    
    (unwind-protect
         (let* ((client (make-instance 'mock-timing-client
                                       :responses
                                       (list
                                        ;; First response: tool call
                                        (make-message
                                         :role :assistant
                                         :content ""
                                         :tool-calls
                                         (list (make-tool-call
                                                :id "call_1"
                                                :name "test-timing-tool"
                                                :arguments '(:input "test"))))
                                        ;; Second response: final text
                                        (make-message
                                         :role :assistant
                                         :content "Done"))))
                (agent (make-agent :client client :max-steps 5)))
           ;; Register hook
           (setf (agent-hooks agent)
                 (list (cons :on-tool-result
                             (lambda (tc result elapsed)
                               (setf hook-called t
                                     hook-tool-call tc
                                     hook-result result
                                     hook-elapsed elapsed)))))
           ;; Execute agent step
           (agent-step agent "test input")
           
           ;; Verify hook was called
           (is-true hook-called "Hook should be called")
           (is-true hook-tool-call "Hook should receive tool-call")
           (is (string= "test-timing-tool" (sibyl.llm:tool-call-name hook-tool-call))
               "Hook should receive correct tool-call")
           (is-true hook-result "Hook should receive result string")
           (is (search "Result: test" hook-result) "Hook should receive correct result")
           (is-true hook-elapsed "Hook should receive elapsed time")
           (is (numberp hook-elapsed) "Elapsed should be a number")
           (is (> hook-elapsed 0.0) "Elapsed should be positive")
           (is (< hook-elapsed 60.0) "Elapsed should be reasonable (< 60s)"))
      ;; Cleanup
      (unregister-tool "test-timing-tool"))))

;;; ============================================================
;;; Test: Hook receives all three arguments
;;; ============================================================

(test hook-receives-all-arguments
  "Verify :on-tool-result hook receives tool-call, result, and elapsed-seconds."
  (let ((hook-args-count 0))
    ;; Define a test tool
    (deftool "test-args-tool"
        (:description "A test tool for argument verification"
         :parameters ((:name "value" :type "string" :required t
                       :description "Test value")))
      (format nil "Value: ~a" (getf args :value)))
    
    (unwind-protect
         (let* ((client (make-instance 'mock-timing-client
                                       :responses
                                       (list
                                        (make-message
                                         :role :assistant
                                         :content ""
                                         :tool-calls
                                         (list (make-tool-call
                                                :id "call_2"
                                                :name "test-args-tool"
                                                :arguments '(:value "42"))))
                                        (make-message
                                         :role :assistant
                                         :content "Complete"))))
                (agent (make-agent :client client :max-steps 5)))
           ;; Register hook that counts arguments
           (setf (agent-hooks agent)
                 (list (cons :on-tool-result
                             (lambda (&rest args)
                               (setf hook-args-count (length args))))))
           ;; Execute
           (agent-step agent "test")
           
           ;; Verify 3 arguments
           (is (= 3 hook-args-count) "Hook should receive exactly 3 arguments"))
      ;; Cleanup
      (unregister-tool "test-args-tool"))))

;;; ============================================================
;;; Test: Hook fires for error results
;;; ============================================================

(test hook-fires-for-errors
  "Verify :on-tool-result hook fires even when tool execution fails."
  (let ((hook-called nil)
        (hook-result nil))
    ;; Define a tool that always errors
    (deftool "test-error-tool"
        (:description "A test tool that always errors"
         :parameters ((:name "dummy" :type "string" :required t
                       :description "Dummy parameter")))
      (error "Intentional test error"))
    
    (unwind-protect
         (let* ((client (make-instance 'mock-timing-client
                                       :responses
                                       (list
                                        (make-message
                                         :role :assistant
                                         :content ""
                                         :tool-calls
                                         (list (make-tool-call
                                                :id "call_3"
                                                :name "test-error-tool"
                                                :arguments '(:dummy "test"))))
                                        (make-message
                                         :role :assistant
                                         :content "Handled error"))))
                (agent (make-agent :client client :max-steps 5)))
           ;; Register hook
           (setf (agent-hooks agent)
                 (list (cons :on-tool-result
                             (lambda (tc result elapsed)
                               (declare (ignore tc elapsed))
                               (setf hook-called t
                                     hook-result result)))))
           ;; Execute
           (agent-step agent "test")
           
           ;; Verify hook was called with error result
           (is-true hook-called "Hook should be called even on error")
           (is-true hook-result "Hook should receive error result")
           (is (search "Error:" hook-result) "Result should contain error message"))
      ;; Cleanup
      (unregister-tool "test-error-tool"))))

;;; ============================================================
;;; Test: Hook errors don't interrupt agent-step
;;; ============================================================

(test hook-errors-are-caught
  "Verify that errors in :on-tool-result hook don't interrupt agent-step."
  (let ((agent-completed nil))
    ;; Define a test tool
    (deftool "test-hook-error-tool"
        (:description "A test tool for hook error handling"
         :parameters ((:name "data" :type "string" :required t
                       :description "Test data")))
      (format nil "Data: ~a" (getf args :data)))
    
    (unwind-protect
         (let* ((client (make-instance 'mock-timing-client
                                       :responses
                                       (list
                                        (make-message
                                         :role :assistant
                                         :content ""
                                         :tool-calls
                                         (list (make-tool-call
                                                :id "call_4"
                                                :name "test-hook-error-tool"
                                                :arguments '(:data "test"))))
                                        (make-message
                                         :role :assistant
                                         :content "Final response"))))
                (agent (make-agent :client client :max-steps 5)))
           ;; Register hook that always errors
           (setf (agent-hooks agent)
                 (list (cons :on-tool-result
                             (lambda (tc result elapsed)
                               (declare (ignore tc result elapsed))
                               (error "Hook error - should be caught")))))
           ;; Execute - should complete despite hook error
           (let ((result (agent-step agent "test")))
             (setf agent-completed t)
             (is (string= "Final response" result)
                 "Agent should complete despite hook error")))
      ;; Cleanup
      (unregister-tool "test-hook-error-tool"))
    
    (is-true agent-completed "Agent-step should complete successfully")))

;;; ============================================================
;;; Test: Timing is wall-clock seconds
;;; ============================================================

(test timing-is-wall-clock-seconds
  "Verify that elapsed time is measured in wall-clock seconds."
  (let ((hook-elapsed nil))
    ;; Define a tool with known delay
    (deftool "test-delay-tool"
        (:description "A test tool with known delay"
         :parameters ((:name "delay" :type "string" :required t
                       :description "Delay parameter")))
      (sleep 0.05)  ; 50ms delay
      "Delayed result")
    
    (unwind-protect
         (let* ((client (make-instance 'mock-timing-client
                                       :responses
                                       (list
                                        (make-message
                                         :role :assistant
                                         :content ""
                                         :tool-calls
                                         (list (make-tool-call
                                                :id "call_5"
                                                :name "test-delay-tool"
                                                :arguments '(:delay "50ms"))))
                                        (make-message
                                         :role :assistant
                                         :content "Done"))))
                (agent (make-agent :client client :max-steps 5)))
           ;; Register hook
           (setf (agent-hooks agent)
                 (list (cons :on-tool-result
                             (lambda (tc result elapsed)
                               (declare (ignore tc result))
                               (setf hook-elapsed elapsed)))))
           ;; Execute
           (agent-step agent "test")
           
           ;; Verify timing is reasonable (>= 50ms, < 1s)
           (is-true hook-elapsed "Hook should receive elapsed time")
           (is (>= hook-elapsed 0.04) "Elapsed should be >= 40ms (accounting for variance)")
           (is (< hook-elapsed 1.0) "Elapsed should be < 1s"))
      ;; Cleanup
      (unregister-tool "test-delay-tool"))))
