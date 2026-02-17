;;;; parallel-agent-test.lisp — Tests for parallel task execution in multi-agent system

(defpackage #:sibyl.parallel.tests
  (:use #:cl #:fiveam)
  (:import-from #:sibyl.agent
                #:make-agent-coordinator
                #:create-task
                #:coordinator-task-queue
                #:coordinator-strategy
                #:task-status
                #:task-result
                #:task-description
                #:task-id
                #:execute-tasks
                #:execute-tasks-parallel
                #:execute-tasks-sequential))

(in-package #:sibyl.parallel.tests)

(def-suite parallel-agent-tests
  :description "Tests for parallel task execution in multi-agent system"
  :in sibyl.tests:sibyl-tests)

(in-suite parallel-agent-tests)

;;; ============================================================
;;; Task creation and queue management
;;; ============================================================

(test task-creation-basic
  "Tasks are created with :pending status and unique IDs."
  (let ((coordinator (make-agent-coordinator :strategy :parallel)))
    (let ((t1 (create-task coordinator "task-A"))
          (t2 (create-task coordinator "task-B")))
      (is (eq :pending (task-status t1)))
      (is (eq :pending (task-status t2)))
      (is (string/= (task-id t1) (task-id t2))
          "Task IDs must be unique")
      (is (= 2 (length (coordinator-task-queue coordinator)))))))

(test task-with-dependencies
  "Tasks can be created with dependency lists."
  (let ((coordinator (make-agent-coordinator :strategy :parallel)))
    (let* ((t1 (create-task coordinator "base-task"))
           (t2 (create-task coordinator "dependent-task"
                            :dependencies (list (task-id t1)))))
      (is (null (sibyl.agent:task-dependencies t1))
          "Base task has no dependencies")
      (is (equal (list (task-id t1))
                 (sibyl.agent:task-dependencies t2))
          "Dependent task has correct dependency"))))

;;; ============================================================
;;; Parallel execution: independent tasks run concurrently
;;; ============================================================

(test parallel-execution-runs-all-tasks
  "execute-tasks-parallel completes all independent tasks."
  (let ((coordinator (make-agent-coordinator :strategy :parallel))
        (execution-log (list))
        (log-lock (bt:make-lock "test-log-lock")))
    ;; Register a custom executor that records execution order
    (let ((t1 (create-task coordinator "alpha"))
          (t2 (create-task coordinator "beta"))
          (t3 (create-task coordinator "gamma")))
      ;; Run parallel execution with a mock executor
      (sibyl.agent:execute-tasks-parallel
       coordinator
       :task-fn (lambda (task)
                  (bt:with-lock-held (log-lock)
                    (push (task-description task) execution-log))
                  (format nil "done:~a" (task-description task))))
      ;; All tasks should be completed
      (is (eq :completed (task-status t1)))
      (is (eq :completed (task-status t2)))
      (is (eq :completed (task-status t3)))
      ;; All tasks should have results
      (is (string= "done:alpha" (task-result t1)))
      (is (string= "done:beta"  (task-result t2)))
      (is (string= "done:gamma" (task-result t3)))
      ;; All 3 tasks were executed
      (is (= 3 (length execution-log))))))

(test parallel-execution-is-concurrent
  "Independent tasks run in parallel (timing test)."
  (let ((coordinator (make-agent-coordinator :strategy :parallel))
        (start-times (make-hash-table :test 'equal))
        (lock (bt:make-lock "timing-lock")))
    ;; Create 3 tasks that each sleep 0.1s
    (create-task coordinator "slow-1")
    (create-task coordinator "slow-2")
    (create-task coordinator "slow-3")
    (let ((wall-start (get-internal-real-time)))
      (sibyl.agent:execute-tasks-parallel
       coordinator
       :task-fn (lambda (task)
                  (bt:with-lock-held (lock)
                    (setf (gethash (task-description task) start-times)
                          (get-internal-real-time)))
                  (sleep 0.1)
                  "ok"))
      (let* ((wall-end (get-internal-real-time))
             (elapsed (/ (- wall-end wall-start)
                         internal-time-units-per-second)))
        ;; Sequential would take 0.3s; parallel should take ~0.1s
        ;; Allow generous margin: must be < 0.25s
        (is (< elapsed 0.25)
            (format nil "Parallel execution took ~,3fs (expected < 0.25s)" elapsed))))))

;;; ============================================================
;;; Dependency-aware parallel execution
;;; ============================================================

(test parallel-respects-dependencies
  "Tasks with dependencies run only after their dependencies complete."
  (let ((coordinator (make-agent-coordinator :strategy :parallel))
        (completion-order (list))
        (lock (bt:make-lock "order-lock")))
    (let* ((base (create-task coordinator "base"))
           (dep  (create-task coordinator "dependent"
                              :dependencies (list (task-id base)))))
      (sibyl.agent:execute-tasks-parallel
       coordinator
       :task-fn (lambda (task)
                  (bt:with-lock-held (lock)
                    (push (task-description task) completion-order))
                  "done"))
      ;; "base" must appear before "dependent" in completion order
      ;; (completion-order is built with push, so it's reversed)
      (let ((order (reverse completion-order)))
        (is (string= "base" (first order))
            "Base task must complete before dependent task")
        (is (string= "dependent" (second order))
            "Dependent task must run after base")))))

;;; ============================================================
;;; Error handling in parallel tasks
;;; ============================================================

(test parallel-handles-task-errors
  "Failed tasks are marked :failed without crashing other tasks."
  (let ((coordinator (make-agent-coordinator :strategy :parallel)))
    (let ((good (create-task coordinator "good-task"))
          (bad  (create-task coordinator "bad-task")))
      (sibyl.agent:execute-tasks-parallel
       coordinator
       :task-fn (lambda (task)
                  (if (string= "bad-task" (task-description task))
                      (error "Simulated task failure")
                      "success")))
      ;; Good task should complete
      (is (eq :completed (task-status good)))
      (is (string= "success" (task-result good)))
      ;; Bad task should be marked failed
      (is (eq :failed (task-status bad))))))

;;; ============================================================
;;; execute-tasks dispatch
;;; ============================================================

(test execute-tasks-dispatches-parallel
  "execute-tasks with :parallel strategy calls execute-tasks-parallel."
  (let ((coordinator (make-agent-coordinator :strategy :parallel)))
    (create-task coordinator "dispatch-test")
    ;; Should not error — dispatches to parallel implementation
    (is (not (null
              (handler-case
                  (progn
                    (execute-tasks coordinator
                                   :task-fn (lambda (task)
                                              (declare (ignore task))
                                              "dispatched"))
                    t)
                (error () nil)))))))
