;;;; repl-test.lisp — Tests for REPL commands

(in-package #:sibyl.tests)

(def-suite repl-tests
  :description "Tests for REPL command handling.")

(in-suite repl-tests)

;;; ============================================================
;;; Command recognition tests
;;; ============================================================

(test improve-command-registered
  "Test that /improve command is registered in *repl-commands*."
  (is (assoc "/improve" sibyl.repl::*repl-commands* :test #'string-equal))
  (is (eq :improve (cdr (assoc "/improve" sibyl.repl::*repl-commands* 
                                :test #'string-equal)))))

(test improve-command-recognized
  "Test that /improve is recognized as a command."
  (is (eq :improve (sibyl.repl:repl-command-p "/improve"))))

;;; ============================================================
;;; Argument parsing tests
;;; ============================================================

(test parse-improve-args-basic
  "Test parsing basic /improve command with task description."
  (multiple-value-bind (task auto-commit)
      (sibyl.repl::parse-improve-args "/improve add new feature")
    (is (string= "add new feature" task))
    (is (null auto-commit))))

(test parse-improve-args-with-auto-commit
  "Test parsing /improve command with --auto-commit flag."
  (multiple-value-bind (task auto-commit)
      (sibyl.repl::parse-improve-args "/improve add feature --auto-commit")
    (is (string= "add feature" task))
    (is (eq t auto-commit))))

(test parse-improve-args-auto-commit-middle
  "Test parsing /improve with --auto-commit in middle of task."
  (multiple-value-bind (task auto-commit)
      (sibyl.repl::parse-improve-args "/improve add --auto-commit new feature")
    ;; Should extract everything before --auto-commit as task
    (is (string= "add" task))
    (is (eq t auto-commit))))

(test parse-improve-args-empty-task
  "Test parsing /improve with empty task description."
  (multiple-value-bind (task auto-commit)
      (sibyl.repl::parse-improve-args "/improve")
    (is (string= "" task))
    (is (null auto-commit))))

(test parse-improve-args-only-auto-commit
  "Test parsing /improve with only --auto-commit flag."
  (multiple-value-bind (task auto-commit)
      (sibyl.repl::parse-improve-args "/improve --auto-commit")
    (is (string= "" task))
    (is (eq t auto-commit))))

(test parse-improve-args-whitespace-handling
  "Test that parse-improve-args handles extra whitespace correctly."
  (multiple-value-bind (task auto-commit)
      (sibyl.repl::parse-improve-args "/improve   task with spaces   --auto-commit  ")
    (is (string= "task with spaces" task))
    (is (eq t auto-commit))))

;;; ============================================================
;;; Help text verification
;;; ============================================================

(test help-includes-improve
  "Test that /help output includes /improve command."
  (let ((output (with-output-to-string (*standard-output*)
                  (sibyl.repl::handle-repl-command :help nil))))
    (is (search "/improve" output :test #'string-equal))))
