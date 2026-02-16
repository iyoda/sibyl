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

;;; ============================================================
;;; /review command tests
;;; ============================================================

(test review-command-registered
  "Test that /review command is registered in *repl-commands*."
  (is (assoc "/review" sibyl.repl::*repl-commands* :test #'string-equal))
  (is (eq :review (cdr (assoc "/review" sibyl.repl::*repl-commands* 
                              :test #'string-equal)))))

(test review-command-recognized
  "Test that /review is recognized as a command."
  (is (eq :review (sibyl.repl:repl-command-p "/review"))))

(test help-includes-review
  "Test that /help output includes /review command."
  (let ((output (with-output-to-string (*standard-output*)
                  (sibyl.repl::handle-repl-command :help nil))))
    (is (search "/review" output :test #'string-equal))))

;;; ============================================================
;;; Suggestion state management tests
;;; ============================================================

(test store-suggestion-creates-id
  "Test that store-suggestion creates a suggestion with an ID."
  (sibyl.repl::clear-suggestions)
  (let ((suggestion (sibyl.repl::store-suggestion 
                     "Add feature X" 
                     "It would be useful"
                     "high")))
    (is (numberp (getf suggestion :id)))
    (is (string= "Add feature X" (getf suggestion :description)))
    (is (string= "It would be useful" (getf suggestion :rationale)))
    (is (string= "high" (getf suggestion :priority)))
    (is (string= "pending" (getf suggestion :status)))))

(test get-suggestion-by-id-retrieves
  "Test that get-suggestion-by-id retrieves the correct suggestion."
  (sibyl.repl::clear-suggestions)
  (let* ((s1 (sibyl.repl::store-suggestion "Feature 1" "Reason 1" "high"))
         (s2 (sibyl.repl::store-suggestion "Feature 2" "Reason 2" "medium"))
         (id1 (getf s1 :id))
         (id2 (getf s2 :id)))
    (is (eq s1 (sibyl.repl::get-suggestion-by-id id1)))
    (is (eq s2 (sibyl.repl::get-suggestion-by-id id2)))))

(test get-suggestion-by-id-returns-nil-when-not-found
  "Test that get-suggestion-by-id returns NIL for non-existent ID."
  (sibyl.repl::clear-suggestions)
  (is (null (sibyl.repl::get-suggestion-by-id 99999))))

(test update-suggestion-status-modifies
  "Test that update-suggestion-status changes suggestion status."
  (sibyl.repl::clear-suggestions)
  (let* ((suggestion (sibyl.repl::store-suggestion 
                      "Feature" "Reason" "high"))
         (id (getf suggestion :id)))
    (is (string= "pending" (getf suggestion :status)))
    (sibyl.repl::update-suggestion-status id "approved")
    (is (string= "approved" (getf (sibyl.repl::get-suggestion-by-id id) :status)))))

(test store-suggestions-batch
  "Test that store-suggestions can store multiple suggestions."
  (sibyl.repl::clear-suggestions)
  (sibyl.repl::store-suggestions 
   (list (list :description "Feature 1" :rationale "Reason 1" :priority "high")
         (list :description "Feature 2" :rationale "Reason 2" :priority "medium")))
  (is (= 2 (length sibyl.repl::*pending-suggestions*))))

(test clear-suggestions-empties-list
  "Test that clear-suggestions removes all suggestions."
  (sibyl.repl::clear-suggestions)
  (sibyl.repl::store-suggestion "Feature" "Reason" "high")
  (is (> (length sibyl.repl::*pending-suggestions*) 0))
  (sibyl.repl::clear-suggestions)
  (is (= 0 (length sibyl.repl::*pending-suggestions*))))

;;; ============================================================
;;; /review argument parsing tests
;;; ============================================================

(test parse-review-args-no-action
  "Test parsing /review with no arguments (list all)."
  (multiple-value-bind (action id modification)
      (sibyl.repl::parse-review-args "/review")
    (is (null action))
    (is (null id))
    (is (null modification))))

(test parse-review-args-approve
  "Test parsing /review approve <id>."
  (multiple-value-bind (action id modification)
      (sibyl.repl::parse-review-args "/review approve 42")
    (is (string= "approve" action))
    (is (= 42 id))
    (is (null modification))))

(test parse-review-args-reject
  "Test parsing /review reject <id>."
  (multiple-value-bind (action id modification)
      (sibyl.repl::parse-review-args "/review reject 7")
    (is (string= "reject" action))
    (is (= 7 id))
    (is (null modification))))

(test parse-review-args-modify
  "Test parsing /review modify <id> <description>."
  (multiple-value-bind (action id modification)
      (sibyl.repl::parse-review-args "/review modify 3 add unit tests too")
    (is (string= "modify" action))
    (is (= 3 id))
    (is (string= "add unit tests too" modification))))

(test parse-review-args-invalid-id
  "Test parsing /review with invalid (non-numeric) ID."
  (multiple-value-bind (action id modification)
      (sibyl.repl::parse-review-args "/review approve notanumber")
    (is (string= "approve" action))
    (is (null id))
    (is (null modification))))

(test parse-review-args-whitespace
  "Test that parse-review-args handles extra whitespace."
  (multiple-value-bind (action id modification)
      (sibyl.repl::parse-review-args "/review   approve   5   ")
    (is (string= "approve" action))
    (is (= 5 id))
    (is (null modification))))
