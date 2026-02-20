;;;; session-test.lisp — Tests for session persistence

(in-package #:sibyl.tests)

(def-suite session-tests
  :description "Tests for session serialization, file I/O, and auto-save timer."
  :in sibyl-tests)

(in-suite session-tests)

;;; ============================================================
;;; Helper: create temp directory for tests
;;; ============================================================

(defun %make-test-session-dir ()
  "Create a unique temp directory for session tests."
  (let ((dir (merge-pathnames
              (format nil "sibyl-session-test-~a-~a/"
                      (get-universal-time) (random 100000))
              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defun %cleanup-test-dir (dir)
  "Remove test directory, ignoring errors."
  (handler-case (uiop:delete-directory-tree dir :validate t)
    (error () nil)))

;;; ============================================================
;;; Serialization round-trip tests
;;; ============================================================

(test test-tool-call-roundtrip
  "Tool-call with arguments survives sexp round-trip."
  (let* ((tc (sibyl.llm:make-tool-call :id "tc_42" :name "read-file"
              :arguments '(("path" . "/tmp/foo.lisp"))))
         (sexp (sibyl.repl::tool-call->sexp tc))
         (restored (sibyl.repl::sexp->tool-call sexp)))
    (is (string= "tc_42" (sibyl.llm:tool-call-id restored)))
    (is (string= "read-file" (sibyl.llm:tool-call-name restored)))
    (is (string= "/tmp/foo.lisp"
                 (cdr (assoc "path" (sibyl.llm:tool-call-arguments restored)
                             :test #'string=))))))

(test test-tool-call-nil-args
  "Tool-call with nil arguments round-trips."
  (let* ((tc (sibyl.llm:make-tool-call :id "tc_99" :name "no-args" :arguments nil))
         (sexp (sibyl.repl::tool-call->sexp tc))
         (restored (sibyl.repl::sexp->tool-call sexp)))
    (is (string= "tc_99" (sibyl.llm:tool-call-id restored)))
    (is (null (sibyl.llm:tool-call-arguments restored)))))

(test test-message-user-roundtrip
  "User message round-trips through sexp."
  (let* ((msg (sibyl.llm:user-message "hello world"))
         (sexp (sibyl.repl::message->sexp msg))
         (restored (sibyl.repl::sexp->message sexp)))
    (is (eq :user (sibyl.llm:message-role restored)))
    (is (string= "hello world" (sibyl.llm:message-content restored)))))

(test test-message-assistant-with-tools
  "Assistant message with tool-calls round-trips."
  (let* ((tc (sibyl.llm:make-tool-call :id "tc_1" :name "shell"
              :arguments '(("command" . "ls"))))
         (msg (sibyl.llm:assistant-message "thinking..." :tool-calls (list tc)))
         (sexp (sibyl.repl::message->sexp msg))
         (restored (sibyl.repl::sexp->message sexp)))
    (is (eq :assistant (sibyl.llm:message-role restored)))
    (is (string= "thinking..." (sibyl.llm:message-content restored)))
    (is (= 1 (length (sibyl.llm:message-tool-calls restored))))
    (is (string= "tc_1"
                 (sibyl.llm:tool-call-id
                  (first (sibyl.llm:message-tool-calls restored)))))))

(test test-message-tool-result
  "Tool result message with tool-call-id round-trips."
  (let* ((msg (sibyl.llm:tool-result-message "tc_1" "file contents here"))
         (sexp (sibyl.repl::message->sexp msg))
         (restored (sibyl.repl::sexp->message sexp)))
    (is (eq :tool (sibyl.llm:message-role restored)))
    (is (string= "file contents here" (sibyl.llm:message-content restored)))
    (is (string= "tc_1" (sibyl.llm:message-tool-call-id restored)))))

(test test-session-roundtrip
  "Full session with mixed messages and summary round-trips."
  (let* ((messages (list (sibyl.llm:user-message "hello")
                         (sibyl.llm:assistant-message "hi there")
                         (sibyl.llm:tool-result-message "tc_1" "result")))
         (sexp (sibyl.repl::session->sexp "test-sess" messages "old context"
                                          :command-count 7)))
    (multiple-value-bind (restored-msgs summary count)
        (sibyl.repl::sexp->session sexp)
      (is (= 3 (length restored-msgs)))
      (is (string= "old context" summary))
      (is (= 7 count)))))

(test test-system-messages-filtered
  "System messages are excluded from session serialization."
  (let* ((messages (list (sibyl.llm:make-message :role :system :content "You are Sibyl")
                         (sibyl.llm:user-message "hello")
                         (sibyl.llm:assistant-message "hi")))
         (sexp (sibyl.repl::session->sexp "test-filter" messages nil)))
    (multiple-value-bind (restored-msgs summary count)
        (sibyl.repl::sexp->session sexp)
      (declare (ignore summary count))
      (is (= 2 (length restored-msgs)))
      (is (notany (lambda (m) (eq :system (sibyl.llm:message-role m)))
                  restored-msgs)))))

;;; ============================================================
;;; File I/O tests
;;; ============================================================

(test test-save-and-load
  "Save session then load it back — data preserved."
  (let* ((dir (%make-test-session-dir))
         (msgs (list (sibyl.llm:user-message "save test")
                     (sibyl.llm:assistant-message "ok"))))
    (unwind-protect
         (progn
           (sibyl.repl:save-session "io-test-001" msgs "test summary"
                                     :directory dir :command-count 5)
           (multiple-value-bind (loaded-msgs loaded-summary loaded-count)
               (sibyl.repl:load-session "io-test-001" :directory dir)
             (is (= 2 (length loaded-msgs)))
             (is (string= "test summary" loaded-summary))
             (is (= 5 loaded-count))))
      (%cleanup-test-dir dir))))

(test test-load-nonexistent
  "Loading a nonexistent session returns nil."
  (let ((dir (%make-test-session-dir)))
    (unwind-protect
         (is (null (sibyl.repl:load-session "does-not-exist-999" :directory dir)))
      (%cleanup-test-dir dir))))

(test test-list-sessions
  "Saving multiple sessions and listing returns correct metadata."
  (let* ((dir (%make-test-session-dir))
         (msgs-a (list (sibyl.llm:user-message "a1")
                       (sibyl.llm:assistant-message "a2")
                       (sibyl.llm:user-message "a3")))
         (msgs-b (list (sibyl.llm:user-message "b1"))))
    (unwind-protect
         (progn
           (sibyl.repl:save-session "list-a" msgs-a nil :directory dir)
           (sibyl.repl:save-session "list-b" msgs-b nil :directory dir)
           (let ((sessions (sibyl.repl:list-sessions :directory dir)))
             (is (= 2 (length sessions)))
             (let ((entry-a (find "list-a" sessions
                                  :key (lambda (e) (getf e :id))
                                  :test #'string=)))
               (is (not (null entry-a)))
               (is (= 3 (getf entry-a :message-count))))))
      (%cleanup-test-dir dir))))

(test test-index-rebuild
  "Index is rebuilt when index file is missing."
  (let* ((dir (%make-test-session-dir))
         (msgs (list (sibyl.llm:user-message "rebuild test"))))
    (unwind-protect
         (progn
           (sibyl.repl:save-session "rebuild-001" msgs nil :directory dir)
           ;; Delete index file
           (let ((index-path (merge-pathnames "index.lisp"
                               (uiop:ensure-directory-pathname dir))))
             (when (probe-file index-path) (delete-file index-path)))
           ;; List should rebuild
           (let ((sessions (sibyl.repl:list-sessions :directory dir)))
             (is (= 1 (length sessions)))
             (is (string= "rebuild-001" (getf (first sessions) :id)))))
      (%cleanup-test-dir dir))))

;;; ============================================================
;;; Session ID generation tests
;;; ============================================================

(test test-generate-session-id
  "Generated ID matches expected format."
  (let ((id (sibyl.repl:generate-session-id)))
    (is (stringp id))
    (is (> (length id) 20))
    (is (eql 0 (search "session-" id)))))

(test test-unique-session-ids
  "Two generated IDs are different (random suffix)."
  (let ((id1 (sibyl.repl:generate-session-id))
        (id2 (sibyl.repl:generate-session-id)))
    (is (not (string= id1 id2)))))

;;; ============================================================
;;; Auto-save timer tests
;;; ============================================================

(test test-timer-stops
  "Timer stops cleanly on stop-auto-save-timer."
  (sibyl.repl::start-auto-save-timer
   (lambda () (values nil nil nil 0))
   60)
  (sibyl.repl::stop-auto-save-timer)
  (is (null sibyl.repl::*auto-save-thread*))
  (is (eq t sibyl.repl::*auto-save-stop-flag*)))
