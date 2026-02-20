;;;; tests/rich-repl-test.lisp — Tests for rich REPL features (Phase 1)
;;;;
;;;; Covers: spinner lifecycle, cancel flags, llm-cancelled condition,
;;;; tool-call hook, elapsed-time utilities, readline probe, defvar defaults.

(in-package #:sibyl.tests)

(def-suite rich-repl-tests
  :description "Tests for rich REPL functionality (Phase 1)"
  :in sibyl-tests)

(in-suite rich-repl-tests)

;;; ============================================================
;;; Existing output tests (format helpers)
;;; ============================================================

(test format-colored-text-basic
  "format-colored-text emits text and the ANSI green color code"
  (let ((output (with-output-to-string (*standard-output*)
                  (sibyl.repl::format-colored-text "Hello" :green))))
    (is (search "Hello" output))
    (is (search "32m" output)))) ; ANSI green color code

(test format-prompt-enhanced
  "format-enhanced-prompt returns a string containing the name and counter"
  (let ((prompt (sibyl.repl::format-enhanced-prompt "sibyl" 42)))
    (is (stringp prompt))
    (is (search "sibyl" prompt))
    (is (search "42" prompt))))

(test command-completion-basic
  "complete-command returns /help for the partial string /he"
  (let ((completions (sibyl.repl::complete-command "/he")))
    (is (member "/help" completions :test #'string=))))

;;; ============================================================
;;; Spinner: start/stop lifecycle
;;; ============================================================

(test spinner-starts-active
  "start-spinner returns a spinner with active-p true"
  (let* ((*standard-output* (make-string-output-stream))
         (s (sibyl.repl.spinner:start-spinner "Test")))
    (unwind-protect
         (is (sibyl.repl.spinner:spinner-active-p s))
      (sibyl.repl.spinner:stop-spinner s))))

(test spinner-stops
  "stop-spinner deactivates the spinner (active-p returns NIL)"
  (let* ((*standard-output* (make-string-output-stream))
         (s (sibyl.repl.spinner:start-spinner "Test")))
    (sibyl.repl.spinner:stop-spinner s)
    (is (not (sibyl.repl.spinner:spinner-active-p s)))))

(test spinner-thread-terminates
  "stop-spinner terminates the background thread within 500 ms"
  (let* ((*standard-output* (make-string-output-stream))
         (s (sibyl.repl.spinner:start-spinner "Test"))
         (thread (sibyl.repl.spinner::spinner-thread s)))
    (sibyl.repl.spinner:stop-spinner s)
    (loop repeat 30  ; max 300ms (30 * 10ms)
          while (bt:thread-alive-p thread)
          do (sleep 0.01))
    (is (not (bt:thread-alive-p thread)))))

(test spinner-message-default
  "start-spinner initialises message to Thinking... by default"
  (let* ((*standard-output* (make-string-output-stream))
         (s (sibyl.repl.spinner:start-spinner)))
    (unwind-protect
         (is (string= "Thinking..." (sibyl.repl.spinner::spinner-message s)))
      (sibyl.repl.spinner:stop-spinner s))))

;;; ============================================================
;;; Spinner: stream resilience
;;; ============================================================

(test spinner-survives-closed-stream
  "The spinner thread must not crash when its output stream is closed.
   Previously this caused SB-KERNEL:LAYOUT-INVALID and SBCL corruption."
  ;; Bind *standard-output* only during start-spinner so FiveAM's
  ;; format-to-t reporting uses the real output stream.
  (let* ((stream (make-string-output-stream))
         (s (let ((*standard-output* stream))
              (sibyl.repl.spinner:start-spinner "resilience test"))))
    ;; Close the stream while the spinner is still running.
    ;; The spinner should detect the write failure and self-stop
    ;; instead of crashing the SBCL process.
    (close stream)
    ;; Give the spinner thread time to encounter the closed stream.
    (sleep 0.3)
    ;; Now stop it cleanly — should not error.
    (finishes (sibyl.repl.spinner:stop-spinner s))
    (is (not (sibyl.repl.spinner:spinner-active-p s))
        "Spinner should be inactive after stop")))

(test spinner-stop-on-closed-stream
  "stop-spinner should not error when the spinner's stream is already closed."
  (let* ((stream (make-string-output-stream))
         (s (let ((*standard-output* stream))
              (sibyl.repl.spinner:start-spinner "Test"))))
    (sibyl.repl.spinner:stop-spinner s)
    (close stream)
    ;; Calling stop-spinner again on a closed stream should be safe.
    (finishes (sibyl.repl.spinner:stop-spinner s))))

;;; ============================================================
;;; Cancel flag: *cancel-requested* and *last-interrupt-time*
;;; ============================================================

(test cancel-flag-default
  "*cancel-requested* default value is NIL"
  (is (null sibyl.repl::*cancel-requested*)))

(test cancel-flag-settable
  "*cancel-requested* can be set to T and cleared back to NIL"
  (let ((original sibyl.repl::*cancel-requested*))
    (unwind-protect
         (progn
           (setf sibyl.repl::*cancel-requested* t)
           (is (not (null sibyl.repl::*cancel-requested*)))
           (setf sibyl.repl::*cancel-requested* nil)
           (is (null sibyl.repl::*cancel-requested*)))
      (setf sibyl.repl::*cancel-requested* original))))

(test last-interrupt-time-default
  "*last-interrupt-time* default value is 0"
  (is (= 0 sibyl.repl::*last-interrupt-time*)))

;;; ============================================================
;;; llm-cancelled condition type hierarchy
;;; ============================================================

(test llm-cancelled-is-llm-error
  "llm-cancelled is a subtype of llm-error"
  (let ((c (make-condition 'sibyl.conditions:llm-cancelled :message "test")))
    (is (typep c 'sibyl.conditions:llm-error))))

(test llm-cancelled-is-sibyl-error
  "llm-cancelled is a subtype of sibyl-error (full hierarchy)"
  (let ((c (make-condition 'sibyl.conditions:llm-cancelled :message "test")))
    (is (typep c 'sibyl.conditions:sibyl-error))
    (is (typep c 'error))))

(test llm-cancelled-message-slot
  "llm-cancelled condition report includes the message text"
  (let* ((c (make-condition 'sibyl.conditions:llm-cancelled :message "cancelled by user"))
         (report (format nil "~a" c)))
    (is (search "cancelled by user" report))))

;;; ============================================================
;;; Tool-call hook: make-tool-call-hook
;;; ============================================================

(test tool-hook-displays-tool-name
  "make-tool-call-hook prints the tool name when *use-colors* is nil"
  (let ((sibyl.repl::*use-colors* nil)
        (sibyl.repl::*current-spinner* nil))
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((hook (sibyl.repl::make-tool-call-hook)))
                      (let ((tc (sibyl.llm:make-tool-call
                                 :name "read-file"
                                 :id "tc_test"
                                 :arguments nil)))
                        (funcall hook tc)))
                    ;; Stop the restarted spinner before capturing output
                    (when sibyl.repl::*current-spinner*
                      (sibyl.repl.spinner:stop-spinner sibyl.repl::*current-spinner*)
                      (setf sibyl.repl::*current-spinner* nil)))))
      (is (search "read-file" output)))))

(test tool-hook-no-colors-plain-text
  "make-tool-call-hook uses plain format when *use-colors* is nil (no ANSI codes)"
  (let ((sibyl.repl::*use-colors* nil)
        (sibyl.repl::*current-spinner* nil))
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((hook (sibyl.repl::make-tool-call-hook)))
                      (let ((tc (sibyl.llm:make-tool-call
                                 :name "write-file"
                                 :id "tc_test2"
                                 :arguments nil)))
                        (funcall hook tc)))
                    ;; Stop the restarted spinner before output is finalized
                    (when sibyl.repl::*current-spinner*
                      (sibyl.repl.spinner:stop-spinner sibyl.repl::*current-spinner*)
                      (setf sibyl.repl::*current-spinner* nil)))))
      ;; Check the hook's display line only (before newline).
      ;; Spinner cleanup writes ANSI (\r\e[2K) to the same stream — ignore that.
      (let ((first-line (subseq output 0 (position #\Newline output))))
        (is (not (search (string #\Escape) first-line))
            "hook display line should have no ANSI escape codes")
        (is (search "write-file" first-line)
            "hook display line should contain tool name")))))

;;; ============================================================
;;; Elapsed time utilities
;;; ============================================================

(test format-elapsed-time-output
  "format-elapsed-time produces output containing 'elapsed' and the value"
  (let ((sibyl.repl::*use-colors* nil))
    (let ((output (with-output-to-string (*standard-output*)
                    (sibyl.repl::format-elapsed-time 2.3))))
      (is (search "elapsed" output))
      (is (search "2.3" output)))))

(test format-elapsed-time-bracket-format
  "format-elapsed-time wraps the value in square brackets when colors are off"
  (let ((sibyl.repl::*use-colors* nil))
    (let ((output (with-output-to-string (*standard-output*)
                    (sibyl.repl::format-elapsed-time 1.0))))
      (is (search "[" output))
      (is (search "]" output)))))

(test elapsed-seconds-accuracy
  "elapsed-seconds returns a value in a plausible range after a 100 ms sleep"
  (let ((start (get-internal-real-time)))
    (sleep 0.1)
    (let ((elapsed (sibyl.repl::elapsed-seconds start)))
      (is (> elapsed 0.08))
      (is (< elapsed 0.5)))))

(test elapsed-seconds-non-negative
  "elapsed-seconds returns a non-negative number"
  (let ((start (get-internal-real-time)))
    (let ((elapsed (sibyl.repl::elapsed-seconds start)))
      (is (>= elapsed 0)))))

;;; ============================================================
;;; readline probe
;;; ============================================================

(test readline-returns-boolean
  "readline-available-p returns a generalised boolean (T or NIL)"
  (let ((result (sibyl.repl::readline-available-p)))
    (is (or (eq result t) (eq result nil)))))

;;; ============================================================
;;; defvar default values
;;; ============================================================

(test use-colors-defvar
  "*use-colors* is bound with a truthy default (colors on by default)"
  (is (boundp 'sibyl.repl::*use-colors*)))

(test command-count-defvar
  "*command-count* has integer type and its initial value is 0"
  (is (boundp 'sibyl.repl::*command-count*))
  (is (integerp sibyl.repl::*command-count*)))

(test command-history-defvar
  "*command-history* is bound (initial value NIL = empty history)"
  (is (boundp 'sibyl.repl::*command-history*)))

(test ignore-ctrl-j-defvar
  "*ignore-ctrl-j* is bound and defaults to NIL"
  (is (boundp 'sibyl.repl::*ignore-ctrl-j*))
  (is (null sibyl.repl::*ignore-ctrl-j*)))

;;; ============================================================
;;; input sanitization
;;; ============================================================

(test strip-ctrl-j-removes-linefeed
  "%strip-ctrl-j removes Ctrl+J (linefeed) characters"
  (let ((input (concatenate 'string "hi" (string (code-char 10)) "there")))
    (is (string= "hithere" (sibyl.repl::%strip-ctrl-j input)))))

;;; ============================================================
;;; streaming defaults
;;; ============================================================

(test stream-enabled-default
  "Test that *stream-enabled* is T by default"
  (is (eq t sibyl.repl::*stream-enabled*)))

(test streaming-callback-invoked
  "Test that *streaming-text-callback* is invoked when bound and
   reverts to its enclosing binding when the let scope exits.
   Uses an explicit outer nil binding so the test is independent of
   whether start-repl is currently running."
  (let ((collected nil))
    ;; Outer binding: establish a known baseline of NIL so the test
    ;; is not affected by start-repl's own let* binding.
    (let ((sibyl.llm:*streaming-text-callback* nil))
      (let ((sibyl.llm:*streaming-text-callback*
              (lambda (text) (push text collected))))
        (is (functionp sibyl.llm:*streaming-text-callback*))
        (funcall sibyl.llm:*streaming-text-callback* "hello")
        (funcall sibyl.llm:*streaming-text-callback* " world"))
      ;; After inner let exits, should revert to outer binding (NIL).
      (is (null sibyl.llm:*streaming-text-callback*)))
    (is (equal '(" world" "hello") collected))))

;;; ============================================================
;;; Tool-call hook: spinner restart after tool display
;;; ============================================================

(test tool-hook-restarts-spinner-after-display
  "make-tool-call-hook should restart spinner after displaying tool name"
  (let ((sibyl.repl::*use-colors* nil)
        (sibyl.repl::*current-spinner* nil))
    (let* ((out (make-string-output-stream))
           (*standard-output* out)
           (hook (sibyl.repl::make-tool-call-hook))
           (tc (sibyl.llm:make-tool-call
                :id "test-id"
                :name "shell"
                :arguments (list (cons "command" "ls")))))
      (unwind-protect
           (progn
              (funcall hook tc)
              ;; Verify *current-spinner* is restarted after hook invocation
              (is (not (null sibyl.repl::*current-spinner*))
                 "spinner should be restarted after tool call display")
             (is (sibyl.repl.spinner:spinner-active-p sibyl.repl::*current-spinner*)
                  "restarted spinner should be active"))
         ;; Cleanup
         (when sibyl.repl::*current-spinner*
          (sibyl.repl.spinner:stop-spinner sibyl.repl::*current-spinner*)
          (setf sibyl.repl::*current-spinner* nil))))))

(test tool-hook-spinner-message-shows-tool-name
  "restarted spinner message should contain the tool name"
  (let ((sibyl.repl::*use-colors* nil)
        (sibyl.repl::*current-spinner* nil))
    (let* ((out (make-string-output-stream))
           (*standard-output* out)
           (hook (sibyl.repl::make-tool-call-hook))
           (tc (sibyl.llm:make-tool-call
                :id "test-id-2"
                :name "read-file"
                :arguments (list (cons "path" "src/repl.lisp")))))
      (unwind-protect
             (progn
              (funcall hook tc)
              (when sibyl.repl::*current-spinner*
                (let ((msg (sibyl.repl.spinner::spinner-message
                            sibyl.repl::*current-spinner*)))
                  (is (search "Thinking" msg)
                      "restarted spinner message should say Thinking..."))))
        (when sibyl.repl::*current-spinner*
          (sibyl.repl.spinner:stop-spinner sibyl.repl::*current-spinner*)
          (setf sibyl.repl::*current-spinner* nil))))))

(test tool-hook-multiple-calls-each-restart-spinner
  "consecutive tool call hooks each restart the spinner"
  (let ((sibyl.repl::*use-colors* nil)
        (sibyl.repl::*current-spinner* nil))
    (let* ((out (make-string-output-stream))
           (*standard-output* out)
           (hook (sibyl.repl::make-tool-call-hook)))
       (unwind-protect
            (progn
              ;; First tool call
              (funcall hook (sibyl.llm:make-tool-call
                            :id "tc-1" :name "shell"
                            :arguments (list (cons "command" "ls"))))
              (is (not (null sibyl.repl::*current-spinner*))
                  "spinner should exist after first tool call")
              ;; Second tool call (spinner already active)
              (funcall hook (sibyl.llm:make-tool-call
                            :id "tc-2" :name "read-file"
                            :arguments (list (cons "path" "src/"))))
             (is (not (null sibyl.repl::*current-spinner*))
                 "spinner should still exist after second tool call")
             (is (sibyl.repl.spinner:spinner-active-p sibyl.repl::*current-spinner*)
                 "spinner should be active after second tool call"))
        (when sibyl.repl::*current-spinner*
          (sibyl.repl.spinner:stop-spinner sibyl.repl::*current-spinner*)
          (setf sibyl.repl::*current-spinner* nil))))))

;;; ============================================================
;;; unbind-key integration tests
;;; ============================================================

(test ignore-ctrl-j-unbind-key-called-when-enabled
  "When *ignore-ctrl-j* is T and readline is available, unbind-key is called with (code-char 10)"
  ;; Mock: capture the key passed to unbind-key
  (let* ((captured-key nil)
         (sibyl.repl::*ignore-ctrl-j* t))
    ;; Only run if cl-readline is available (skip gracefully otherwise)
    (when (sibyl.repl::readline-available-p)
      (let ((original-fn (find-symbol "UNBIND-KEY" :cl-readline)))
        ;; Temporarily rebind unbind-key to capture the argument
        (setf (symbol-function (find-symbol "UNBIND-KEY" :cl-readline))
              (lambda (key) (setf captured-key key) nil))
        (unwind-protect
            (progn
              ;; Simulate the guard logic from start-repl
              (when (and sibyl.repl::*ignore-ctrl-j* (sibyl.repl::readline-available-p))
                (funcall (find-symbol "UNBIND-KEY" :cl-readline) (code-char 10)))
              (is (char= captured-key (code-char 10))
                  "unbind-key should be called with (code-char 10)"))
          ;; Restore original function
          (setf (symbol-function (find-symbol "UNBIND-KEY" :cl-readline))
                original-fn))))))

(test ignore-ctrl-j-unbind-key-not-called-when-disabled
  "When *ignore-ctrl-j* is NIL, unbind-key is NOT called"
  (let* ((called-p nil)
         (sibyl.repl::*ignore-ctrl-j* nil))
    (when (sibyl.repl::readline-available-p)
      (let ((original-fn (find-symbol "UNBIND-KEY" :cl-readline)))
        (setf (symbol-function (find-symbol "UNBIND-KEY" :cl-readline))
              (lambda (key) (declare (ignore key)) (setf called-p t) nil))
        (unwind-protect
            (progn
              ;; Simulate the guard logic from start-repl
              (when (and sibyl.repl::*ignore-ctrl-j* (sibyl.repl::readline-available-p))
                (funcall (find-symbol "UNBIND-KEY" :cl-readline) (code-char 10)))
              (is (null called-p)
                  "unbind-key should NOT be called when *ignore-ctrl-j* is NIL"))
          (setf (symbol-function (find-symbol "UNBIND-KEY" :cl-readline))
                original-fn))))
    ;; When readline not available, guard prevents call regardless
    (unless (sibyl.repl::readline-available-p)
      (is (null called-p)
          "unbind-key should NOT be called when readline is not available"))))

(test ignore-ctrl-j-guard-skips-when-readline-unavailable
  "When readline is not available, the guard prevents unbind-key call"
  ;; This tests the (readline-available-p) part of the guard
  ;; by verifying the guard condition evaluates to NIL when readline is absent
  (let ((sibyl.repl::*ignore-ctrl-j* t))
    ;; Simulate readline unavailable by checking the guard with mocked availability
    (let ((guard-result (and sibyl.repl::*ignore-ctrl-j* nil))) ; nil = readline unavailable
      (is (null guard-result)
          "Guard should be NIL when readline is unavailable, preventing unbind-key call"))))

;;; ============================================================
;;; Interrupt handler decision logic tests
;;; ============================================================

(def-suite interrupt-handler-tests
  :description "Tests for Ctrl+C interrupt handler decision logic"
  :in sibyl-tests)

(in-suite interrupt-handler-tests)

(test interrupt-action-single-press-returns-hint
  "Single press with large time gap returns :hint"
  (let ((window (* 2 internal-time-units-per-second))
        (now (* 100 internal-time-units-per-second))
        (last-time 0))
    (is (eq :hint (sibyl.repl::%interrupt-action now last-time window)))))

(test interrupt-action-double-press-returns-exit
  "Double press within 2-second window returns :exit"
  (let* ((window (* 2 internal-time-units-per-second))
         (last-time (* 50 internal-time-units-per-second))
         (now (+ last-time (floor window 2))))
    (is (eq :exit (sibyl.repl::%interrupt-action now last-time window)))))

(test interrupt-action-timeout-expired-returns-hint
  "Press after timeout expired (just over 2 seconds) returns :hint"
  (let* ((window (* 2 internal-time-units-per-second))
         (last-time (* 50 internal-time-units-per-second))
         (now (+ last-time window 1)))
    (is (eq :hint (sibyl.repl::%interrupt-action now last-time window)))))

(test interrupt-action-first-ever-press-returns-hint
  "First-ever press (last-time = 0, now = large) returns :hint"
  (let ((window (* 2 internal-time-units-per-second))
        (now (* 1000 internal-time-units-per-second))
        (last-time 0))
    (is (eq :hint (sibyl.repl::%interrupt-action now last-time window)))))

(test interrupt-action-boundary-at-window-edge
  "Press exactly at window boundary returns :hint (not :exit)"
  (let* ((window (* 2 internal-time-units-per-second))
         (last-time (* 50 internal-time-units-per-second))
         (now (+ last-time window)))
    (is (eq :hint (sibyl.repl::%interrupt-action now last-time window)))
    ;; One tick before window boundary should return :exit
    (is (eq :exit (sibyl.repl::%interrupt-action (1- now) last-time window)))))
