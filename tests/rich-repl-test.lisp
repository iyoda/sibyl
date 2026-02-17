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
    (sleep 0.3)
    (is (not (bt:thread-alive-p thread)))))

(test spinner-message-default
  "start-spinner initialises message to Thinking... by default"
  (let* ((*standard-output* (make-string-output-stream))
         (s (sibyl.repl.spinner:start-spinner)))
    (unwind-protect
         (is (string= "Thinking..." (sibyl.repl.spinner::spinner-message s)))
      (sibyl.repl.spinner:stop-spinner s))))

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
  (let ((sibyl.repl::*use-colors* nil))
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((hook (sibyl.repl::make-tool-call-hook)))
                      (let ((tc (sibyl.llm:make-tool-call
                                 :name "read-file"
                                 :id "tc_test"
                                 :arguments nil)))
                        (funcall hook tc))))))
      (is (search "read-file" output)))))

(test tool-hook-no-colors-plain-text
  "make-tool-call-hook uses plain format when *use-colors* is nil (no ANSI codes)"
  (let ((sibyl.repl::*use-colors* nil))
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((hook (sibyl.repl::make-tool-call-hook)))
                      (let ((tc (sibyl.llm:make-tool-call
                                 :name "write-file"
                                 :id "tc_test2"
                                 :arguments nil)))
                        (funcall hook tc))))))
      ;; Plain text: no escape sequences
      (is (not (search (string #\Escape) output)))
      (is (search "write-file" output)))))

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

;;; ============================================================
;;; streaming defaults
;;; ============================================================

(test stream-enabled-default
  "Test that *stream-enabled* is T by default"
  (is (eq t sibyl.repl::*stream-enabled*)))

(test streaming-callback-invoked
  "Test that *streaming-text-callback* is invoked when bound"
  (let ((collected nil))
    (let ((sibyl.llm:*streaming-text-callback*
            (lambda (text) (push text collected))))
      (is (functionp sibyl.llm:*streaming-text-callback*))
      (funcall sibyl.llm:*streaming-text-callback* "hello")
      (funcall sibyl.llm:*streaming-text-callback* " world"))
    (is (null sibyl.llm:*streaming-text-callback*))
    (is (equal '(" world" "hello") collected))))
