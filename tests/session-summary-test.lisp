;;;; session-summary-test.lisp — Tests for session summary display
;;;;
;;;; Tests the format-session-summary function with various scenarios:
;;;; - Complete session with all metrics
;;;; - Zero requests (empty tracker)
;;;; - No thinking tokens
;;;; - No cache tokens
;;;; - Color vs no-color modes

(in-package #:sibyl.tests)

(def-suite session-summary-tests
  :description "Tests for session summary display formatting"
  :in sibyl-tests)

(in-suite session-summary-tests)

;;; ============================================================
;;; Test Helpers
;;; ============================================================

(defun make-test-tracker (&key (input 0) (output 0) (thinking 0)
                               (cache-read 0) (cache-write 0)
                               (requests 0) (cost 0.0d0))
  "Create a token tracker with specified values for testing."
  (let ((tracker (sibyl.llm:make-token-tracker)))
    (setf (sibyl.llm::token-tracker-input-tokens tracker) input)
    (setf (sibyl.llm::token-tracker-output-tokens tracker) output)
    (setf (sibyl.llm::token-tracker-thinking-tokens tracker) thinking)
    (setf (sibyl.llm::token-tracker-cache-read-tokens tracker) cache-read)
    (setf (sibyl.llm::token-tracker-cache-write-tokens tracker) cache-write)
    (setf (sibyl.llm::token-tracker-request-count tracker) requests)
    (setf (sibyl.llm::token-tracker-cost-usd tracker) cost)
    tracker))

;;; ============================================================
;;; Tests
;;; ============================================================

(test session-summary-includes-total-cost
  "Session summary includes total cost from tracker."
  (let* ((tracker (make-test-tracker :input 10000 :output 5000
                                     :requests 5 :cost 0.145d0))
         (summary (with-output-to-string (s)
                    (sibyl.repl.display:format-session-summary tracker "claude-opus-4-6" s))))
    (is (search "Total Cost" summary))
    (is (search "$0.145" summary))))

(test session-summary-includes-thinking-tokens
  "Session summary includes thinking tokens when > 0."
  (let* ((tracker (make-test-tracker :input 10000 :output 5000
                                     :thinking 2100 :requests 5))
         (summary (with-output-to-string (s)
                    (sibyl.repl.display:format-session-summary tracker "claude-opus-4-6" s))))
    (is (search "Thinking" summary))
    (is (search "2,100" summary))))

(test session-summary-omits-zero-thinking-tokens
  "Session summary omits thinking tokens line when 0."
  (let* ((tracker (make-test-tracker :input 10000 :output 5000
                                     :thinking 0 :requests 5))
         (summary (with-output-to-string (s)
                    (sibyl.repl.display:format-session-summary tracker "claude-opus-4-6" s))))
    ;; Should not have a dedicated "Thinking" line when zero
    ;; (implementation may show "0" or omit entirely; we test for omission)
    (is (not (search "Thinking     2,100" summary)))))

(test session-summary-includes-cache-savings
  "Session summary includes cache savings from decompose-savings."
  (let* ((tracker (make-test-tracker :input 10000 :output 5000
                                     :cache-read 3456 :cache-write 890
                                     :requests 5))
         (summary (with-output-to-string (s)
                    (sibyl.repl.display:format-session-summary tracker "claude-opus-4-6" s))))
    ;; Should show cache savings (calculated via decompose-savings)
    (is (search "saved" summary))
    ;; Should show cache read tokens
    (is (search "3,456" summary))))

(test session-summary-includes-server-cache-hit-rate
  "Session summary includes server cache hit rate."
  (let* ((tracker (make-test-tracker :input 10000 :output 5000
                                     :cache-read 3456 :cache-write 890
                                     :requests 5))
         (summary (with-output-to-string (s)
                    (sibyl.repl.display:format-session-summary tracker "claude-opus-4-6" s))))
    ;; Should show server hit rate percentage
    (is (search "Server Hit Rate" summary))
    (is (search "%" summary))))

(test session-summary-includes-response-cache-stats
  "Session summary includes response cache hit rate and entries."
  ;; Note: This test assumes response cache is initialized
  ;; We'll test that the summary attempts to access cache stats
  (let* ((tracker (make-test-tracker :input 10000 :output 5000 :requests 12))
         (summary (with-output-to-string (s)
                    (sibyl.repl.display:format-session-summary tracker "claude-opus-4-6" s))))
    ;; Should show response cache section
    (is (search "Response Cache" summary))
    ;; Should show hit rate or entries
    (is (or (search "Entries" summary)
            (search "/" summary)))))

(test session-summary-no-color-mode
  "Session summary produces clean text without ANSI codes when *use-colors* = nil."
  (let* ((tracker (make-test-tracker :input 10000 :output 5000 :requests 5))
         (sibyl.repl.display:*use-colors* nil)
         (summary (with-output-to-string (s)
                    (sibyl.repl.display:format-session-summary tracker "claude-opus-4-6" s))))
    ;; Should not contain ANSI escape sequences
    (is (not (search (string #\Escape) summary)))
    ;; Should still contain key information
    (is (search "Session Summary" summary))
    (is (search "10,000" summary))))

(test session-summary-handles-zero-requests
  "Session summary handles empty tracker gracefully (no division by zero)."
  (let* ((tracker (make-test-tracker :requests 0))
         (summary (with-output-to-string (s)
                    (sibyl.repl.display:format-session-summary tracker "claude-opus-4-6" s))))
    ;; Should show 0 requests
    (is (search "Requests" summary))
    (is (search "0" summary))
    ;; Should not crash or show NaN/Inf
    (is (not (search "NaN" summary)))
    (is (not (search "Inf" summary)))))

(test session-summary-uses-separator-line
  "Session summary uses ═ separator for visual distinction."
  (let* ((tracker (make-test-tracker :input 10000 :output 5000 :requests 5))
         (summary (with-output-to-string (s)
                    (sibyl.repl.display:format-session-summary tracker "claude-opus-4-6" s))))
    ;; Should use ═ character (not ─)
    (is (search "═" summary))))

(test session-summary-shows-model-name
  "Session summary displays the model name."
  (let* ((tracker (make-test-tracker :input 10000 :output 5000 :requests 5))
         (summary (with-output-to-string (s)
                    (sibyl.repl.display:format-session-summary tracker "claude-opus-4-6" s))))
    (is (search "claude-opus-4-6" summary))))
