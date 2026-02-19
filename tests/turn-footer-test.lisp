;;;; turn-footer-test.lisp — Tests for format-turn-footer

(in-package :sibyl.tests)

(def-suite turn-footer-tests
  :description "Tests for multi-line turn footer formatting"
  :in sibyl-tests)

(in-suite turn-footer-tests)

;;; ============================================================
;;; Test: Base case (no cache, no thinking)
;;; ============================================================

(test base-footer-structure
  "Base footer has separator + 2 content lines (model/stats + tokens)"
  (let* ((sibyl.repl.display:*use-colors* nil)
         (output (with-output-to-string (s)
                   (sibyl.repl.display:format-turn-footer :stream s
                                       :model "claude-sonnet-4-6"
                                       :elapsed-seconds 2.34
                                       :input-tokens 1234
                                       :output-tokens 567
                                       :cost-usd 0.012345
                                       :context-percentage 42.5))))
    ;; Should have separator line
    (is (search "─────" output))
    ;; Should have model name
    (is (search "claude-sonnet-4-6" output))
    ;; Should have duration
    (is (search "2.34s" output))
    ;; Should have cost with 3 decimals
    (is (search "$0.012" output))
    ;; Should have context percentage
    (is (search "42%" output))
    ;; Should have token counts
    (is (search "In 1,234" output))
    (is (search "Out 567" output))
    ;; Should NOT have cache line
    (is (not (search "Cache" output)))
    ;; Should NOT have thinking tokens
    (is (not (search "Thinking" output)))))

;;; ============================================================
;;; Test: With thinking tokens
;;; ============================================================

(test footer-with-thinking
  "Footer includes thinking tokens in tokens line when present"
  (let* ((sibyl.repl.display:*use-colors* nil)
         (output (with-output-to-string (s)
                   (sibyl.repl.display:format-turn-footer :stream s
                                       :model "claude-opus-4-6"
                                       :elapsed-seconds 5.67
                                       :input-tokens 2000
                                       :output-tokens 800
                                       :thinking-tokens 1500
                                       :cost-usd 0.045
                                       :context-percentage 75.0))))
    ;; Should have thinking tokens
    (is (search "Thinking 1,500" output))
    ;; Should still have regular tokens
    (is (search "In 2,000" output))
    (is (search "Out 800" output))))

;;; ============================================================
;;; Test: With cache (conditional 3rd line)
;;; ============================================================

(test footer-with-cache
  "Footer includes cache line when cache-read-tokens > 0"
  (let* ((sibyl.repl.display:*use-colors* nil)
         (output (with-output-to-string (s)
                   (sibyl.repl.display:format-turn-footer :stream s
                                       :model "claude-sonnet-4-6"
                                       :elapsed-seconds 1.5
                                       :input-tokens 1000
                                       :output-tokens 500
                                       :cache-read-tokens 450
                                       :cache-write-tokens 89
                                       :cost-usd 0.008
                                       :context-percentage 30.0))))
    ;; Should have cache line
    (is (search "Cache" output))
    (is (search "Read 450" output))
    (is (search "Write 89" output))
    ;; Should have savings
    (is (search "saved $" output))))

;;; ============================================================
;;; Test: Ollama (no cost)
;;; ============================================================

(test footer-ollama-no-cost
  "Footer omits cost display when cost-usd is nil (Ollama)"
  (let* ((sibyl.repl.display:*use-colors* nil)
         (output (with-output-to-string (s)
                   (sibyl.repl.display:format-turn-footer :stream s
                                       :model "llama3.2:latest"
                                       :elapsed-seconds 3.2
                                       :input-tokens 1500
                                       :output-tokens 600
                                       :cost-usd nil
                                       :context-percentage 50.0))))
    ;; Should NOT have cost
    (is (not (search "$" output)))
    ;; Should have model and duration
    (is (search "llama3.2:latest" output))
    (is (search "3.20s" output))
    ;; Should have context percentage
    (is (search "50%" output))))

;;; ============================================================
;;; Test: Zero input tokens (edge case)
;;; ============================================================

(test footer-zero-input
  "Footer handles zero input tokens gracefully (0% context)"
  (let* ((sibyl.repl.display:*use-colors* nil)
         (output (with-output-to-string (s)
                   (sibyl.repl.display:format-turn-footer :stream s
                                       :model "claude-sonnet-4-6"
                                       :elapsed-seconds 1.0
                                       :input-tokens 0
                                       :output-tokens 100
                                       :cost-usd 0.001
                                       :context-percentage 0.0))))
    ;; Should have 0% context
    (is (search "0%" output))
    ;; Should have tokens
    (is (search "In 0" output))
    (is (search "Out 100" output))))

;;; ============================================================
;;; Test: Color mode
;;; ============================================================

(test footer-with-colors
  "Footer includes ANSI color codes when *use-colors* = t"
  (let* ((sibyl.repl.display:*use-colors* t)
         (output (with-output-to-string (s)
                   (sibyl.repl.display:format-turn-footer :stream s
                                       :model "claude-sonnet-4-6"
                                       :elapsed-seconds 2.0
                                       :input-tokens 5000
                                       :output-tokens 1000
                                       :cost-usd 0.020
                                       :context-percentage 85.0))))
    ;; Should have ANSI escape codes
    (is (search (string #\Escape) output))
    ;; Context percentage should be colored red (>80%)
    (is (search "85%" output))))

;;; ============================================================
;;; Test: No cache when cache-read-tokens = 0
;;; ============================================================

(test footer-no-cache-when-zero
  "Footer omits cache line when cache-read-tokens is 0"
  (let* ((sibyl.repl.display:*use-colors* nil)
         (output (with-output-to-string (s)
                   (sibyl.repl.display:format-turn-footer :stream s
                                       :model "claude-sonnet-4-6"
                                       :elapsed-seconds 1.0
                                       :input-tokens 1000
                                       :output-tokens 500
                                       :cache-read-tokens 0
                                       :cache-write-tokens 100
                                       :cost-usd 0.010
                                       :context-percentage 40.0))))
    ;; Should NOT have cache line (even though cache-write > 0)
    (is (not (search "Cache" output)))))

;;; ============================================================
;;; Test: Duration formatting (minutes)
;;; ============================================================

(test footer-duration-minutes
  "Footer formats durations >= 60s as minutes"
  (let* ((sibyl.repl.display:*use-colors* nil)
         (output (with-output-to-string (s)
                   (sibyl.repl.display:format-turn-footer :stream s
                                       :model "claude-opus-4-6"
                                       :elapsed-seconds 125.3
                                       :input-tokens 5000
                                       :output-tokens 2000
                                       :cost-usd 0.100
                                       :context-percentage 60.0))))
    ;; Should format as "2m 5s"
    (is (search "2m 5s" output))))

;;; ============================================================
;;; Test: Context percentage color thresholds
;;; ============================================================

(test footer-context-percentage-colors
  "Context percentage uses correct color thresholds"
  ;; Green: < 60%
  (let* ((sibyl.repl.display:*use-colors* t)
         (output-green (with-output-to-string (s)
                         (sibyl.repl.display:format-turn-footer :stream s
                                             :model "test"
                                             :elapsed-seconds 1.0
                                             :input-tokens 1000
                                             :output-tokens 100
                                             :cost-usd 0.01
                                             :context-percentage 50.0))))
    ;; Should contain green color code (32)
    (is (search "[32m" output-green)))
  
  ;; Yellow: 60-80%
  (let* ((sibyl.repl.display:*use-colors* t)
         (output-yellow (with-output-to-string (s)
                          (sibyl.repl.display:format-turn-footer :stream s
                                              :model "test"
                                              :elapsed-seconds 1.0
                                              :input-tokens 1000
                                              :output-tokens 100
                                              :cost-usd 0.01
                                              :context-percentage 70.0))))
    ;; Should contain yellow color code (33)
    (is (search "[33m" output-yellow)))
  
  ;; Red: > 80%
  (let* ((sibyl.repl.display:*use-colors* t)
         (output-red (with-output-to-string (s)
                       (sibyl.repl.display:format-turn-footer :stream s
                                           :model "test"
                                           :elapsed-seconds 1.0
                                           :input-tokens 1000
                                           :output-tokens 100
                                           :cost-usd 0.01
                                           :context-percentage 90.0))))
    ;; Should contain red color code (31)
    (is (search "[31m" output-red))))

;;; ============================================================
;;; Test: Complete footer with all features
;;; ============================================================

(test footer-complete
  "Complete footer with cache, thinking, cost, and context"
  (let* ((sibyl.repl.display:*use-colors* nil)
         (output (with-output-to-string (s)
                   (sibyl.repl.display:format-turn-footer :stream s
                                       :model "claude-opus-4-6"
                                       :elapsed-seconds 10.5
                                       :input-tokens 10000
                                       :output-tokens 5000
                                       :thinking-tokens 3000
                                       :cache-read-tokens 2000
                                       :cache-write-tokens 500
                                       :cost-usd 0.150
                                       :context-percentage 95.0))))
    ;; Should have all components
    (is (search "claude-opus-4-6" output))
    (is (search "10.50s" output))
    (is (search "$0.150" output))
    (is (search "95%" output))
    (is (search "In 10,000" output))
    (is (search "Out 5,000" output))
    (is (search "Thinking 3,000" output))
    (is (search "Cache" output))
    (is (search "Read 2,000" output))
    (is (search "Write 500" output))
    (is (search "saved $" output))))
