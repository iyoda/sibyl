;;;; token-tracker-enhancement-test.lisp — Tests for cost accumulator and thinking tokens

(in-package #:sibyl.llm)

(5am:def-suite token-tracker-enhancement-tests
  :description "Tests for token-tracker cost and thinking-tokens enhancements")

(5am:in-suite token-tracker-enhancement-tests)

;;; Test 1: make-token-tracker creates struct with cost-usd=0.0d0 and thinking-tokens=0
(5am:test make-token-tracker-initializes-new-fields
  "make-token-tracker creates struct with cost-usd=0.0d0 and thinking-tokens=0"
  (let ((tracker (make-token-tracker)))
    (5am:is (= 0.0d0 (token-tracker-cost-usd tracker)))
    (5am:is (= 0 (token-tracker-thinking-tokens tracker)))))

;;; Test 2: tracker-add-usage with :thinking-tokens increments field
(5am:test tracker-add-usage-increments-thinking-tokens
  "tracker-add-usage with :thinking-tokens 500 increments field"
  (let ((tracker (make-token-tracker)))
    (tracker-add-usage tracker '(:input-tokens 100 :output-tokens 50 :thinking-tokens 500))
    (5am:is (= 500 (token-tracker-thinking-tokens tracker)))
    (5am:is (= 100 (token-tracker-input-tokens tracker)))
    (5am:is (= 50 (token-tracker-output-tokens tracker)))))

;;; Test 3: tracker-add-usage WITHOUT :thinking-tokens works (backward compat)
(5am:test tracker-add-usage-backward-compatible
  "tracker-add-usage WITHOUT :thinking-tokens works (backward compat)"
  (let ((tracker (make-token-tracker)))
    (tracker-add-usage tracker '(:input-tokens 100 :output-tokens 50))
    (5am:is (= 0 (token-tracker-thinking-tokens tracker)))
    (5am:is (= 100 (token-tracker-input-tokens tracker)))
    (5am:is (= 50 (token-tracker-output-tokens tracker)))))

;;; Test 4: tracker-add-cost accumulates: 0.01 + 0.02 = 0.03
(5am:test tracker-add-cost-accumulates
  "tracker-add-cost accumulates: 0.01 + 0.02 = 0.03"
  (let ((tracker (make-token-tracker)))
    (tracker-add-cost tracker 0.01d0)
    (5am:is (= 0.01d0 (token-tracker-cost-usd tracker)))
    (tracker-add-cost tracker 0.02d0)
    (5am:is (= 0.03d0 (token-tracker-cost-usd tracker)))))

;;; Test 5: context-window-for-model returns correct values
(5am:test context-window-for-model-returns-correct-values
  "context-window-for-model returns correct values for known models"
  ;; Claude Opus 4.6
  (5am:is (= 200000 (context-window-for-model "claude-opus-4-6")))
  ;; Claude Sonnet 4.6
  (5am:is (= 200000 (context-window-for-model "claude-sonnet-4-6")))
  ;; GPT-5 Mini
  (5am:is (= 200000 (context-window-for-model "gpt-5-mini")))
  ;; Unknown model (fallback)
  (5am:is (= 200000 (context-window-for-model "unknown-model-xyz"))))

;;; Test 6: Multiple thinking-tokens accumulations
(5am:test tracker-add-usage-accumulates-thinking-tokens
  "tracker-add-usage accumulates thinking-tokens across multiple calls"
  (let ((tracker (make-token-tracker)))
    (tracker-add-usage tracker '(:input-tokens 100 :output-tokens 50 :thinking-tokens 200))
    (tracker-add-usage tracker '(:input-tokens 150 :output-tokens 75 :thinking-tokens 300))
    (5am:is (= 500 (token-tracker-thinking-tokens tracker)))
    (5am:is (= 250 (token-tracker-input-tokens tracker)))
    (5am:is (= 125 (token-tracker-output-tokens tracker)))))
