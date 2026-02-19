;;;; pricing.lisp — Official Claude/OpenAI pricing table and cost estimation
;;;; Phase 3: Cost calculation engine
;;;; Prices are in USD per 1M tokens (as of 2025-10).
;;;; Sources: https://www.anthropic.com/pricing, https://openai.com/pricing

(in-package #:sibyl.llm)

(defparameter *model-pricing-table*
  '(;; Anthropic Claude models (USD per 1M tokens)
    ("claude-haiku-4-5-20251015"
     :input 1.00 :output 5.00 :cache-write 1.25 :cache-read 0.10)
    ("claude-haiku-4-5"
     :input 1.00 :output 5.00 :cache-write 1.25 :cache-read 0.10)
    ("claude-sonnet-4-5"
     :input 3.00 :output 15.00 :cache-write 3.75 :cache-read 0.30)
    ("claude-sonnet-4-6"
     :input 3.00 :output 15.00 :cache-write 3.75 :cache-read 0.30)
    ("claude-opus-4-5"
     :input 5.00 :output 25.00 :cache-write 6.25 :cache-read 0.50)
    ("claude-opus-4-6"
     :input 5.00 :output 25.00 :cache-write 6.25 :cache-read 0.50)
    ;; OpenAI models (USD per 1M tokens)
     ("gpt-5-mini"
      :input 0.25 :output 1.00 :cache-write 0.0 :cache-read 0.025)
     ("gpt-5.2-codex"
      :input 1.75 :output 14.00 :cache-write 0.0 :cache-read 0.175))
  "Pricing table: model-name -> (:input N :output N :cache-write N :cache-read N)
   All prices are USD per 1,000,000 tokens.")


(defun lookup-model-pricing (model-name)
  "Return pricing plist for MODEL-NAME, or NIL if not found.
   Falls back to a prefix match (e.g. 'claude-sonnet' matches 'claude-sonnet-4-6')."
  (or (cdr (assoc model-name *model-pricing-table* :test #'string-equal))
      ;; prefix match: find first entry whose key is a prefix of model-name
      (loop for (name . pricing) in *model-pricing-table*
            when (let ((len (length name)))
                   (and (<= len (length model-name))
                        (string-equal name model-name :end2 len)))
            return pricing)
      ;; fallback: return medium/sonnet pricing with a warning
      (progn
        (warn "No pricing found for model ~s; using sonnet-4-6 pricing as fallback." model-name)
        (cdr (assoc "claude-sonnet-4-6" *model-pricing-table* :test #'string-equal)))))


(defun estimate-cost-usd (model-name &key (input-tokens 0) (output-tokens 0)
                                          (cache-write-tokens 0) (cache-read-tokens 0))
  "Estimate cost in USD for a given model and token counts.
   Returns a plist: (:total N :input N :output N :cache-write N :cache-read N)
   All values are in USD."
  (let* ((pricing (lookup-model-pricing model-name))
         (price-input      (getf pricing :input 3.00))
         (price-output     (getf pricing :output 15.00))
         (price-cw         (getf pricing :cache-write 3.75))
         (price-cr         (getf pricing :cache-read 0.30))
         (cost-input       (* input-tokens       (/ price-input  1000000.0d0)))
         (cost-output      (* output-tokens      (/ price-output 1000000.0d0)))
         (cost-cw          (* cache-write-tokens (/ price-cw     1000000.0d0)))
         (cost-cr          (* cache-read-tokens  (/ price-cr     1000000.0d0)))
         (total            (+ cost-input cost-output cost-cw cost-cr)))
    (list :total total
          :input cost-input
          :output cost-output
          :cache-write cost-cw
          :cache-read cost-cr)))

