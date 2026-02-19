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
    ;; Baseline model for savings calculation (medium tier default)
    ("__baseline__"
     :input 3.00 :output 15.00 :cache-write 3.75 :cache-read 0.30))
  "Pricing table: model-name -> (:input N :output N :cache-write N :cache-read N)
   All prices are USD per 1,000,000 tokens.")


(defun lookup-model-pricing (model-name)
  "Return pricing plist for MODEL-NAME, or NIL if not found.
   Falls back to a prefix match (e.g. 'claude-sonnet' matches 'claude-sonnet-4-6')."
  (or (cdr (assoc model-name *model-pricing-table* :test #'string-equal))
      ;; prefix match: find first entry whose key is a prefix of model-name
      (loop for (name . pricing) in *model-pricing-table*
            when (and (not (string= name "__baseline__"))
                      (let ((len (length name)))
                        (and (<= len (length model-name))
                             (string-equal name model-name :end2 len))))
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


(defun estimate-baseline-cost-usd (&key (input-tokens 0) (output-tokens 0)
                                        (cache-write-tokens 0) (cache-read-tokens 0))
  "Estimate what the cost WOULD HAVE BEEN if we had used the baseline model (medium/sonnet).
   Used to compute savings from tier switching."
  (estimate-cost-usd "__baseline__"
                     :input-tokens input-tokens
                     :output-tokens output-tokens
                     :cache-write-tokens cache-write-tokens
                     :cache-read-tokens cache-read-tokens))


(defun compute-savings-pct (actual-cost baseline-cost)
  "Compute percentage savings: (baseline - actual) / baseline * 100.
   Returns 0.0 if baseline is zero."
  (if (zerop baseline-cost)
      0.0d0
      (* 100.0d0 (/ (- baseline-cost actual-cost) baseline-cost))))

(defun decompose-savings (model-name
                          &key (input-tokens 0) (output-tokens 0)
                               (cache-write-tokens 0) (cache-read-tokens 0))
  "Decompose total savings into tier-savings and cache-savings components.

   tier-savings  = savings attributable to using a cheaper/more-expensive tier
                   compared to the baseline (medium/sonnet).
                   Computed on non-cached tokens only.

   cache-savings = savings attributable to cache hits (cache-read is cheaper
                   than regular input).  Negative if cache-write overhead
                   exceeds cache-read savings.

   Returns a plist:
     :tier-savings-usd   N
     :cache-savings-usd  N
     :total-savings-usd  N   (= tier + cache)
     :tier-savings-pct   N   (% of baseline)
     :cache-savings-pct  N   (% of baseline)"
  (let* ((actual-pricing   (lookup-model-pricing model-name))
         (baseline-pricing (lookup-model-pricing "__baseline__"))
         ;; Per-token prices (USD / 1M tokens → USD / token)
         (a-in  (/ (getf actual-pricing   :input  3.00) 1000000.0d0))
         (a-out (/ (getf actual-pricing   :output 15.00) 1000000.0d0))
         (a-cw  (/ (getf actual-pricing   :cache-write 3.75) 1000000.0d0))
         (a-cr  (/ (getf actual-pricing   :cache-read  0.30) 1000000.0d0))
         (b-in  (/ (getf baseline-pricing :input  3.00) 1000000.0d0))
         (b-out (/ (getf baseline-pricing :output 15.00) 1000000.0d0))
         (b-cw  (/ (getf baseline-pricing :cache-write 3.75) 1000000.0d0))
         (b-cr  (/ (getf baseline-pricing :cache-read  0.30) 1000000.0d0))
         ;; Tier savings: difference in price × non-cached tokens
         ;; (cache tokens are handled separately)
         (tier-savings-input  (* (- b-in  a-in)  input-tokens))
         (tier-savings-output (* (- b-out a-out) output-tokens))
         (tier-savings-cw     (* (- b-cw  a-cw)  cache-write-tokens))
         (tier-savings-cr     (* (- b-cr  a-cr)  cache-read-tokens))
         (tier-savings        (+ tier-savings-input tier-savings-output
                                 tier-savings-cw tier-savings-cr))
         ;; Cache savings: using cache-read instead of regular input
         ;;   benefit  = (a-in - a-cr) * cache-read-tokens   (cheaper reads)
         ;;   overhead = (a-cw - a-in) * cache-write-tokens  (more expensive writes)
         (cache-benefit   (* (- a-in a-cr) cache-read-tokens))
         (cache-overhead  (* (- a-cw a-in) cache-write-tokens))
         (cache-savings   (- cache-benefit cache-overhead))
         ;; Baseline total (for percentage calculation)
         (baseline-total  (+ (* b-in  input-tokens)
                             (* b-out output-tokens)
                             (* b-cw  cache-write-tokens)
                             (* b-cr  cache-read-tokens)))
         (total-savings   (+ tier-savings cache-savings))
         (tier-pct  (compute-savings-pct (- baseline-total tier-savings) baseline-total))
         (cache-pct (compute-savings-pct (- baseline-total cache-savings) baseline-total)))
    (list :tier-savings-usd  (coerce tier-savings  'double-float)
          :cache-savings-usd (coerce cache-savings 'double-float)
          :total-savings-usd (coerce total-savings 'double-float)
          :tier-savings-pct  (coerce tier-pct  'double-float)
          :cache-savings-pct (coerce cache-pct 'double-float))))
