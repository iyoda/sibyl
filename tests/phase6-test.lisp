;;;; phase6-test.lisp — Phase 6: Japanese patterns, cost-factor fix, threshold tuning

(in-package #:sibyl.tests)

(def-suite phase6-tests
  :description "Phase 6: Japanese complexity rules, cost-factor correction, threshold tuning"
  :in sibyl-tests)

(in-suite phase6-tests)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Japanese pattern tests
;;; ─────────────────────────────────────────────────────────────────────────

(test japanese-simple-questions-are-light
  "Simple Japanese questions (no technical keywords) should be classified as light."
  (let* ((analyzer (sibyl.llm::make-task-analyzer))
         (tasks '("2+2は何ですか？"
                  "フランスの首都はどこですか？"
                  "水の沸点は何度ですか？"
                  "一週間は何日ですか？"
                  "日本の人口は何人ですか？"))
         (tiers (mapcar (lambda (task)
                          (sibyl.llm::recommended-tier
                           (sibyl.llm::analyze-task-complexity analyzer task)))
                        tasks)))
    (is (every (lambda (r) (string= r "light")) tiers)
        "Simple Japanese questions should be light, got: ~a" tiers)))

(test japanese-technical-tasks-are-medium-or-heavy
  "Japanese tasks with technical keywords should be medium or heavy."
  (let* ((analyzer (sibyl.llm::make-task-analyzer))
         (tasks '("再帰アルゴリズムを実装してください。"
                  "バイナリサーチ関数を書いてください。"
                  "SQLクエリを最適化してください。"))
         (tiers (mapcar (lambda (task)
                          (sibyl.llm::recommended-tier
                           (sibyl.llm::analyze-task-complexity analyzer task)))
                        tasks)))
    (is (every (lambda (r) (or (string= r "medium") (string= r "heavy"))) tiers)
        "Japanese technical tasks should be medium or heavy, got: ~a" tiers)))

(test japanese-complex-tasks-are-heavy
  "Japanese tasks with complex/system-level keywords should be heavy."
  (let* ((analyzer (sibyl.llm::make-task-analyzer))
         (tasks '("分散合意アルゴリズムを設計・実装してください。システム全体のアーキテクチャも含めて。"
                  "機械学習フレームワークをゼロから実装してください。複雑な最適化も含む。"))
         (tiers (mapcar (lambda (task)
                          (sibyl.llm::recommended-tier
                           (sibyl.llm::analyze-task-complexity analyzer task)))
                        tasks)))
    (is (every (lambda (r) (string= r "heavy")) tiers)
        "Complex Japanese tasks should be heavy, got: ~a" tiers)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Cost-factor correction tests
;;; ─────────────────────────────────────────────────────────────────────────

(test cost-factor-ratios-match-actual-pricing
  "Tier cost-factors should reflect actual price ratios (haiku/sonnet/opus)."
  ;; haiku input=$1, sonnet input=$3, opus input=$5
  ;; light/medium = 1/3 ≈ 0.333, heavy/medium = 5/3 ≈ 1.667
  (let* ((selector (sibyl.llm::make-model-selector))
         (light-tier  (sibyl.llm::find-tier selector "light"))
         (medium-tier (sibyl.llm::find-tier selector "medium"))
         (heavy-tier  (sibyl.llm::find-tier selector "heavy")))
    (is (not (null light-tier))  "light tier should exist")
    (is (not (null medium-tier)) "medium tier should exist")
    (is (not (null heavy-tier))  "heavy tier should exist")
    ;; light cost-factor should be <= 0.4 (actual ratio is ~0.333)
    (is (<= (sibyl.llm::tier-cost-factor light-tier) 0.4d0)
        "light cost-factor should be <= 0.4, got ~a"
        (sibyl.llm::tier-cost-factor light-tier))
    ;; heavy cost-factor should be <= 2.0 (actual ratio is ~1.667)
    (is (<= (sibyl.llm::tier-cost-factor heavy-tier) 2.0d0)
        "heavy cost-factor should be <= 2.0, got ~a"
        (sibyl.llm::tier-cost-factor heavy-tier))))

(test estimate-cost-reflects-tier-savings
  "Haiku should cost ~1/3 of sonnet for same token counts."
  (let* ((tokens 1000000)
         (haiku-cost  (getf (sibyl.llm:estimate-cost-usd
                             "claude-haiku-4-5"
                             :input-tokens tokens) :total))
         (sonnet-cost (getf (sibyl.llm:estimate-cost-usd
                             "claude-sonnet-4-6"
                             :input-tokens tokens) :total))
         (ratio (/ haiku-cost sonnet-cost)))
    (is (< (abs (- ratio (/ 1.0d0 3.0d0))) 0.01d0)
        "haiku/sonnet cost ratio should be ~1/3, got ~a" ratio)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Threshold tuning tests
;;; ─────────────────────────────────────────────────────────────────────────

(test benchmark-light-accuracy-meets-kpi
  "Light tier classification accuracy should be >= 70% on benchmark set."
  (let* ((selector (sibyl.llm::make-model-selector))
         (result (sibyl.llm::evaluate-classification-accuracy selector))
         (by-tier (getf result :by-tier))
         (light-stats (find "light" by-tier :key (lambda (x) (getf x :tier)) :test #'string=))
         (light-acc (getf light-stats :accuracy 0.0)))
    (is (>= light-acc 0.7)
        "Light tier accuracy should be >= 70%%, got ~,1f%%" (* 100 light-acc))))

(test benchmark-overall-accuracy-meets-kpi
  "Overall classification accuracy should be >= 70% on benchmark set."
  (let* ((selector (sibyl.llm::make-model-selector))
         (result (sibyl.llm::evaluate-classification-accuracy selector))
         (acc (getf result :accuracy 0.0)))
    (is (>= acc 0.7)
        "Overall accuracy should be >= 70%%, got ~,1f%%" (* 100 acc))))
