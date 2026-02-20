;;;; Intelligent model selection based on task complexity

(in-package #:sibyl.llm)

;; Model configuration
(defclass model-config ()
  ((provider :initarg :provider :accessor model-provider :type keyword
             :documentation "Provider: :anthropic or :openai")
   (model-name :initarg :model-name :accessor model-name :type string)
   (max-tokens :initarg :max-tokens :accessor model-max-tokens :type integer :initform 4096)
   (temperature :initarg :temperature :accessor model-temperature :type number :initform 0.0)
   (supports-tools :initarg :supports-tools :accessor model-supports-tools :type boolean :initform t)))


;; Task complexity analyzer
(defclass task-analyzer ()
  ((complexity-rules :initarg :complexity-rules :accessor analyzer-rules :initform nil
                     :documentation "List of complexity analysis rules")))


;; Complexity analysis result
(defclass complexity-analysis ()
  ((score :initarg :score :accessor complexity-score :type number
          :documentation "Complexity score (1-10 scale)")
   (factors :initarg :factors :accessor complexity-factors :type list
            :documentation "List of complexity factors found")
   (reasoning :initarg :reasoning :accessor complexity-reasoning :type string
              :documentation "Human-readable explanation of complexity assessment")
   (recommended-tier :initarg :recommended-tier :accessor recommended-tier :type string)))


;; Default complexity analysis rules
(defparameter *default-complexity-rules*
  '(;; ── English patterns ──────────────────────────────────────────────────
    (:pattern "defclass|defmethod|defgeneric|defmacro" :weight 2 :factor "complex-lisp-constructs")
    (:pattern "lambda|mapcar|reduce|loop" :weight 1 :factor "functional-programming")
    (:pattern "eval-form|macroexpand|compile" :weight 3 :factor "meta-programming")
    (:pattern "test|spec|unit|integration" :weight -1 :factor "testing-task")
    (:pattern "refactor|clean|format" :weight 1 :factor "refactoring-task")
    (:pattern "design|architect|plan" :weight 2 :factor "design-task")
    (:pattern "debug|fix|error|bug" :weight 2 :factor "debugging-task")
    (:pattern "system|module|package" :weight 2 :factor "large-scope")
    (:pattern "function|method|variable" :weight 1.5 :factor "small-scope")
    (:pattern "entire|whole|complete|full" :weight 2 :factor "comprehensive-scope")
    (:pattern "complex|complicated|difficult|advanced" :weight 3 :factor "explicit-complexity")
    (:pattern "simple|easy|basic|quick" :weight -2 :factor "explicit-simplicity")
    (:pattern "optimize|performance|efficient" :weight 2 :factor "optimization-task")
    (:pattern "first.*then|step.*step|phase.*phase" :weight 2 :factor "multi-step-process")
    (:pattern "analyze.*implement|design.*code" :weight 2 :factor "multi-phase-task")
    (:pattern "implement|write a|create a|build a" :weight 1.5 :factor "implementation-task")
    (:pattern "explain|describe|difference|compare" :weight 1.5 :factor "explanation-task")
    (:pattern "algorithm|data structure|protocol" :weight 2 :factor "cs-concept")
    ;; ── Japanese patterns ─────────────────────────────────────────────────
    ;; Light-tier signals: simple question words
    (:pattern "何ですか|どこですか|いつですか|誰ですか|なぜですか|どうですか" :weight -1 :factor "ja-simple-question")
    (:pattern "何人|何日|何度|何時|何年|何月" :weight -1 :factor "ja-factual-question")
    ;; Medium-tier signals: implementation / technical keywords
    (:pattern "実装|実装して|実装する" :weight 2 :factor "ja-implementation")
    (:pattern "アルゴリズム|データ構造|プロトコル" :weight 2 :factor "ja-cs-concept")
    (:pattern "関数|メソッド|クラス|モジュール" :weight 1.5 :factor "ja-code-element")
    (:pattern "デバッグ|バグ|エラー|修正" :weight 2 :factor "ja-debugging")
    (:pattern "最適化|パフォーマンス|効率" :weight 2 :factor "ja-optimization")
    (:pattern "テスト|スペック|ユニット" :weight -1 :factor "ja-testing")
    (:pattern "書いて|作って|作成して|構築して" :weight 1.5 :factor "ja-creation-task")
    (:pattern "説明|比較|違い|解説" :weight 1.5 :factor "ja-explanation")
    ;; Heavy-tier signals: design / system / complex
    (:pattern "設計|アーキテクチャ|システム全体" :weight 2.5 :factor "ja-design-task")
    (:pattern "複雑|高度|難しい|困難" :weight 3 :factor "ja-explicit-complexity")
    (:pattern "分散|合意|フレームワーク|ゼロから" :weight 3 :factor "ja-advanced-concept")
    (:pattern "機械学習|深層学習|ニューラル" :weight 3 :factor "ja-ml-task")
    ;; Japanese multi-step connectors
    (:pattern "そして|また|さらに|加えて|含めて" :weight 0.5 :factor "ja-complex-logic"))
  "Default complexity rules for task analysis.
   Includes both English and Japanese patterns.
   Weights: positive = more complex, negative = simpler.")


;; Constructor functions
(defun make-task-analyzer (&optional (rules *default-complexity-rules*))
  "Create a task analyzer with complexity rules"
  (make-instance 'task-analyzer :complexity-rules rules))


;; Complexity analysis functions
(defmethod analyze-task-complexity ((analyzer task-analyzer) task-description)
  "Analyze the complexity of a task description and return complexity analysis.
   Baseline score is 3.0 (lowered from 5.0) to improve light-tier classification accuracy."
  (let ((score 3.0) ; baseline complexity (lowered from 5.0 to improve light-tier accuracy)
        (factors nil)
        (reasoning-parts nil))
    
    ;; Apply complexity rules
    (dolist (rule (analyzer-rules analyzer))
      (let ((pattern (getf rule :pattern))
            (weight (getf rule :weight))
            (factor (getf rule :factor)))
        (when (cl-ppcre:scan (format nil "(?i)~a" pattern) task-description)
          (incf score weight)
          (push factor factors)
          (push (format nil "~a (weight: ~@d)" factor weight) reasoning-parts))))
    
    ;; Additional heuristics
    (let ((word-count (length (cl-ppcre:split "\\s+" task-description))))
      (when (> word-count 50)
        (incf score 1)
        (push "long-description" factors)
        (push "Long task description (+1)" reasoning-parts)))
    
    (when (cl-ppcre:scan "(?i)\\b(and|or|but|however|also|additionally)\\b" task-description)
      (incf score 0.5)
      (push "complex-logic" factors)
      (push "Complex logical structure (+0.5)" reasoning-parts))
    
    ;; Clamp score to 1-10 range
    (setf score (max 1.0 (min 10.0 score)))
    
    ;; Determine recommended tier
    (let ((recommended-tier
           (cond
             ((<= score 3.5) "light")
             ((<= score 7.0) "medium")
             (t "heavy"))))
      
      (make-instance 'complexity-analysis
                     :score score
                     :factors (reverse factors)
                     :reasoning (format nil "Complexity score: ~,1f~%Factors: ~{~a~^, ~}~%Reasoning: ~{~a~^; ~}"
                                        score factors (reverse reasoning-parts))
                     :recommended-tier recommended-tier))))


;; Enhanced model configuration with release date
(defclass enhanced-model-config (model-config)
  ((release-date :initarg :release-date :accessor model-release-date :type string
                 :documentation "Model release date in YYYY-MM-DD format")
   (version :initarg :version :accessor model-version :type string :initform "1.0"
            :documentation "Model version string")
   (context-window :initarg :context-window :accessor model-context-window :type integer
                   :initform 128000 :documentation "Maximum context window size")
   (capabilities :initarg :capabilities :accessor model-capabilities :type list
                 :initform nil :documentation "List of special capabilities")))

;; Flat model registry (as of 2026-02)
(defparameter *model-registry*
  (list
   ;; Light tier models - Fast, cost-effective
   (make-instance 'enhanced-model-config
                  :provider :anthropic
                  :model-name "claude-haiku-4-5-20251015"
                  :release-date "2025-10-15"
                  :version "4.5"
                  :max-tokens 8192
                  :context-window 200000
                  :capabilities '(:fast-reasoning :code-generation))
   (make-instance 'enhanced-model-config
                  :provider :openai
                  :model-name "gpt-5-nano"
                  :release-date "2025-12-01"
                  :version "5-nano"
                  :max-tokens 8192
                  :context-window 200000
                  :capabilities '(:fast-inference :code-generation))
   
   ;; Medium tier models - Balanced performance
   (make-instance 'enhanced-model-config
                  :provider :anthropic
                  :model-name "claude-sonnet-4-6"
                  :release-date "2025-09-29"
                  :version "4.5"
                  :max-tokens 16384
                  :context-window 200000
                  :capabilities '(:advanced-reasoning :code-generation :analysis :thinking))
   (make-instance 'enhanced-model-config
                  :provider :openai
                  :model-name "gpt-5-mini"
                  :release-date "2025-12-01"
                  :version "5-mini"
                  :max-tokens 16384
                  :context-window 200000
                  :capabilities '(:advanced-reasoning :multimodal))
   
   ;; Heavy tier models - Maximum capability
   (make-instance 'enhanced-model-config
                  :provider :anthropic
                  :model-name "claude-opus-4-6"
                  :release-date "2026-02-05"
                  :version "4.6"
                  :max-tokens 32768
                  :context-window 200000
                  :temperature 0.1
                  :capabilities '(:deep-reasoning :code-generation :analysis :architecture :thinking))
   ;; Note: gpt-5.2-codex must come before gpt-5.2 for correct prefix matching
   (make-instance 'enhanced-model-config
                  :provider :openai
                  :model-name "gpt-5.2-codex"
                  :release-date "2026-01-15"
                  :version "5.2-codex"
                  :max-tokens 32768
                  :context-window 400000
                  :temperature 0.05
                  :capabilities '(:advanced-reasoning :multimodal :agentic-coding))
   (make-instance 'enhanced-model-config
                  :provider :openai
                  :model-name "gpt-5.2"
                  :release-date "2025-12-01"
                  :version "5.2"
                  :max-tokens 32768
                  :context-window 200000
                  :temperature 0.05
                  :capabilities '(:advanced-reasoning :multimodal :high-precision))
   (make-instance 'enhanced-model-config
                  :provider :ollama
                  :model-name "gpt-oss:120b"
                  :release-date "2025-01-01"
                  :version "120b"
                  :max-tokens 8192
                  :context-window 131072
                  :temperature 1.0
                  :capabilities '(:deep-reasoning :code-generation :thinking :tool-calling)))
  "Flat registry of all available models with metadata.")


;;; ============================================================
;;; Context window lookup utility
;;; ============================================================

(defun context-window-for-model (model-name)
  "Return context window size for MODEL-NAME. Falls back to 200000 for unknown models.
   Looks up model in *model-registry* by prefix match."
  (let ((model-name-lower (string-downcase model-name)))
    (block search
      ;; Search all models for a match
      (dolist (config *model-registry*)
        (let ((config-name (string-downcase (model-name config))))
          ;; Prefix match: model-name starts with config-name or vice versa
          (when (or (alexandria:starts-with-subseq config-name model-name-lower)
                    (alexandria:starts-with-subseq model-name-lower config-name))
            (return-from search (model-context-window config)))))
      ;; Fallback for unknown models
      200000)))

;;; ============================================================
;;; Client factory from model-config
;;; ============================================================

(defun create-client-for-model (config)
  "Create an LLM client from a MODEL-CONFIG instance.
Dispatches on the provider slot (:anthropic, :openai, :ollama)."
  (ecase (model-provider config)
    (:anthropic (make-anthropic-client :model (model-name config)
                                      :max-tokens (model-max-tokens config)
                                      :temperature (model-temperature config)))
    (:openai    (make-openai-client :model (model-name config)
                                   :max-tokens (model-max-tokens config)
                                   :temperature (model-temperature config)))
    (:ollama    (make-ollama-client :model (model-name config)
                                   :max-tokens (model-max-tokens config)
                                   :temperature (model-temperature config)))))
