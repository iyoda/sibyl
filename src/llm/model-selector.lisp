;;;; Intelligent model selection based on task complexity

(in-package #:sibyl.llm)

;; Model capability tiers
(defclass model-tier ()
  ((name :initarg :name :accessor tier-name :type string)
   (description :initarg :description :accessor tier-description :type string)
   (models :initarg :models :accessor tier-models :type list
           :documentation "List of model configurations for this tier")
   (cost-factor :initarg :cost-factor :accessor tier-cost-factor :type number
                :documentation "Relative cost factor (1.0 = baseline)")
   (speed-factor :initarg :speed-factor :accessor tier-speed-factor :type number
                 :documentation "Relative speed factor (1.0 = baseline)")
   (capability-score :initarg :capability-score :accessor tier-capability-score :type number
                     :documentation "Capability score (1-10 scale)")))


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


;; Model selector
(defclass model-selector ()
  ((tiers :initarg :tiers :accessor selector-tiers :type list)
   (analyzer :initarg :analyzer :accessor selector-analyzer :type task-analyzer)
   (current-tier :initarg :current-tier :accessor selector-current-tier :initform "medium"
                 :documentation "Currently selected tier name")
   (auto-select :initarg :auto-select :accessor selector-auto-select :type boolean :initform t
                :documentation "Whether to automatically select models based on task complexity")))


;; Predefined model tiers
(defparameter *default-model-tiers*
  (list
   (make-instance 'model-tier
                  :name "light"
                  :description "Fast, lightweight models for simple tasks"
                  :cost-factor 0.2
                  :speed-factor 3.0
                  :capability-score 6
                   :models (list
                           (make-instance 'model-config
                                          :provider :anthropic
                                          :model-name "claude-haiku-4-5-20251015"
                                          :max-tokens 4096)
                           (make-instance 'model-config
                                          :provider :openai
                                          :model-name "gpt-5-nano"
                                          :max-tokens 4096)))

   (make-instance 'model-tier
                  :name "medium"
                  :description "Balanced models for general tasks"
                  :cost-factor 1.0
                  :speed-factor 1.0
                  :capability-score 8
                   :models (list
                           (make-instance 'model-config
                                          :provider :anthropic
                                          :model-name "claude-sonnet-4-6"
                                          :max-tokens 8192)
                           (make-instance 'model-config
                                          :provider :openai
                                          :model-name "gpt-5-mini"
                                          :max-tokens 8192)))

   (make-instance 'model-tier
                  :name "heavy"
                  :description "High-capability models for complex tasks"
                  ;; cost-factor reflects actual price ratio: opus($5)/sonnet($3) ≈ 1.667
                  :cost-factor 1.667
                  :speed-factor 0.5
                  :capability-score 10
                  :models (list
                           (make-instance 'model-config
                                          :provider :anthropic
                                          :model-name "claude-opus-4-6"
                                          :max-tokens 16384)
                           (make-instance 'model-config
                                          :provider :openai
                                          :model-name "gpt-5.2"
                                          :max-tokens 16384
                                          :temperature 0.1)))))


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


;;; ============================================================
;;; Config-driven model resolution
;;; ============================================================

(defparameter *tier-metadata*
  '(("light"  :description "Fast, lightweight models for simple tasks"
              :cost-factor 0.2   :speed-factor 3.0 :capability-score 6  :max-tokens 4096)
    ("medium" :description "Balanced models for general tasks"
              :cost-factor 1.0   :speed-factor 1.0 :capability-score 8  :max-tokens 8192)
    ("heavy"  :description "High-capability models for complex tasks"
              :cost-factor 1.667 :speed-factor 0.5 :capability-score 10 :max-tokens 16384))
  "Tier metadata (non-model properties). Model names come from config.
   Cost-factors reflect actual API price ratios (haiku=$1, sonnet=$3, opus=$5):
     light/medium ≈ 0.333, heavy/medium ≈ 1.667")

(defun configured-model-name (tier-name provider)
  "Get model name for TIER-NAME and PROVIDER from config.
   Config key format: models.<tier>.<provider> (e.g. models.light.anthropic).
   Returns the configured string or NIL."
  (sibyl.config:config-value
   (format nil "models.~a.~(~a~)" tier-name provider)))

(defun build-model-tiers-from-config ()
  "Build model-tier instances from config values.
   Reads models.<tier>.<provider> keys (anthropic, openai, ollama).
   Falls back to *default-model-tiers* if no config values are found."
  (let ((any-configured nil))
    (let ((tiers
            (mapcar
             (lambda (meta)
               (let* ((name (first meta))
                      (props (rest meta))
                      (anthropic-model (configured-model-name name "anthropic"))
                      (openai-model (configured-model-name name "openai"))
                      (ollama-model (configured-model-name name "ollama"))
                      (max-tokens (getf props :max-tokens 4096)))
                 (when (or anthropic-model openai-model ollama-model)
                   (setf any-configured t))
                 (make-instance 'model-tier
                                :name name
                                :description (getf props :description)
                                :cost-factor (getf props :cost-factor 1.0)
                                :speed-factor (getf props :speed-factor 1.0)
                                :capability-score (getf props :capability-score 5)
                                :models (remove nil
                                                (list
                                                 (when anthropic-model
                                                   (make-instance 'model-config
                                                                  :provider :anthropic
                                                                  :model-name anthropic-model
                                                                  :max-tokens max-tokens))
                                                 (when openai-model
                                                   (make-instance 'model-config
                                                                  :provider :openai
                                                                  :model-name openai-model
                                                                  :max-tokens max-tokens))
                                                 (when ollama-model
                                                   (make-instance 'model-config
                                                                  :provider :ollama
                                                                  :model-name ollama-model
                                                                  :max-tokens max-tokens)))))))
             *tier-metadata*)))
      (if any-configured tiers *default-model-tiers*))))

;; Constructor functions
(defun make-task-analyzer (&optional (rules *default-complexity-rules*))
  "Create a task analyzer with complexity rules"
  (make-instance 'task-analyzer :complexity-rules rules))


(defun make-model-selector (&key (tiers nil tiers-supplied-p)
                                  (analyzer (make-task-analyzer))
                                  (auto-select t))
  "Create a model selector. Tiers are resolved from config by default.
   Config keys: models.light.anthropic, models.medium.openai, etc."
  (make-instance 'model-selector 
                 :tiers (if tiers-supplied-p tiers (build-model-tiers-from-config))
                 :analyzer analyzer 
                 :auto-select auto-select))

(defun make-default-model-selector ()
  "Create a model selector with default tiers and complexity analyzer.
   Convenience constructor for use in start-repl and tests."
  (make-model-selector))

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

;; Dispatch analyze-task-complexity on model-selector (delegates to its analyzer)
(defmethod analyze-task-complexity ((selector model-selector) task-description)
  "Analyze task complexity using the model selector's built-in task analyzer."
  (analyze-task-complexity (selector-analyzer selector) task-description))

;; Model selection functions
(defmethod find-tier ((selector model-selector) tier-name)
  "Find a model tier by name"
  (find tier-name (selector-tiers selector) :key #'tier-name :test #'string-equal))

(defmethod select-model-for-task ((selector model-selector) task-description &key force-tier)
  "Select the best model for a given task"
  (let* ((analysis (when (selector-auto-select selector)
                     (analyze-task-complexity (selector-analyzer selector) task-description)))
         (target-tier (or force-tier
                          (when analysis (recommended-tier analysis))
                          (selector-current-tier selector)))
         (tier (find-tier selector target-tier)))
    
    (unless tier
      (warn "Tier '~a' not found, falling back to 'medium'" target-tier)
      (setf tier (find-tier selector "medium")))
    
    (let* ((preferred-provider (or (sibyl.config:config-value "llm.preferred-provider") :anthropic))
           (model-config (or (find preferred-provider (tier-models tier) :key #'model-provider)
                             (first (tier-models tier)))))
      
      (values model-config tier analysis))))

(defmethod create-client-for-model ((model-config model-config))
  "Create an LLM client for the specified model configuration"
  (case (model-provider model-config)
    (:anthropic
     (make-anthropic-client 
      :api-key (sibyl.config:config-value "llm.anthropic.api-key")
      :model (model-name model-config)
      :max-tokens (model-max-tokens model-config)
      :temperature (model-temperature model-config)))
    (:openai
     (make-openai-client
      :api-key (sibyl.config:config-value "llm.openai.api-key")
      :model (model-name model-config)
      :max-tokens (model-max-tokens model-config)
      :temperature (model-temperature model-config)))
    (:ollama
     (make-ollama-client
      :model (model-name model-config)
      :host (sibyl.config:config-value "llm.ollama.host")
      :max-tokens (model-max-tokens model-config)
      :temperature (model-temperature model-config)))
    (t
     (error "Unsupported model provider: ~a" (model-provider model-config)))))

;; Adaptive agent with model selection
(defclass adaptive-agent (sibyl.agent:agent)
  ((model-selector
    :initarg :model-selector
    :accessor agent-model-selector
    :type model-selector)
   (current-model-config
    :initarg :current-model-config
    :accessor agent-current-model-config
    :initform nil)
   (task-history
    :initarg :task-history
    :accessor agent-task-history
    :initform nil
    :documentation "History of tasks and model selections")
   (cost-records
    :initarg :cost-records
    :accessor agent-cost-records
    :initform nil
    :documentation "List of task-cost-record structs accumulated during this session")))

(defmethod make-adaptive-agent (&key (model-selector (make-model-selector))
                                     (name "Adaptive-Sibyl")
                                     (system-prompt sibyl.agent::*default-system-prompt*)
                                     (max-steps 50)
                                     (max-memory-messages 100))
  "Create an adaptive agent that selects models based on task complexity"
  (let ((agent (make-instance 'adaptive-agent
                              :model-selector model-selector
                              :name name
                              :system-prompt system-prompt
                              :max-steps max-steps
                              :memory (sibyl.agent:make-memory :max-messages max-memory-messages))))
    ;; Initialize with medium tier model
    (multiple-value-bind (model-config tier)
        (select-model-for-task model-selector "general task" :force-tier "medium")
      (setf (agent-current-model-config agent) model-config)
      (setf (sibyl.agent:agent-client agent) (create-client-for-model model-config)))
    agent))

(defmethod adapt-model-for-task ((agent adaptive-agent) task-description)
  "Adapt the agent's model based on task complexity"
  (multiple-value-bind (model-config tier analysis)
      (select-model-for-task (agent-model-selector agent) task-description)
    
    ;; Update client if model changed
    (unless (and (agent-current-model-config agent)
                 (string= (model-name model-config) 
                          (model-name (agent-current-model-config agent))))
      (setf (agent-current-model-config agent) model-config)
      (setf (sibyl.agent:agent-client agent) (create-client-for-model model-config))
      
      ;; Log the model change
      (push (list :timestamp (get-universal-time)
                  :task task-description
                  :model (model-name model-config)
                  :tier (tier-name tier)
                  :complexity-score (when analysis (complexity-score analysis)))
            (agent-task-history agent)))
    
    (values model-config tier analysis)))

(defmethod agent-run-adaptive ((agent adaptive-agent) input)
  "Run agent with automatic model adaptation and cost tracking.
   Snapshots the token tracker before the run, then records a task-cost-record
   (from sibyl.llm/metrics.lisp) capturing the delta tokens and estimated USD cost."
  (multiple-value-bind (model-config tier analysis)
      (adapt-model-for-task agent input)
    (when analysis
      (format t "~%[Model Selection: ~a (~a) - Complexity: ~,1f]~%"
              (model-name model-config) (tier-name tier)
              (complexity-score analysis)))
    (let* ((tracker (sibyl.agent:agent-token-tracker agent))
           (before  (when tracker (snapshot-tracker tracker)))
           (result  (sibyl.agent:agent-run agent input)))
      (when (and tracker before)
        (let* ((delta  (tracker-delta before tracker))
               (record (make-task-cost-record-from-delta
                        input
                        (model-name model-config)
                        (tier-name tier)
                        (when analysis (complexity-score analysis))
                        delta)))
          (push record (agent-cost-records agent))))
      result)))

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

;; Latest model configurations (as of 2026-02)
(defparameter *latest-model-tiers*
  (list
   ;; Light tier - Fast, cost-effective models
   (make-instance 'model-tier
                  :name "light"
                  :description "Latest lightweight models for simple tasks"
                  :cost-factor 0.15
                  :speed-factor 4.0
                  :capability-score 7
                  :models (list
                           ;; Claude Haiku 4.5 (latest Haiku)
                           (make-instance 'enhanced-model-config
                                          :provider :anthropic
                                          :model-name "claude-haiku-4-5-20251015"
                                          :release-date "2025-10-15"
                                          :version "4.5"
                                          :max-tokens 8192
                                          :context-window 200000
                                          :capabilities '(:fast-reasoning :code-generation))
                           ;; GPT-5 Nano
                           (make-instance 'enhanced-model-config
                                          :provider :openai
                                          :model-name "gpt-5-nano"
                                          :release-date "2025-12-01"
                                          :version "5-nano"
                                          :max-tokens 8192
                                          :context-window 200000
                                          :capabilities '(:fast-inference :code-generation))))
   
   ;; Medium tier - Balanced performance
   (make-instance 'model-tier
                  :name "medium"
                  :description "Latest balanced models for general tasks"
                  :cost-factor 1.0
                  :speed-factor 1.5
                  :capability-score 9
                  :models (list
                           ;; Claude Sonnet 4.5 (latest Sonnet)
                            (make-instance 'enhanced-model-config
                                           :provider :anthropic
                                           :model-name "claude-sonnet-4-6"
                                           :release-date "2025-09-29"
                                           :version "4.5"
                                           :max-tokens 16384
                                           :context-window 200000
                                           :capabilities '(:advanced-reasoning :code-generation :analysis :thinking))
                           ;; GPT-5 Mini (latest balanced)
                           (make-instance 'enhanced-model-config
                                          :provider :openai
                                          :model-name "gpt-5-mini"
                                          :release-date "2025-12-01"
                                          :version "5-mini"
                                          :max-tokens 16384
                                          :context-window 200000
                                          :capabilities '(:advanced-reasoning :multimodal))))
   
   ;; Heavy tier - Maximum capability models
   (make-instance 'model-tier
                  :name "heavy"
                  :description "Latest high-capability models for complex tasks"
                  :cost-factor 2.5
                  :speed-factor 0.8
                  :capability-score 10
                  :models (list
                           ;; Claude Opus 4.6 (latest Opus)
                            (make-instance 'enhanced-model-config
                                           :provider :anthropic
                                           :model-name "claude-opus-4-6"
                                           :release-date "2026-02-05"
                                           :version "4.6"
                                           :max-tokens 32768
                                           :context-window 200000
                                           :temperature 0.1
                                           :capabilities '(:deep-reasoning :code-generation :analysis :architecture :thinking))
                           ;; GPT-5.2 (latest high-capability)
                           (make-instance 'enhanced-model-config
                                          :provider :openai
                                          :model-name "gpt-5.2"
                                          :release-date "2025-12-01"
                                          :version "5.2"
                                          :max-tokens 32768
                                          :context-window 200000
                                          :temperature 0.05
                                          :capabilities '(:advanced-reasoning :multimodal :high-precision))
                           ;; GPT-OSS 120B (local Ollama, 116.8B MXFP4)
                           (make-instance 'enhanced-model-config
                                          :provider :ollama
                                          :model-name "gpt-oss:120b"
                                          :release-date "2025-01-01"
                                          :version "120b"
                                          :max-tokens 8192
                                          :context-window 131072
                                          :temperature 1.0
                                          :capabilities '(:deep-reasoning :code-generation :thinking :tool-calling))))))

;; Model selection with latest model preference
(defmethod select-latest-model-for-task ((selector model-selector) task-description &key force-tier prefer-provider)
  "Select the latest and most suitable model for a given task"
  (let* ((analysis (when (selector-auto-select selector)
                     (analyze-task-complexity (selector-analyzer selector) task-description)))
         (target-tier (or force-tier
                          (when analysis (recommended-tier analysis))
                          (selector-current-tier selector)))
         (tier (find-tier selector target-tier)))
    
    (unless tier
      (warn "Tier '~a' not found, falling back to 'medium'" target-tier)
      (setf tier (find-tier selector "medium")))
    
    ;; Sort models by release date (newest first) and capabilities
    (let* ((available-models (tier-models tier))
           (sorted-models 
            (sort (copy-list available-models)
                  (lambda (a b)
                    (cond
                      ;; First priority: prefer specified provider
                      ((and prefer-provider 
                            (eq (model-provider a) prefer-provider)
                            (not (eq (model-provider b) prefer-provider)))
                       t)
                      ((and prefer-provider 
                            (eq (model-provider b) prefer-provider)
                            (not (eq (model-provider a) prefer-provider)))
                       nil)
                      ;; Second priority: newer release date
                      ((and (typep a 'enhanced-model-config)
                            (typep b 'enhanced-model-config))
                       (string> (model-release-date a) (model-release-date b)))
                      ;; Third priority: enhanced models over basic ones
                      ((and (typep a 'enhanced-model-config)
                            (not (typep b 'enhanced-model-config)))
                       t)
                      ((and (typep b 'enhanced-model-config)
                            (not (typep a 'enhanced-model-config)))
                       nil)
                      ;; Default: keep original order
                      (t nil)))))
           (selected-model (first sorted-models)))
      
      (values selected-model tier analysis))))

;; Enhanced model selector with latest model preference
(defclass latest-model-selector (model-selector)
  ((prefer-latest :initarg :prefer-latest :accessor selector-prefer-latest :type boolean :initform t
                  :documentation "Whether to prefer latest models")
   (provider-preference :initarg :provider-preference :accessor selector-provider-preference 
                        :initform nil :documentation "Preferred provider (:anthropic or :openai)")
   (capability-requirements :initarg :capability-requirements :accessor selector-capability-requirements
                            :initform nil :documentation "Required capabilities for model selection")))

(defmethod select-model-for-task ((selector latest-model-selector) task-description &key force-tier)
  "Enhanced model selection with latest model preference"
  (if (selector-prefer-latest selector)
      (select-latest-model-for-task selector task-description 
                                    :force-tier force-tier
                                    :prefer-provider (selector-provider-preference selector))
      ;; Fall back to standard selection
      (call-next-method)))

;; Constructor for latest model selector
(defun make-latest-model-selector (&key (tiers nil tiers-supplied-p) 
                                        (analyzer (make-task-analyzer))
                                        (auto-select t)
                                        (prefer-latest t)
                                        (provider-preference nil))
  "Create a model selector that prefers latest models.
   Tiers are resolved from config by default."
  (make-instance 'latest-model-selector 
                 :tiers (if tiers-supplied-p tiers (build-model-tiers-from-config))
                 :analyzer analyzer 
                 :auto-select auto-select
                 :prefer-latest prefer-latest
                 :provider-preference provider-preference))

;;; ============================================================
;;; Benchmark task set and classification accuracy evaluation
;;; ============================================================

(defvar *benchmark-task-set*
  '(;; Light tasks (10)
    (:task "What is 2 + 2?" :expected-tier "light")
    (:task "What is the capital of France?" :expected-tier "light")
    (:task "Convert 100 Celsius to Fahrenheit." :expected-tier "light")
    (:task "What does HTTP stand for?" :expected-tier "light")
    (:task "List the primary colors." :expected-tier "light")
    (:task "What is the boiling point of water?" :expected-tier "light")
    (:task "How many days are in a week?" :expected-tier "light")
    (:task "What is the square root of 16?" :expected-tier "light")
    (:task "Name the continents of the world." :expected-tier "light")
    (:task "What language is spoken in Brazil?" :expected-tier "light")
    ;; Medium tasks (10)
    (:task "Explain the concept of recursion in programming with an example." :expected-tier "medium")
    (:task "Write a function to reverse a string in Python." :expected-tier "medium")
    (:task "Describe the differences between SQL and NoSQL databases." :expected-tier "medium")
    (:task "Implement a binary search algorithm." :expected-tier "medium")
    (:task "Explain how garbage collection works in modern languages." :expected-tier "medium")
    (:task "Write a REST API endpoint for user authentication in Node.js." :expected-tier "medium")
    (:task "Explain the difference between threads and processes." :expected-tier "medium")
    (:task "Implement a simple linked list in Java." :expected-tier "medium")
    (:task "Describe the MVC design pattern with an example." :expected-tier "medium")
    (:task "Write a SQL query to find duplicate records in a table." :expected-tier "medium")
    ;; Heavy tasks (10)
    (:task "Design and implement a distributed consensus algorithm with Byzantine fault tolerance using Paxos. Include defclass defmethod defgeneric for the protocol." :expected-tier "heavy")
    (:task "Implement a full compiler pipeline with lexer, parser, AST, semantic analysis, and code generation using defmacro and eval-form." :expected-tier "heavy")
    (:task "Design a microservices architecture with service mesh, circuit breakers, and distributed tracing. Use defclass defmethod defgeneric macroexpand." :expected-tier "heavy")
    (:task "Implement a machine learning framework from scratch with backpropagation, gradient descent, and neural network layers using lambda mapcar reduce." :expected-tier "heavy")
    (:task "Build a real-time collaborative editor with operational transformation, conflict resolution, and CRDT data structures using eval-form compile." :expected-tier "heavy")
    (:task "Design a distributed database with ACID transactions, MVCC, and WAL logging. Implement using defclass defmethod defgeneric with complex lambda expressions." :expected-tier "heavy")
    (:task "Implement a JIT compiler with IR generation, optimization passes, and native code emission using macroexpand eval-form compile defmacro." :expected-tier "heavy")
    (:task "Build a Kubernetes-like container orchestration system with scheduling, health checks, and auto-scaling using defclass defmethod defgeneric." :expected-tier "heavy")
    (:task "Design a cryptographic protocol with zero-knowledge proofs, homomorphic encryption, and secure multi-party computation using defclass defmethod." :expected-tier "heavy")
    (:task "Implement a reactive programming framework with observables, schedulers, and backpressure handling using lambda mapcar reduce defmacro eval-form." :expected-tier "heavy"))
  "Benchmark task set for evaluating model tier classification accuracy.
Each entry is a plist with :task (description string) and :expected-tier (string).
Contains 30 tasks: 10 light, 10 medium, 10 heavy.")

(defun evaluate-classification-accuracy
    (selector &key (task-set *benchmark-task-set*) (verbose nil))
  "Evaluate the classification accuracy of SELECTOR against TASK-SET.
TASK-SET is a list of plists with :task and :expected-tier keys.
Returns a plist with:
  :accuracy    - fraction of correct classifications (0.0 to 1.0)
  :correct     - number of correct classifications
  :total       - total number of tasks
  :results     - list of per-task result plists
  :by-tier     - accuracy broken down by tier"
  (let ((results '())
        (correct 0)
        (total 0))
    (dolist (entry task-set)
      (let* ((task-desc (getf entry :task))
             (expected (getf entry :expected-tier))
             (analysis (analyze-task-complexity selector task-desc))
             (predicted (recommended-tier analysis))
             (correctp (string= predicted expected)))
        (when correctp (incf correct))
        (incf total)
        (push (list :task task-desc
                    :expected expected
                    :predicted predicted
                    :correct correctp
                    :score (complexity-score analysis))
              results)
        (when verbose
          (format t "~&[~a] ~a -> expected=~a predicted=~a~%"
                  (if correctp "OK" "FAIL")
                  (subseq task-desc 0 (min 50 (length task-desc)))
                  expected predicted))))
    ;; Compute per-tier accuracy
    (let ((by-tier '()))
      (dolist (tier '("light" "medium" "heavy"))
        (let* ((tier-tasks (remove-if-not (lambda (r) (string= (getf r :expected) tier)) results))
               (tier-correct (count-if (lambda (r) (getf r :correct)) tier-tasks))
               (tier-total (length tier-tasks)))
          (push (list :tier tier
                      :correct tier-correct
                      :total tier-total
                      :accuracy (if (zerop tier-total) 0.0
                                    (/ (float tier-correct) tier-total)))
                by-tier)))
      (list :accuracy (if (zerop total) 0.0 (/ (float correct) total))
            :correct correct
            :total total
            :results (nreverse results)
            :by-tier (nreverse by-tier)))))
