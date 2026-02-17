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
                                          :model-name "claude-sonnet-4-5-20250929"
                                          :max-tokens 8192)
                           (make-instance 'model-config
                                          :provider :openai
                                          :model-name "gpt-5-mini"
                                          :max-tokens 8192)))
   
   (make-instance 'model-tier
                  :name "heavy"
                  :description "High-capability models for complex tasks"
                  :cost-factor 3.0
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
  '(;; Code complexity indicators
    (:pattern "defclass|defmethod|defgeneric|defmacro" :weight 2 :factor "complex-lisp-constructs")
    (:pattern "lambda|mapcar|reduce|loop" :weight 1 :factor "functional-programming")
    (:pattern "eval-form|macroexpand|compile" :weight 3 :factor "meta-programming")
    
    ;; Task type indicators
    (:pattern "test|spec|unit|integration" :weight -1 :factor "testing-task")
    (:pattern "refactor|clean|format" :weight 1 :factor "refactoring-task")
    (:pattern "design|architect|plan" :weight 2 :factor "design-task")
    (:pattern "debug|fix|error|bug" :weight 2 :factor "debugging-task")
    
    ;; Scope indicators
    (:pattern "system|module|package" :weight 2 :factor "large-scope")
    (:pattern "function|method|variable" :weight 0 :factor "small-scope")
    (:pattern "entire|whole|complete|full" :weight 2 :factor "comprehensive-scope")
    
    ;; Complexity keywords
    (:pattern "complex|complicated|difficult|advanced" :weight 3 :factor "explicit-complexity")
    (:pattern "simple|easy|basic|quick" :weight -2 :factor "explicit-simplicity")
    (:pattern "optimize|performance|efficient" :weight 2 :factor "optimization-task")
    
    ;; Multi-step indicators
    (:pattern "first.*then|step.*step|phase.*phase" :weight 2 :factor "multi-step-process")
    (:pattern "analyze.*implement|design.*code" :weight 2 :factor "multi-phase-task")))


;;; ============================================================
;;; Config-driven model resolution
;;; ============================================================

(defparameter *tier-metadata*
  '(("light"  :description "Fast, lightweight models for simple tasks"
              :cost-factor 0.2  :speed-factor 3.0 :capability-score 6  :max-tokens 4096)
    ("medium" :description "Balanced models for general tasks"
              :cost-factor 1.0  :speed-factor 1.0 :capability-score 8  :max-tokens 8192)
    ("heavy"  :description "High-capability models for complex tasks"
              :cost-factor 3.0  :speed-factor 0.5 :capability-score 10 :max-tokens 16384))
  "Tier metadata (non-model properties). Model names come from config.")

(defun configured-model-name (tier-name provider)
  "Get model name for TIER-NAME and PROVIDER from config.
   Config key format: models.<tier>.<provider> (e.g. models.light.anthropic).
   Returns the configured string or NIL."
  (sibyl.config:config-value
   (format nil "models.~a.~(~a~)" tier-name provider)))

(defun build-model-tiers-from-config ()
  "Build model-tier instances from config values.
   Reads models.<tier>.<provider> keys. Falls back to *default-model-tiers*
   if no config values are found."
  (let ((any-configured nil))
    (let ((tiers
            (mapcar
             (lambda (meta)
               (let* ((name (first meta))
                      (props (rest meta))
                      (anthropic-model (configured-model-name name "anthropic"))
                      (openai-model (configured-model-name name "openai"))
                      (max-tokens (getf props :max-tokens 4096)))
                 (when (or anthropic-model openai-model)
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

;; Complexity analysis functions
(defmethod analyze-task-complexity ((analyzer task-analyzer) task-description)
  "Analyze the complexity of a task description and return complexity analysis"
  (let ((score 5.0) ; baseline complexity
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
    (t
     (error "Unsupported model provider: ~a" (model-provider model-config)))))

;; Adaptive agent with model selection
(defclass adaptive-agent (sibyl.agent:agent)
  ((model-selector :initarg :model-selector :accessor agent-model-selector 
                   :type model-selector)
   (current-model-config :initarg :current-model-config :accessor agent-current-model-config
                         :initform nil)
   (task-history :initarg :task-history :accessor agent-task-history :initform nil
                 :documentation "History of tasks and model selections")))

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
  "Run agent with automatic model adaptation"
  (multiple-value-bind (model-config tier analysis)
      (adapt-model-for-task agent input)
    
    ;; Add model selection info to the response context
    (when analysis
      (format t "~%[Model Selection: ~a (~a) - Complexity: ~,1f]~%" 
              (model-name model-config)
              (tier-name tier)
              (complexity-score analysis)))
    
    ;; Run the agent normally
    (sibyl.agent:agent-run agent input)))

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
                                          :model-name "claude-sonnet-4-5-20250929"
                                          :release-date "2025-09-29"
                                          :version "4.5"
                                          :max-tokens 16384
                                          :context-window 200000
                                          :capabilities '(:advanced-reasoning :code-generation :analysis))
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
                                          :capabilities '(:deep-reasoning :code-generation :analysis :architecture))
                           ;; GPT-5.2 (latest high-capability)
                           (make-instance 'enhanced-model-config
                                          :provider :openai
                                          :model-name "gpt-5.2"
                                          :release-date "2025-12-01"
                                          :version "5.2"
                                          :max-tokens 32768
                                          :context-window 200000
                                          :temperature 0.05
                                          :capabilities '(:advanced-reasoning :multimodal :high-precision))))))

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
