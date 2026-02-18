;;;; ollama.lisp — Ollama LLM provider implementation
;;;; Local Ollama server client using the /api/chat endpoint.

(in-package #:sibyl.llm)

;;; ============================================================
;;; Ollama client
;;; ============================================================

(defclass ollama-client (llm-client)
  ()
  (:default-initargs
   :base-url "http://localhost:11434"
   :model "glm-4.7-flash:q8_0"
   :api-key "")
  (:documentation "Client for the local Ollama inference server."))

(defparameter *ollama-model-profiles*
  '(("qwen3-coder"
     :temperature 0.6
     :top-p 0.95
     :top-k 20
     :repeat-penalty 1.05
     :min-p 0.0
     :num-ctx 32768
     :num-predict 8192
     :thinking t                ; supports <think>...</think> blocks
     :system-prompt-hint "You are a coding assistant. Use <think>...</think> for internal reasoning when the task is complex. Be precise and concise."
     :description "Alibaba Qwen3-Coder: strong code generation with thinking support")

    ("glm-4"
     :temperature 0.1
     :top-p 0.7
     :top-k 40
     :repeat-penalty 1.1
     :min-p 0.0
     :num-ctx 32768
     :num-predict 8192
     :thinking nil
     :system-prompt-hint "You are a coding assistant. Be precise, concise, and follow instructions exactly."
     :description "THUDM GLM-4: fast bilingual model optimized for instruction following")

    ("deepseek-r1"
     :temperature 0.6
     :top-p 0.95
     :top-k 40
     :repeat-penalty 1.0
     :min-p 0.0
     :num-ctx 32768
     :num-predict 8192
     :thinking t
     :system-prompt-hint "You are a coding assistant with strong reasoning abilities."
     :description "DeepSeek-R1: reasoning-focused model with thinking support")

    ("llama3"
     :temperature 0.2
     :top-p 0.9
     :top-k 40
     :repeat-penalty 1.1
     :min-p 0.0
     :num-ctx 8192
     :num-predict 4096
     :thinking nil
     :system-prompt-hint nil
     :description "Meta Llama 3: general-purpose instruction model")

    ("gpt-oss"
     :temperature 1.0              ; model default per `ollama show`
     :top-p 0.9
     :top-k 40
     :repeat-penalty 1.05
     :min-p 0.0
     :num-ctx 131072               ; 128K context window
     :num-predict 8192
     :thinking t                   ; supports thinking (via native API)
     :think-api :think             ; use Ollama native think API ("think": true in request)
                                   ; response returns thinking in message.thinking field
     :large-model t                ; signals special timeout/keep-alive handling
     :load-timeout 600             ; 10 min timeout for initial model load (116.8B MXFP4)
     :system-prompt-hint "You are a coding assistant with strong reasoning abilities. Be precise and concise."
     :description "GPT-OSS 116.8B (MXFP4): large model with native thinking + tool-calling, 128K context")

    ;; Default fallback profile (MUST be last)
    ("*default*"
     :temperature 0.0
     :top-p 0.9
     :top-k 40
     :repeat-penalty 1.1
     :min-p 0.0
     :num-ctx 8192
     :num-predict 8192
     :thinking nil
     :system-prompt-hint nil
     :description "Default fallback profile for unknown models"))
  "Per-model-family optimization profiles for Ollama.
Each entry is (model-family-prefix . plist) where the prefix is matched
against the beginning of the model name (e.g. \"qwen3-coder\" matches
\"qwen3-coder:30b\" and \"qwen3-coder:latest\").
The \"*default*\" entry is used as fallback for unrecognized models.")

(defun lookup-model-profile (model-name)
  "Find the best matching model profile for MODEL-NAME.
Matches by prefix: 'qwen3-coder:30b' matches the 'qwen3-coder' profile.
Falls back to '*default*' if no prefix matches."
  (or (cdr (find-if (lambda (entry)
                       (let ((prefix (car entry)))
                         (and (not (string= prefix "*default*"))
                              (>= (length model-name) (length prefix))
                              (string= prefix (subseq model-name 0 (length prefix))))))
                     *ollama-model-profiles*))
      ;; Fallback to default
      (cdr (find "*default*" *ollama-model-profiles* :key #'car :test #'string=))))

(defun make-ollama-client (&key (model "glm-4.7-flash:q8_0")
                                (host nil)
                                (max-tokens nil)
                                (temperature nil))
  "Create an Ollama API client.
HOST overrides the server URL (default: http://localhost:11434).
Falls back to config key llm.ollama.host when HOST is nil.
When TEMPERATURE or MAX-TOKENS are not specified, uses model profile defaults."
  (log-debug "llm" "Creating Ollama client (model: ~a)" model)
  (let* ((profile (lookup-model-profile model))
         (effective-max-tokens
           (or max-tokens
               (let ((cfg (config-value "ollama.num-predict")))
                 (when cfg (if (stringp cfg) (parse-integer cfg :junk-allowed t) cfg)))
               (getf profile :num-predict)
               8192))
         (effective-temperature
           (if temperature
               temperature
               (or (getf profile :temperature) 0.0))))
    (make-instance 'ollama-client
                   :model model
                   :api-key ""
                   :max-tokens effective-max-tokens
                   :temperature effective-temperature
                   :base-url (or host
                                 (config-value "llm.ollama.host")
                                 "http://localhost:11434"))))

;;; ============================================================
;;; URL helper
;;; ============================================================

(defun ollama-api-url (client path)
  "Build a full Ollama API URL by appending PATH to the client's base URL."
  (concatenate 'string (client-base-url client) path))

;;; ============================================================
;;; Capability detection
;;; ============================================================

(defun ollama-model-info (client)
  "Query Ollama /api/show to get model metadata.
Returns a hash-table with model info, or NIL on failure.
Non-blocking: errors are caught and logged."
  (handler-case
      (let ((body `(("model" . ,(client-model client)))))
        (http-post-json (ollama-api-url client "/api/show") '() body))
    (error (e)
      (log-debug "llm" "Ollama /api/show failed (non-fatal): ~a" e)
      nil)))

(defun detect-model-capabilities (client)
  "Detect model capabilities from /api/show response.
Returns a plist of detected capabilities:
  :context-length — model's context window size (integer or NIL)
  :family — model family string (e.g. \"qwen3\")
  :supports-tools — T if model template indicates tool support
  :parameter-size — parameter size string (e.g. \"30B\")"
  (let ((info (ollama-model-info client)))
    (when info
      (let* ((model-info-ht (gethash "model_info" info))
             (details (gethash "details" info))
             (params (gethash "parameters" info))
             (template (gethash "template" info)))
        (declare (ignore params))
        (list
         :context-length (when model-info-ht
                           ;; Try common keys for context length
                           (or (gethash "context_length" model-info-ht)
                               ;; Some models store it with the architecture prefix
                               (loop for key being the hash-keys of model-info-ht
                                     using (hash-value val)
                                     when (search "context_length" key)
                                       return val)))
         :family (when details (gethash "family" details))
         :supports-tools (when template
                           (or (search "tools" template)
                               (search "tool_call" template)
                               (search ".Tools" template))
                           t)
         :parameter-size (when details (gethash "parameter_size" details)))))))

;;; ============================================================
;;; Server state & health API
;;; ============================================================

(defun ollama-server-reachable-p (client)
  "Return T if the Ollama server responds to a basic GET /api/tags request.
Useful as a pre-flight check before inference or pre-warming.
Uses a short 5-second connect timeout."
  (handler-case
      (progn
        (dex:get (ollama-api-url client "/api/tags")
                 :connect-timeout 5
                 :read-timeout 5)
        t)
    (error ()
      nil)))

(defun ollama-running-models (client)
  "Query Ollama GET /api/ps to list models currently loaded in VRAM.
Returns a list of alists, each with keys:
  \"name\"      — model name string
  \"size\"      — total size in bytes
  \"size_vram\" — VRAM size in bytes
  \"expires_at\" — expiration timestamp string
Returns NIL on failure (non-fatal)."
  (handler-case
      (let* ((response-body (dex:get (ollama-api-url client "/api/ps")
                                     :connect-timeout 5
                                     :read-timeout 10))
             (parsed (yason:parse response-body :object-as :hash-table))
             (raw-models (gethash "models" parsed)))
        (when raw-models
          (let ((models (if (vectorp raw-models)
                            (coerce raw-models 'list)
                            raw-models)))
            (mapcar (lambda (m)
                      (list (cons "name"       (gethash "name" m))
                            (cons "size"       (gethash "size" m))
                            (cons "size_vram"  (gethash "size_vram" m))
                            (cons "expires_at" (gethash "expires_at" m))))
                    models))))
    (error (e)
      (log-debug "llm" "Ollama /api/ps failed (non-fatal): ~a" e)
      nil)))

(defun ollama-model-loaded-p (client &optional (model-name nil))
  "Return T if MODEL-NAME (or client's model) is currently loaded in VRAM.
Uses /api/ps to check running models without triggering a model load."
  (let* ((target (or model-name (client-model client)))
         (models (ollama-running-models client)))
    (when models
      (some (lambda (m)
              (let ((name (cdr (assoc "name" m :test #'string=))))
                (and name (string= name target))))
            models))))

(defun ollama-model-exists-p (client &optional (model-name nil))
  "Return T if MODEL-NAME (or client's model) is available locally.
Uses /api/show which returns metadata WITHOUT loading the model into VRAM."
  (handler-case
      (let* ((target (or model-name (client-model client)))
             (body `(("model" . ,target)))
             (response (http-post-json (ollama-api-url client "/api/show")
                                       '() body
                                       :connect-timeout 5
                                       :read-timeout 10)))
        (declare (ignore response))
        t)
    (error ()
      nil)))

(defun ollama-health-check (client)
  "Comprehensive health check for Ollama server + model availability.
Returns a plist:
  :server-up   — T if Ollama server responds
  :model-exists — T if the configured model is available locally
  :model-loaded — T if the model is currently in VRAM
  :capabilities — plist from detect-model-capabilities, or NIL"
  (let ((server-up (ollama-server-reachable-p client)))
    (if (not server-up)
        (list :server-up nil :model-exists nil :model-loaded nil :capabilities nil)
        (let ((model-exists (ollama-model-exists-p client))
              (model-loaded (ollama-model-loaded-p client))
              (caps (when (ollama-model-exists-p client)
                      (detect-model-capabilities client))))
          (list :server-up t
                :model-exists model-exists
                :model-loaded model-loaded
                :capabilities caps)))))

;;; ============================================================
;;; Message format conversion
;;; ============================================================

(defun ollama-content-blocks->text (blocks)
  "Extract text from a list of content-block alists or hash-tables.
Returns an empty string when no text blocks are present."
  (let ((parts nil))
    (dolist (block blocks)
      (cond
        ((hash-table-p block)
         (when (string= (gethash "type" block) "text")
           (let ((text (gethash "text" block)))
             (when text (push text parts)))))
        ((and (listp block) (not (null block)))
         (let ((type (cdr (assoc "type" block :test #'string=))))
           (when (and type (string= type "text"))
             (let ((text (cdr (assoc "text" block :test #'string=))))
               (when text (push text parts))))))
        ((stringp block)
         (push block parts))))
    (if parts
        (string-join "" (nreverse parts))
        "")))

(defun ollama-normalize-content (content)
  "Normalize message CONTENT for Ollama, which expects a string." 
  (cond
    ((stringp content) content)
    ((null content) "")
    ((listp content) (ollama-content-blocks->text content))
    (t (prin1-to-string content))))

(defun ollama-recover-tool-name (id)
  "Recover tool name from a synthetic Ollama tool-call ID.
ID format: 'ollama-{tool-name}-{counter}'
Example: 'ollama-get_weather-0' => 'get_weather'"
  (if (and (>= (length id) 7)
           (string= "ollama-" (subseq id 0 7)))
      (let* ((without-prefix (subseq id 7))
             (dash-pos (position #\- without-prefix :from-end t)))
        (if dash-pos
            (subseq without-prefix 0 dash-pos)
            without-prefix))
      id))

(defun messages-to-ollama-format (messages)
  "Convert message structs to Ollama /api/chat message format."
  (mapcar
   (lambda (msg)
     (case (message-role msg)
       (:system
        `(("role"    . "system")
          ("content" . ,(ollama-normalize-content (message-content msg)))))
       (:user
        `(("role"    . "user")
          ("content" . ,(ollama-normalize-content (message-content msg)))))
       (:assistant
        (if (message-tool-calls msg)
            ;; Assistant with tool calls: content must be empty string
            `(("role"       . "assistant")
              ("content"    . "")
              ("tool_calls" . ,(mapcar
                                (lambda (tc)
                                  `(("function"
                                     . (("name"      . ,(tool-call-name tc))
                                        ("arguments" . ,(or (tool-call-arguments tc)
                                                            (make-hash-table
                                                             :test 'equal)))))))
                                (message-tool-calls msg))))
            ;; Plain assistant message
            `(("role"    . "assistant")
              ("content" . ,(ollama-normalize-content
                             (or (message-content msg) ""))))))
       (:tool
        ;; Tool result: recover tool name from synthetic ID
        `(("role"      . "tool")
          ("content"   . ,(ollama-normalize-content (message-content msg)))
          ("tool_name" . ,(ollama-recover-tool-name
                           (or (message-tool-call-id msg) "")))))))
   messages))

;;; ============================================================
;;; Tool schema conversion
;;; ============================================================

(defun tools-to-ollama-format (tools)
  "Convert tool schemas to Ollama function-call format."
  (mapcar (lambda (ts)
            `(("type" . "function")
              ("function"
               . (("name"        . ,(getf ts :name))
                  ("description" . ,(getf ts :description))
                  ("parameters"  . ,(getf ts :parameters))))))
          tools))

;;; ============================================================
;;; Response parsing
;;; ============================================================

(defun parse-ollama-tool-calls (raw-tool-calls)
  "Parse tool_calls array from an Ollama response.
RAW-TOOL-CALLS is a vector or list as parsed from JSON.
Returns a list of tool-call structs with synthetic IDs.

IMPORTANT: Ollama delivers arguments as a hash-table (already
parsed from JSON), NOT a JSON string — do not double-parse."
  (let ((tcs (if (vectorp raw-tool-calls)
                 (coerce raw-tool-calls 'list)
                 raw-tool-calls))
        (counter 0))
    (mapcar (lambda (tc)
              (let* ((func     (gethash "function" tc))
                     (name     (gethash "name" func))
                     (raw-args (gethash "arguments" func))
                     ;; Convert hash-table to alist; already parsed, not a JSON string.
                     (args     (cond
                                 ((hash-table-p raw-args) (hash-to-alist raw-args))
                                 ((null raw-args)         nil)
                                 (t                       raw-args)))
                     (id       (format nil "ollama-~a-~a"
                                       name (prog1 counter (incf counter)))))
                (make-tool-call :id id :name name :arguments args)))
            tcs)))

(defun parse-ollama-response (response)
  "Parse a non-streaming Ollama /api/chat response.
Returns (values message usage-plist).
Supports two thinking modes:
  1. Ollama native: model returns thinking in message.thinking field
     (used by GLM-4 with \"think\": true in request)
  2. Inline tags: model embeds <think>...</think> in message.content
     (used by DeepSeek-R1, Qwen3 base)"
  (let* ((msg           (gethash "message" response))
         (raw-content   (gethash "content" msg))
         (native-thinking (gethash "thinking" msg))  ; Ollama native think API
         (raw-tcs       (gethash "tool_calls" msg))
         (tool-calls    (when raw-tcs
                          (parse-ollama-tool-calls raw-tcs)))
         (prompt-tokens (gethash "prompt_eval_count" response))
         (output-tokens (gethash "eval_count" response))
         ;; Duration metrics (nanoseconds from Ollama)
         (load-duration-ns  (gethash "load_duration" response))
         (total-duration-ns (gethash "total_duration" response))
         (eval-duration-ns  (gethash "eval_duration" response))
         (done-reason       (gethash "done_reason" response))
         (usage-plist   (list :input-tokens  (or prompt-tokens 0)
                              :output-tokens (or output-tokens 0)
                              :load-duration-ms (when load-duration-ns
                                                  (round load-duration-ns 1000000))
                              :total-duration-ms (when total-duration-ns
                                                   (round total-duration-ns 1000000))
                              :eval-duration-ms (when eval-duration-ns
                                                  (round eval-duration-ns 1000000)))))
    ;; Log load duration — detect cold loads (>1s load time)
    (when (and load-duration-ns (> load-duration-ns 1000000000))
      (log-warn "llm" "Ollama cold load detected: ~,1fs (total: ~,1fs, reason: ~a)"
                (/ load-duration-ns 1.0d9)
                (if total-duration-ns (/ total-duration-ns 1.0d9) 0)
                (or done-reason "unknown")))
    ;; Two thinking paths:
    ;; 1. If native thinking field is present (Ollama think API), use it directly
    ;; 2. Otherwise, extract inline <think> tags from content
    (if (and native-thinking (stringp native-thinking) (not (string= native-thinking "")))
        ;; Native thinking: content is clean, thinking is separate
        (values
         (assistant-message (if (and raw-content (stringp raw-content)
                                     (not (string= raw-content "")))
                                raw-content
                                nil)
                            :tool-calls tool-calls
                            :thinking native-thinking)
         usage-plist)
        ;; Inline thinking: extract <think> blocks from content
        (multiple-value-bind (content thinking)
            (if (and raw-content (not (string= raw-content "")))
                (extract-thinking-blocks raw-content)
                (values nil nil))
          (values
           (assistant-message (if (and content (not (string= content "")))
                                  content
                                  nil)
                              :tool-calls tool-calls
                              :thinking thinking)
           usage-plist)))))

;;; ============================================================
;;; Request body builder (shared by streaming and non-streaming)
;;; ============================================================

(defun %ollama-keep-alive (&optional model-name)
  "Return the keep_alive value for a model.
Large models (profile :large-model t) default to -1 (never unload)
to avoid costly reload cycles. Small models default to 30m.
Config key ollama.keep-alive overrides the profile-based default."
  (or (config-value "ollama.keep-alive")
      (when model-name
        (let ((profile (lookup-model-profile model-name)))
          (when (getf profile :large-model)
            -1)))                          ; never unload large models
      "30m"))

(defun %parse-number (val)
  "Parse VAL as a number. Handles strings (both integer and float),
integers, and floats. Returns a number or NIL."
  (cond
    ((numberp val) val)
    ((stringp val)
     (let ((trimmed (string-trim '(#\Space #\Tab) val)))
       (or (ignore-errors (parse-integer trimmed))
           (ignore-errors (read-from-string trimmed)))))
    (t nil)))

(defun %ollama-extra-options (&optional profile)
  "Build an alist of Ollama inference parameters.
Merges model-profile defaults with config overrides.
Config values always take precedence over profile defaults.
Supports both integer (num_ctx) and float (top_p) parameter types."
  (let ((opts nil))
    (flet ((add-option (config-key ollama-key profile-key)
             (let ((config-val (config-value config-key))
                   (profile-val (when (and profile profile-key)
                                  (getf profile profile-key))))
               (let ((val (or config-val profile-val)))
                 (when val
                   (let ((n (%parse-number val)))
                     (when (and n (numberp n))
                       (push (cons ollama-key n) opts))))))))
      (add-option "ollama.num-ctx"        "num_ctx"        :num-ctx)
      (add-option "ollama.num-batch"      "num_batch"      nil)
      (add-option "ollama.num-gpu"        "num_gpu"        nil)
      (add-option "ollama.num-thread"     "num_thread"     nil)
      ;; Sampling parameters from profiles
      (add-option "ollama.top-p"          "top_p"          :top-p)
      (add-option "ollama.top-k"          "top_k"          :top-k)
      (add-option "ollama.repeat-penalty" "repeat_penalty" :repeat-penalty)
      (add-option "ollama.min-p"          "min_p"          :min-p))
    opts))

(defun build-ollama-request (client messages &key tools stream)
  "Build the request body alist for POST /api/chat.
STREAM T => JSON true; STREAM NIL => JSON false (yason:false).
Includes keep_alive to prevent model unloading, model profile
inference parameters, and optional think API field for models
that support Ollama's native thinking (e.g. GLM-4)."
  (let* ((profile (lookup-model-profile (client-model client)))
         (api-messages (messages-to-ollama-format messages))
         (base-options `(("temperature" . ,(client-temperature client))
                         ("num_predict" . ,(client-max-tokens client))))
         (extra-options (%ollama-extra-options profile))
         (all-options (append base-options extra-options))
          (body `(("model"      . ,(client-model client))
                  ("messages"   . ,api-messages)
                  ("stream"     . ,(if stream t yason:false))
                  ("keep_alive" . ,(%ollama-keep-alive (client-model client)))
                  ("options"    . ,all-options))))
    ;; Add think API field for models that use Ollama's native thinking
    ;; (e.g. GLM-4 uses top-level "think": true/false in the request body,
    ;;  NOT the options sub-object).
    (when (eq (getf profile :think-api) :think)
      (push `("think" . t) body))
    (if tools
        (append body `(("tools" . ,(tools-to-ollama-format tools))))
        body)))

;;; ============================================================
;;; Thinking block extraction
;;; ============================================================

(defun extract-thinking-blocks (text)
  "Extract <think>...</think> blocks from TEXT.
Returns (values clean-text thinking-text) where:
  - clean-text has thinking blocks removed
  - thinking-text is the concatenated content of all thinking blocks, or NIL"
  (if (or (null text) (string= text ""))
      (values text nil)
      (let ((thinking-parts nil)
            (clean-parts nil)
            (pos 0)
            (len (length text)))
        (loop
          (let ((start (search "<think>" text :start2 pos)))
            (if start
                (progn
                  ;; Add text before <think>
                  (when (> start pos)
                    (push (subseq text pos start) clean-parts))
                  (let ((end (search "</think>" text :start2 (+ start 7))))
                    (if end
                        (progn
                          (push (subseq text (+ start 7) end) thinking-parts)
                          (setf pos (+ end 8)))
                        ;; Unclosed <think> tag — treat rest as thinking
                        (progn
                          (push (subseq text (+ start 7)) thinking-parts)
                          (return)))))
                (progn
                  ;; No more <think> tags
                  (when (< pos len)
                    (push (subseq text pos) clean-parts))
                  (return)))))
        (values
         (let ((clean (format nil "~{~a~}" (nreverse clean-parts))))
           (string-trim '(#\Space #\Newline #\Tab) clean))
         (when thinking-parts
           (format nil "~{~a~}" (nreverse thinking-parts)))))))

;;; ============================================================
;;; Streaming implementation (NDJSON)
;;; ============================================================

(defun complete-ollama-streaming (client messages &key tools)
  "Stream Ollama NDJSON response, invoking *streaming-text-callback* per chunk.
Returns (values message usage-plist). Synchronous: completes when stream ends.
Supports both native thinking (message.thinking field from Ollama think API)
and inline <think> tags in content."
  (let ((content-chunks '())
        (thinking-chunks '())           ; native thinking from Ollama think API
        (tool-calls-raw nil)
        (final-usage    nil)
        (url            (ollama-api-url client "/api/chat"))
        (body           (build-ollama-request client messages
                                              :tools tools :stream t)))
    (http-post-ndjson-stream
     url
     '()                                ; Ollama is local — no auth headers
     body
     ;; on-chunk: called for each NDJSON line
     (lambda (chunk)
        (let* ((msg-obj  (gethash "message" chunk))
               (text     (and msg-obj (gethash "content" msg-obj)))
               (thinking (and msg-obj (gethash "thinking" msg-obj)))
               (done     (gethash "done" chunk)))
          ;; Collect native thinking tokens (Ollama think API)
          (when (and thinking (stringp thinking) (not (string= thinking "")))
            (push thinking thinking-chunks))
          ;; Collect and stream content tokens
          (when (and text (not (string= text "")))
            (push text content-chunks)
            (when *streaming-text-callback*
              (funcall *streaming-text-callback* text)))
          (when done
            ;; Tool calls arrive in the final (done=true) chunk
            (let ((tc (and msg-obj (gethash "tool_calls" msg-obj))))
              (when tc (setf tool-calls-raw tc)))
            ;; Extract duration metrics from final chunk
            (let ((load-ns (gethash "load_duration" chunk))
                  (total-ns (gethash "total_duration" chunk))
                  (eval-ns (gethash "eval_duration" chunk)))
              (when (and load-ns (> load-ns 1000000000))
                (log-warn "llm" "Ollama streaming cold load: ~,1fs"
                          (/ load-ns 1.0d9)))
              (setf final-usage
                    (list :input-tokens  (or (gethash "prompt_eval_count" chunk) 0)
                          :output-tokens (or (gethash "eval_count" chunk) 0)
                          :load-duration-ms (when load-ns
                                              (round load-ns 1000000))
                          :total-duration-ms (when total-ns
                                               (round total-ns 1000000))
                          :eval-duration-ms (when eval-ns
                                              (round eval-ns 1000000))))))))
     ;; on-done: http-post-ndjson-stream is synchronous; fires last
     (lambda () nil)
     :read-timeout (%ollama-read-timeout client))
    ;; After stream ends — assemble thinking and content
    (let* ((has-native-thinking (not (null thinking-chunks)))
           (full-content (format nil "~{~a~}" (nreverse content-chunks)))
           (native-thinking (when has-native-thinking
                              (format nil "~{~a~}" (nreverse thinking-chunks)))))
      (if has-native-thinking
          ;; Native thinking: content is clean, thinking collected separately
          (let ((tcs (when tool-calls-raw
                       (parse-ollama-tool-calls tool-calls-raw))))
            (values (assistant-message (if (string= full-content "")
                                          nil
                                          full-content)
                                      :tool-calls tcs
                                      :thinking native-thinking)
                    final-usage))
          ;; Inline thinking: extract <think> blocks from content
          (multiple-value-bind (content thinking)
              (if (string= full-content "")
                  (values nil nil)
                  (extract-thinking-blocks full-content))
            (let ((tcs (when tool-calls-raw
                         (parse-ollama-tool-calls tool-calls-raw))))
              (values (assistant-message (if (and content (not (string= content "")))
                                            content
                                            nil)
                                        :tool-calls tcs
                                        :thinking thinking)
                      final-usage)))))))

;;; ============================================================
;;; CLOS methods
;;; ============================================================

(defun %ollama-read-timeout (client)
  "Compute read timeout for an Ollama request.
Large models (profile :large-model t) use :load-timeout from profile;
others use the global default."
  (let* ((profile (lookup-model-profile (client-model client)))
         (large-p (getf profile :large-model))
         (load-timeout (getf profile :load-timeout)))
    (if (and large-p load-timeout)
        load-timeout
        nil)))                            ; nil → use global default

(defmethod complete ((client ollama-client) messages &key)
  "Send messages to the local Ollama server.
Returns (values message usage-plist).
Uses retry with exponential backoff for transient failures."
  (log-info "llm" "Ollama complete (model: ~a, streaming: ~a)"
            (client-model client)
            (if *streaming-text-callback* "yes" "no"))
  (if *streaming-text-callback*
      (complete-ollama-streaming client messages)
      (call-with-retry
       (lambda ()
         (let* ((body     (build-ollama-request client messages :stream nil))
                (response (http-post-json
                           (ollama-api-url client "/api/chat")
                           '()                 ; no auth headers
                           body
                           :read-timeout (%ollama-read-timeout client))))
           (parse-ollama-response response)))
       :description (format nil "Ollama /api/chat (~a)" (client-model client)))))

(defmethod complete-with-tools ((client ollama-client) messages tools &key)
  "Send messages with tool schemas to the local Ollama server.
Returns (values message usage-plist).
Uses retry with exponential backoff for transient failures."
  (log-info "llm" "Ollama complete-with-tools (model: ~a, tools: ~a, streaming: ~a)"
            (client-model client)
            (length tools)
            (if *streaming-text-callback* "yes" "no"))
  (if *streaming-text-callback*
      (complete-ollama-streaming client messages :tools tools)
      (call-with-retry
       (lambda ()
         (let* ((body     (build-ollama-request client messages
                                                :tools tools :stream nil))
                (response (http-post-json
                           (ollama-api-url client "/api/chat")
                           '()                 ; no auth headers
                           body
                           :read-timeout (%ollama-read-timeout client))))
           (parse-ollama-response response)))
       :description (format nil "Ollama /api/chat+tools (~a)" (client-model client)))))

(defmethod count-tokens ((client ollama-client) text)
  "Rough token estimate for Ollama models: ~4 characters per token."
  (ceiling (length text) 4))

;;; ============================================================
;;; Pre-warm: load model into VRAM before first real request
;;; ============================================================

(defun ollama-pre-warm (client)
  "Send a minimal request to load the model into VRAM.
Returns T on success, NIL on failure (non-fatal).
Designed to be called in a background thread at REPL startup.
Uses the Ollama empty-messages preload pattern (done_reason: load)."
  (handler-case
      (let* ((profile (lookup-model-profile (client-model client)))
             (large-p (getf profile :large-model))
             (load-timeout (or (getf profile :load-timeout) 300))
             ;; Use empty messages: Ollama treats this as a preload-only request
             ;; Returns immediately with done_reason="load" after model is in VRAM
             (body `(("model"      . ,(client-model client))
                     ("messages"   . ())
                     ("stream"     . ,yason:false)
                     ("keep_alive" . ,(%ollama-keep-alive (client-model client))))))
        (when large-p
          (log-info "llm" "Pre-warming large model ~a (timeout: ~as)..."
                    (client-model client) load-timeout))
        (http-post-json (ollama-api-url client "/api/chat") '() body
                        :read-timeout load-timeout)
        (log-info "llm" "Ollama model ~a pre-warmed successfully"
                  (client-model client))
        t)
    (error (e)
      (log-debug "llm" "Ollama pre-warm failed (non-fatal): ~a" e)
      nil)))

(defun ollama-ensure-warm (client &key (timeout 600) (poll-interval 5)
                                       (on-progress nil))
  "Block until the model is loaded in VRAM, polling /api/ps.
Triggers a preload if model is not already loaded.
TIMEOUT — maximum seconds to wait (default 600 = 10 minutes).
POLL-INTERVAL — seconds between /api/ps checks (default 5).
ON-PROGRESS — optional callback (lambda (elapsed-seconds loaded-p) ...)
              called each poll cycle for progress reporting.
Returns T if model is warm, NIL if timeout exceeded."
  (let ((model (client-model client)))
    ;; Already loaded? Return immediately.
    (when (ollama-model-loaded-p client)
      (log-info "llm" "Model ~a already loaded in VRAM" model)
      (when on-progress (funcall on-progress 0 t))
      (return-from ollama-ensure-warm t))
    ;; Server reachable?
    (unless (ollama-server-reachable-p client)
      (log-error "llm" "Ollama server not reachable at ~a" (client-base-url client))
      (return-from ollama-ensure-warm nil))
    ;; Trigger preload in a separate thread so we can poll /api/ps
    (log-info "llm" "Triggering preload for model ~a..." model)
    (let ((preload-thread
            (bt:make-thread
             (lambda () (ollama-pre-warm client))
             :name "ollama-preload")))
      (declare (ignore preload-thread))
      ;; Poll /api/ps until model appears or timeout
      (let ((start (get-internal-real-time)))
        (loop
          (let* ((now (get-internal-real-time))
                 (elapsed (/ (- now start) internal-time-units-per-second)))
            (when (> elapsed timeout)
              (log-warn "llm" "Model ~a preload timed out after ~as" model elapsed)
              (when on-progress (funcall on-progress elapsed nil))
              (return nil))
            (when (ollama-model-loaded-p client)
              (log-info "llm" "Model ~a loaded in VRAM (~,1fs)" model elapsed)
              (when on-progress (funcall on-progress elapsed t))
              (return t))
            (when on-progress (funcall on-progress elapsed nil))
            (sleep poll-interval)))))))
