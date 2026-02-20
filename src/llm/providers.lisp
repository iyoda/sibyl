;;;; providers.lisp — LLM provider implementations
;;;; Concrete clients for Anthropic and OpenAI APIs.

(in-package #:sibyl.llm)

;;; ============================================================
;;; Anthropic Claude
;;; ============================================================

(defparameter *oauth-token-prefix* "sk-ant-oat01-"
  "Prefix identifying OAuth access tokens from Claude Code.")

(defparameter *oauth-beta-flag* "oauth-2025-04-20"
  "Anthropic beta flag required for OAuth authentication.")


(defparameter *token-efficient-beta-flag*
  "token-efficient-tools-2025-02-19"
  "Anthropic beta flag for token-efficient tool use.
   Reduces token usage for tool calls by ~14% on average.
   See https://docs.anthropic.com/en/docs/agents-and-tools/tool-use/token-efficient-tool-use")

(defclass anthropic-client (llm-client)
  ((api-version :initarg :api-version
                :accessor anthropic-api-version
                :initform "2023-06-01"
                :type string)
   (oauth-p     :initarg :oauth-p
                :accessor anthropic-oauth-p
                :initform nil
                :type boolean
                :documentation "T when using OAuth Bearer authentication."))
  (:default-initargs
   :base-url "https://api.anthropic.com/v1"
   :model "claude-opus-4-6")
  (:documentation "Client for Anthropic's Claude API."))

(defun oauth-token-p (api-key)
  "Return T if API-KEY is an OAuth access token."
  (and (stringp api-key)
       (>= (length api-key) (length *oauth-token-prefix*))
       (string= *oauth-token-prefix*
                (subseq api-key 0 (length *oauth-token-prefix*)))))

(defun make-anthropic-client (&key api-key
                                (model "claude-opus-4-6")
                                (max-tokens 8192)
                                (temperature 0.1))
  "Create an Anthropic API client.
   Automatically detects OAuth tokens (sk-ant-oat01-...) and configures
   Bearer authentication with the required beta flags."
  (log-debug "llm" "Creating Anthropic client (model: ~a)" model)
  (let ((key (or api-key
                 (config-value "llm.anthropic.api-key")
                 (error 'config-missing-key-error
                         :key "llm.anthropic.api-key"))))
    (make-instance 'anthropic-client
                   :api-key key
                   :oauth-p (oauth-token-p key)
                   :model model
                   :max-tokens max-tokens
                   :temperature temperature)))

(defun anthropic-headers (client)
  "Build Anthropic-specific headers.
   Uses Bearer auth for OAuth tokens, x-api-key for standard API keys.
   Always includes token-efficient-tools beta flag to reduce tool-use token costs."
  (let ((headers (list (cons "anthropic-version" (anthropic-api-version client))
                       (cons "User-Agent" "sibyl/0.1.0"))))
    (if (anthropic-oauth-p client)
        (cons (cons "Authorization"
                    (format nil "Bearer ~a" (client-api-key client)))
              (cons (cons "anthropic-beta"
                          (format nil "~a,~a"
                                  *oauth-beta-flag*
                                  *token-efficient-beta-flag*))
                    headers))
        (cons (cons "x-api-key" (client-api-key client))
              (cons (cons "anthropic-beta" *token-efficient-beta-flag*)
                    headers)))))

(defun anthropic-content-blocks-p (content)
  "Return T when CONTENT is a list of Anthropic content-block alists."
  (and (listp content)
       (every (lambda (block)
                (and (listp block)
                     (assoc "type" block :test #'string=)))
              content)))

(defun ensure-anthropic-content-blocks (content)
  "Normalize CONTENT into a list of Anthropic content-block alists."
  (cond
    ((null content) '())
    ((anthropic-content-blocks-p content) content)
    ((stringp content)
     (list `(("type" . "text") ("text" . ,content))))
    (t
     (list `(("type" . "text") ("text" . ,(prin1-to-string content)))))))

(defun flatten-content-to-string (content)
  "Flatten CONTENT to a plain string for OpenAI.
CONTENT may be a string (returned as-is), NIL (returns \"\"),
or a list of Anthropic-style content-block alists whose \"text\"
values are joined with newlines."
  (cond ((stringp content) content)
        ((null content) "")
        ((listp content)
         (format nil "~{~A~^~%~}"
                 (loop for block in content
                       when (and (listp block)
                                 (equal (cdr (assoc "type" block :test #'equal)) "text"))
                       collect (cdr (assoc "text" block :test #'equal)))))
        (t (princ-to-string content))))

(defun messages-to-anthropic-format (messages)
  "Convert message structs to Anthropic API format.
   Returns (values system-prompt api-messages).
   The system value may be a plain string or a list of content-block alists —
   the Anthropic API accepts both forms for the top-level \"system\" field.
   Callers pass it through to alist-to-hash / to-json-value unchanged."
  (let ((system nil)
        (api-msgs nil))
    (dolist (msg messages)
      (case (message-role msg)
        (:system
         ;; message-content may be a string or a list of content-block alists.
         ;; Either form is accepted by the Anthropic API and serialised correctly
         ;; by to-json-value: a string stays a string; a list of alists becomes
         ;; a JSON array of objects (the structured system-prompt format).
         (setf system (message-content msg)))
        (:user
          (push `(("role" . "user")
                  ("content" . ,(ensure-anthropic-content-blocks
                                 (message-content msg))))
                api-msgs))
          (:assistant
           (if (message-tool-calls msg)
               ;; Assistant with tool use
               (let ((content-blocks nil))
                  ;; Add thinking block first if present (with signature for API compliance)
                  (when (message-thinking msg)
                    (push (if (message-thinking-signature msg)
                              `(("type" . "thinking")
                                ("thinking" . ,(message-thinking msg))
                                ("signature" . ,(message-thinking-signature msg)))
                              `(("type" . "thinking")
                                ("thinking" . ,(message-thinking msg))))
                          content-blocks))
                  ;; Add text content blocks
                  (setf content-blocks
                        (nconc (nreverse content-blocks)
                               (ensure-anthropic-content-blocks (message-content msg))))
                  ;; Add tool_use blocks
                  (dolist (tc (message-tool-calls msg))
                    (setf content-blocks
                          (nconc content-blocks
                                 (list `(("type" . "tool_use")
                                         ("id" . ,(tool-call-id tc))
                                         ("name" . ,(tool-call-name tc))
                                         ("input" . ,(or (tool-call-arguments tc)
                                                         (make-hash-table
                                                          :test 'equal))))))))
                 (push `(("role" . "assistant")
                         ("content" . ,content-blocks))
                       api-msgs))
               ;; Plain assistant message
               (let ((content-blocks nil))
                 ;; Add thinking block first if present (with signature for API compliance)
                 (when (message-thinking msg)
                   (push (if (message-thinking-signature msg)
                             `(("type" . "thinking")
                               ("thinking" . ,(message-thinking msg))
                               ("signature" . ,(message-thinking-signature msg)))
                             `(("type" . "thinking")
                               ("thinking" . ,(message-thinking msg))))
                         content-blocks))
                 ;; Add text content blocks
                 (setf content-blocks
                       (nconc (nreverse content-blocks)
                              (ensure-anthropic-content-blocks (message-content msg))))
                 (push `(("role" . "assistant")
                         ("content" . ,content-blocks))
                       api-msgs))))
        (:tool
         (push `(("role" . "user")
                 ("content" . ((("type" . "tool_result")
                                ("tool_use_id" . ,(message-tool-call-id msg))
                                ("content" . ,(message-content msg))))))
               api-msgs))))
    (values system (nreverse api-msgs))))

(defun normalize-tool-parameters (params)
  "Ensure PARAMS is a JSON Schema object with safe empty defaults."
  (let* ((base (or params '(("type" . "object")
                            ("properties" . nil)
                            ("required" . nil))))
         (params-ht (if (hash-table-p base)
                        base
                        (alist-to-hash base))))
    (when (null (gethash "properties" params-ht))
      (setf (gethash "properties" params-ht)
            (make-hash-table :test 'equal)))
    (when (null (gethash "required" params-ht))
      (setf (gethash "required" params-ht) #()))
    params-ht))

(defun tools-to-anthropic-format (tools)
  "Convert tool schemas to Anthropic format.
When optimization.cache-enabled is set, adds cache_control ephemeral to the
last tool so the entire tools prefix is cached by the Anthropic API."
  (let ((formatted-tools
          (mapcar (lambda (tool-schema)
                    `(("name" . ,(getf tool-schema :name))
                      ("description" . ,(getf tool-schema :description))
                      ("input_schema" . ,(normalize-tool-parameters
                                          (getf tool-schema :parameters)))))
                  tools)))
    (when (and formatted-tools
               (config-value "optimization.cache-enabled"))
      (let* ((last-cons (last formatted-tools))
             (with-cache (append (car last-cons)
                                 '(("cache_control" . (("type" . "ephemeral")))))))
        (setf (car last-cons) with-cache)))
    formatted-tools))

(defun add-cache-control-to-last-block (content-blocks)
  "Destructively add cache_control ephemeral to the last content block in
CONTENT-BLOCKS.  Skips if cache_control is already present.
Returns CONTENT-BLOCKS."
  (when (and (listp content-blocks) content-blocks)
    (let* ((last-cons  (last content-blocks))
           (last-block (car last-cons)))
      (unless (assoc "cache_control" last-block :test #'equal)
        (setf (car last-cons)
              (append last-block
                      (list (cons "cache_control"
                                  (list (cons "type" "ephemeral")))))))))
  content-blocks)

(defun add-conversation-cache-points (messages)
  "Add cache_control ephemeral markers to conversation history messages.
Marks the last content block of the oldest user turn and the last content
block of the second-oldest user turn, giving Anthropic two cache anchors
within the conversation (per the multi-turn caching best practice).
Returns MESSAGES unchanged when caching is disabled or history is too short."
  (when (or (not (config-value "optimization.cache-enabled"))
            (< (length messages) 3))
    (return-from add-conversation-cache-points messages))
  (let ((user-turns (remove-if-not
                     (lambda (m)
                       (equal (cdr (assoc "role" m :test #'equal)) "user"))
                     messages)))
    (when (>= (length user-turns) 2)
      (let ((oldest      (nth (- (length user-turns) 2) user-turns))
            (second-last (nth (- (length user-turns) 1) user-turns)))
        (add-cache-control-to-last-block
         (cdr (assoc "content" oldest :test #'equal)))
        (add-cache-control-to-last-block
         (cdr (assoc "content" second-last :test #'equal))))))
  messages)

(defun prepare-anthropic-messages (messages)
  "Extract system prompt and format conversation messages for the Anthropic API.
Applies conversation-level cache markers before returning.
Returns (values system-content-blocks api-messages)."
  (multiple-value-bind (system api-messages)
      (messages-to-anthropic-format messages)
    (values system
            (add-conversation-cache-points api-messages))))

(defun add-cache-control-to-system (system)
  "Add cache_control ephemeral to the first block of a system content list.
The first block is the static system prompt; it is cache-stable and benefits
most from prompt caching.  The second block (dynamic summary) is intentionally
left unmarked because it changes after every compaction.
Returns SYSTEM unchanged when caching is disabled or SYSTEM is not a list."
  (if (and (listp system)
           system
           (config-value "optimization.cache-enabled"))
      (let* ((first-block (first system))
             (cached-first (append first-block
                                   '(("cache_control" . (("type" . "ephemeral"))))))
             (rest-blocks (rest system)))
        (cons cached-first rest-blocks))
      system))

(defun anthropic-thinking-params (model-name)
  "Return thinking params alist for supported Anthropic models, or NIL." 
  (labels ((version-at-least-p (prefix min-version)
             (when (and (stringp model-name)
                        (<= (length prefix) (length model-name))
                        (string= prefix (subseq model-name 0 (length prefix))))
               (let* ((rest (subseq model-name (length prefix)))
                      (dash (position #\- rest))
                      (version-str (if dash (subseq rest 0 dash) rest))
                      (version (ignore-errors (parse-integer version-str
                                                             :junk-allowed t))))
                 (and version (>= version min-version)))))
           (model-is-opus-p ()
             (version-at-least-p "claude-opus-4-" 6))
           (model-supports-thinking-p ()
             (or (model-is-opus-p)
                 (version-at-least-p "claude-sonnet-4-" 6))))
    (when (model-supports-thinking-p)
      (let ((effort (or (config-value "thinking.effort") "high")))
        (when (and (string= effort "max")
                   (not (model-is-opus-p)))
          (log-warn "llm" "effort=max only supported on Opus, downgrading to high for ~a"
                    model-name)
          (setf effort "high"))
        (list (cons "thinking" '(("type" . "adaptive")))
              (cons "output_config" `(("effort" . ,effort))))))))

(defun apply-anthropic-thinking (body model-name)
  "Merge thinking params into BODY for MODEL-NAME, adjusting temperature.
Anthropic requires temperature=1 when thinking/adaptive mode is active.
Returns the (possibly modified) body alist."
  (let ((thinking-params (anthropic-thinking-params model-name)))
    (if thinking-params
        (let ((adjusted (mapcar (lambda (pair)
                                  (if (string= (car pair) "temperature")
                                      (cons "temperature" 1.0)
                                      pair))
                                body)))
          (append adjusted thinking-params))
        body)))


(defvar *cache-stats*
  '(:total-input-tokens 0 :total-cached-tokens 0
    :total-output-tokens 0 :request-count 0)
  "Cumulative cache statistics for the session.")

(defun track-cache-stats (usage-plist)
  "Accumulate cache statistics from a single request's usage-plist."
  (when usage-plist
    (incf (getf *cache-stats* :total-input-tokens)
          (or (getf usage-plist :input-tokens) 0))
    (incf (getf *cache-stats* :total-cached-tokens)
          (or (getf usage-plist :cache-read-tokens) 0))
    (incf (getf *cache-stats* :total-output-tokens)
          (or (getf usage-plist :output-tokens) 0))
    (incf (getf *cache-stats* :request-count))))

(defun format-cache-stats ()
  "Return a formatted string of cumulative cache statistics."
  (let* ((input (getf *cache-stats* :total-input-tokens))
         (cached (getf *cache-stats* :total-cached-tokens))
         (output (getf *cache-stats* :total-output-tokens))
         (requests (getf *cache-stats* :request-count))
         (pct (if (and input (> input 0))
                  (round (* 100 cached) input)
                  0)))
    (format nil "Cache Statistics (session):~%  Requests: ~a~%  Total input tokens: ~:d~%  Cached tokens: ~:d (~a%)~%  Output tokens: ~:d"
            requests input cached pct output)))

(defun reset-cache-stats ()
  "Reset cumulative cache statistics."
  (setf (getf *cache-stats* :total-input-tokens) 0
        (getf *cache-stats* :total-cached-tokens) 0
        (getf *cache-stats* :total-output-tokens) 0
        (getf *cache-stats* :request-count) 0))

(defun log-and-track-usage (usage-plist &optional (prefix ""))
  "Log usage with cache percentage and accumulate stats."
  (when usage-plist
    (let* ((input (getf usage-plist :input-tokens))
           (cached (getf usage-plist :cache-read-tokens))
           (pct (if (and input (> input 0))
                    (round (* 100 cached) input)
                    0)))
      (log-info "llm"
       "Tokens~a: input=~a output=~a cache-read=~a (~a%) cache-write=~a"
       prefix input (getf usage-plist :output-tokens)
       cached pct (getf usage-plist :cache-write-tokens)))
    (track-cache-stats usage-plist)))

(defun parse-anthropic-response (response)
  "Parse Anthropic API response into a message struct.
Returns (values message usage-plist) where usage-plist may be nil."
  (let* ((raw-blocks (gethash "content" response))
         (content-blocks (if (listp raw-blocks)
                             raw-blocks
                             (coerce raw-blocks 'list)))
         (text-parts nil)
         (thinking-parts nil)
         (thinking-signature nil)
         (tool-calls nil)
         (usage-ht (gethash "usage" response))
         (usage-plist (when usage-ht
                        (list :input-tokens (or (gethash "input_tokens" usage-ht) 0)
                              :output-tokens (or (gethash "output_tokens" usage-ht) 0)
                              :cache-read-tokens (or (gethash "cache_read_input_tokens" usage-ht) 0)
                              :cache-write-tokens (or (gethash "cache_creation_input_tokens" usage-ht) 0)))))
    (when content-blocks
      (loop for block in content-blocks
            for block-type = (gethash "type" block)
            do (cond
                 ((string= block-type "text")
                  (push (gethash "text" block) text-parts))
                 ((string= block-type "thinking")
                  (push (gethash "thinking" block) thinking-parts)
                  (let ((sig (gethash "signature" block)))
                    (when sig (setf thinking-signature sig))))
                 ((string= block-type "tool_use")
                  (push (make-tool-call
                         :id (gethash "id" block)
                         :name (gethash "name" block)
                         :arguments (hash-to-alist
                                     (gethash "input" block)))
                        tool-calls)))))
    (log-and-track-usage usage-plist)
    (values
     (assistant-message
      (when text-parts
        (string-join "" (nreverse text-parts)))
      :tool-calls (nreverse tool-calls)
      :thinking (when thinking-parts
                  (string-join "" (nreverse thinking-parts)))
      :thinking-signature thinking-signature)
     usage-plist)))

(defun parse-anthropic-sse-events (event-type data-str)
  "Parse an Anthropic SSE event into a normalized plist.
Returns NIL when parsing fails."
  (handler-case
      (let ((data (yason:parse data-str :object-as :hash-table)))
        (cond
          ((string= event-type "content_block_start")
           (let ((block (gethash "content_block" data)))
             (list :event event-type
                   :content-block-type (gethash "type" block)
                   :tool-id (gethash "id" block)
                   :tool-name (gethash "name" block))))
           ((string= event-type "content_block_delta")
            (let* ((delta (gethash "delta" data))
                   (delta-type (gethash "type" delta)))
              (list :event event-type
                    :delta-type delta-type
                    :text (gethash "text" delta)
                    :thinking (gethash "thinking" delta)
                    :signature (gethash "signature" delta)
                    :partial-json (gethash "partial_json" delta))))
          ((string= event-type "content_block_stop")
           (list :event event-type))
          ((string= event-type "message_stop")
           (list :event event-type))
          ((string= event-type "message_start")
           (let* ((message-obj (gethash "message" data))
                  (usage-obj (when message-obj (gethash "usage" message-obj))))
             (list :event event-type
                   :input-tokens (when usage-obj (gethash "input_tokens" usage-obj))
                   :cache-read-tokens (when usage-obj (gethash "cache_read_input_tokens" usage-obj))
                   :cache-write-tokens (when usage-obj (gethash "cache_creation_input_tokens" usage-obj)))))
          ((string= event-type "message_delta")
           (let ((usage-obj (gethash "usage" data)))
             (list :event event-type
                   :output-tokens (when usage-obj (gethash "output_tokens" usage-obj)))))
          (t
           (list :event event-type :data data))))
    (error () nil)))

(defun complete-anthropic-streaming (client messages tools)
  "Stream Anthropic responses, invoking *streaming-text-callback* per text delta
and *streaming-thinking-callback* per thinking delta.
Returns a reconstructed assistant message."
  (multiple-value-bind (system api-messages)
      (prepare-anthropic-messages messages)
    (let* ((system-with-cache (add-cache-control-to-system system))
           (body `(("model" . ,(client-model client))
                   ("max_tokens" . ,(client-max-tokens client))
                   ("temperature" . ,(client-temperature client))
                   ("messages" . ,api-messages)
                   ("stream" . t)))
           (body-with-tools (if tools
                                (append body `(("tools" . ,(tools-to-anthropic-format tools))))
                                body))
           (final-body (if system-with-cache
                           (append `(("system" . ,system-with-cache)) body-with-tools)
                           body-with-tools))
           (final-body-with-thinking (apply-anthropic-thinking final-body (client-model client)))
           (text-parts nil)
           (thinking-parts nil)
           (signature-parts nil)
           (tool-calls nil)
           (current-tool-id nil)
           (current-tool-name nil)
           (current-tool-input-parts nil)
           (input-tokens-from-start nil)
           (output-tokens-from-delta nil)
           (cache-read-tokens-from-start nil)
           (cache-write-tokens-from-start nil))
      (labels ((finalize-tool-call ()
                 (when current-tool-name
                   (let* ((json-str (if current-tool-input-parts
                                        (format nil "~{~a~}" (nreverse current-tool-input-parts))
                                        "{}"))
                          (args (handler-case
                                    (let ((parsed-args (yason:parse json-str :object-as :hash-table)))
                                      (if (hash-table-p parsed-args)
                                          (hash-to-alist parsed-args)
                                          parsed-args))
                                  (error () nil))))
                     (push (make-tool-call
                            :id current-tool-id
                            :name current-tool-name
                            :arguments args)
                           tool-calls)
                     (setf current-tool-id nil
                           current-tool-name nil
                           current-tool-input-parts nil)))))
        (http-post-stream
         (anthropic-messages-url client)
         (anthropic-headers client)
         (alist-to-hash final-body-with-thinking)
         (lambda (event-type data-str)
           (let ((parsed (parse-anthropic-sse-events event-type data-str)))
             (when parsed
               (let ((parsed-event (getf parsed :event)))
                 (cond
                   ((string= parsed-event "content_block_delta")
                    (let ((delta-type (getf parsed :delta-type)))
                      (cond
                        ((string= delta-type "text_delta")
                         (let ((text (getf parsed :text)))
                           (when text
                             (push text text-parts)
                             (when *streaming-text-callback*
                               (funcall *streaming-text-callback* text)))))
                        ((string= delta-type "thinking_delta")
                         (let ((thinking (getf parsed :thinking)))
                           (when thinking
                             (push thinking thinking-parts)
                             ;; ★ リアルタイム表示コールバックを呼ぶ（修正箇所）
                             (when *streaming-thinking-callback*
                               (funcall *streaming-thinking-callback* thinking)))))
                        ((string= delta-type "signature_delta")
                         (let ((sig (getf parsed :signature)))
                           (when sig
                             (push sig signature-parts))))
                        ((string= delta-type "input_json_delta")
                         (let ((partial (getf parsed :partial-json)))
                           (when partial
                             (push partial current-tool-input-parts)))))))
                   ((string= parsed-event "content_block_start")
                    (let ((block-type (getf parsed :content-block-type)))
                      (cond
                        ((string= block-type "tool_use")
                         (setf current-tool-id (getf parsed :tool-id)
                               current-tool-name (getf parsed :tool-name)
                               current-tool-input-parts nil))
                        ((string= block-type "thinking")
                         nil))))
                   ((string= parsed-event "content_block_stop")
                    (finalize-tool-call))
                   ((string= parsed-event "message_start")
                    (setf input-tokens-from-start (getf parsed :input-tokens)
                          cache-read-tokens-from-start (getf parsed :cache-read-tokens)
                          cache-write-tokens-from-start (getf parsed :cache-write-tokens)))
                   ((string= parsed-event "message_delta")
                    (setf output-tokens-from-delta (getf parsed :output-tokens))))))))
         (lambda () nil))
        (let ((usage-plist (when (or input-tokens-from-start output-tokens-from-delta)
                             (list :input-tokens (or input-tokens-from-start 0)
                                   :output-tokens (or output-tokens-from-delta 0)
                                   :cache-read-tokens (or cache-read-tokens-from-start 0)
                                   :cache-write-tokens (or cache-write-tokens-from-start 0)))))
          (log-and-track-usage usage-plist " (streaming)")
          (when usage-plist
            (handler-case
                (let ((total-cache-tokens (+ (or cache-read-tokens-from-start 0)
                                             (or cache-write-tokens-from-start 0))))
                  (sibyl.cache:record-server-cache-tokens total-cache-tokens))
              (error (e)
                (log-warn "llm" "Telemetry recording failed: ~a" e))))
          (values
           (assistant-message
            (when text-parts
              (string-join "" (nreverse text-parts)))
            :tool-calls (nreverse tool-calls)
            :thinking (when thinking-parts
                        (string-join "" (nreverse thinking-parts)))
            :thinking-signature (when signature-parts
                                  (string-join "" (nreverse signature-parts))))
           usage-plist))))))

(defun anthropic-messages-url (client)
  "Build the messages endpoint URL. Appends ?beta=true for OAuth."
  (let ((base (format nil "~a/messages" (client-base-url client))))
    (if (anthropic-oauth-p client)
        (concatenate 'string base "?beta=true")
        base)))

(defmethod complete ((client anthropic-client) messages &key)
  "Send messages to Anthropic Claude API.
Returns (values message usage-plist)."
  (log-info "llm" "Anthropic complete (model: ~a, streaming: ~a)"
            (client-model client)
            (if *streaming-text-callback* "yes" "no"))
  (if *streaming-text-callback*
      (complete-anthropic-streaming client messages nil)
      (multiple-value-bind (system api-messages)
          (prepare-anthropic-messages messages)
        (let* ((system-with-cache (add-cache-control-to-system system))
                (body `(("model" . ,(client-model client))
                        ("max_tokens" . ,(client-max-tokens client))
                        ("temperature" . ,(client-temperature client))
                        ("messages" . ,api-messages))))
          (when system-with-cache
            (push (cons "system" system-with-cache) body))
          (setf body (apply-anthropic-thinking body (client-model client)))
          (let ((response (http-post-json
                           (anthropic-messages-url client)
                           (anthropic-headers client)
                           (alist-to-hash body))))
            (parse-anthropic-response response))))))

(defmethod complete-with-tools ((client anthropic-client) messages tools &key)
  "Send messages with tools to Anthropic Claude API.
Returns (values message usage-plist)."
  (log-info "llm" "Anthropic complete-with-tools (model: ~a, tools: ~a, streaming: ~a)"
            (client-model client)
            (length tools)
            (if *streaming-text-callback* "yes" "no"))
  (if *streaming-text-callback*
      (complete-anthropic-streaming client messages tools)
      (multiple-value-bind (system api-messages)
          (prepare-anthropic-messages messages)
        (let* ((system-with-cache (add-cache-control-to-system system))
                (body `(("model" . ,(client-model client))
                        ("max_tokens" . ,(client-max-tokens client))
                        ("temperature" . ,(client-temperature client))
                        ("messages" . ,api-messages)
                        ("tools" . ,(tools-to-anthropic-format tools)))))
          (when system-with-cache
            (push (cons "system" system-with-cache) body))
          (setf body (apply-anthropic-thinking body (client-model client)))
          (let ((response (http-post-json
                           (anthropic-messages-url client)
                           (anthropic-headers client)
                           (alist-to-hash body))))
            (parse-anthropic-response response))))))

;;; ============================================================
;;; OpenAI (GPT)
;;; ============================================================


(defvar *openai-reasoning-effort* nil
  "When non-nil, a string like \"low\", \"medium\", \"high\", or \"xhigh\"
specifying the reasoning effort level for OpenAI models that support it.")

(defun openai-temperature-pair (client)
  "Return a (\"temperature\" . value) pair when supported for CLIENT, else NIL."
  (let* ((temp (client-temperature client))
         (model (client-model client))
         (gpt5-p (and model
                      (<= 5 (length model))
                      (string= "gpt-5" (subseq model 0 5)))))
    (cond
      ((null temp) nil)
      (gpt5-p (when (= temp 1) `("temperature" . ,temp)))
      (t `("temperature" . ,temp)))))

(defun openai-reasoning-params ()
  "Return an alist entry for the OpenAI 'reasoning' parameter when configured.
Returns a list like ((\"reasoning\" (\"effort\" . \"high\"))) or NIL."
  (when *openai-reasoning-effort*
    `(("reasoning" . (("effort" . ,*openai-reasoning-effort*))))))

(defun apply-openai-reasoning (body)
  "Merge OpenAI reasoning effort params into BODY alist if configured.
Returns the (possibly modified) body alist."
  (let ((reasoning-params (openai-reasoning-params)))
    (if reasoning-params
        (append body reasoning-params)
        body)))

(defclass openai-client (llm-client)
  ()
   (:default-initargs
    :base-url "https://api.openai.com/v1"
    :model "gpt-5.2-codex")
  (:documentation "Client for OpenAI's GPT API."))

(defun make-openai-client (&key api-key
                              (model "gpt-5.2-codex")
                              (max-tokens 4096)
                              (temperature 0.0))
  "Create an OpenAI API client."
  (log-debug "llm" "Creating OpenAI client (model: ~a)" model)
  (make-instance 'openai-client
                 :api-key (or api-key
                              (config-value "llm.openai.api-key")
                              (error 'config-missing-key-error
                                     :key "llm.openai.api-key"))
                 :model model
                 :max-tokens max-tokens
                 :temperature temperature))

(defun openai-headers (client)
  "Build OpenAI-specific headers."
  (list (cons "Authorization"
              (format nil "Bearer ~a" (client-api-key client)))
        (cons "User-Agent" "sibyl/0.1.0")))

(defun messages-to-openai-format (messages)
  "Convert message structs to OpenAI API format.
Content is flattened to plain strings for optimal prefix caching."
  (mapcar
   (lambda (msg)
     (case (message-role msg)
       (:system `(("role" . "system")
                  ("content" . ,(flatten-content-to-string (message-content msg)))))
       (:user `(("role" . "user")
                ("content" . ,(flatten-content-to-string (message-content msg)))))
       (:assistant
        (if (message-tool-calls msg)
            `(("role" . "assistant")
              ("content" . ,(or (message-content msg) :null))
              ("tool_calls"
               . ,(mapcar
                   (lambda (tc)
                     `(("id" . ,(tool-call-id tc)) ("type" . "function")
                       ("function" ("name" . ,(tool-call-name tc))
                        ("arguments"
                         . ,(with-output-to-string (s)
                              (yason:encode
                               (alist-to-hash (tool-call-arguments tc)) s))))))
                   (message-tool-calls msg))))
            `(("role" . "assistant")
              ("content" . ,(flatten-content-to-string (message-content msg))))))
       (:tool
        `(("role" . "tool") ("tool_call_id" . ,(message-tool-call-id msg))
          ("content" . ,(flatten-content-to-string (message-content msg)))))))
   messages))

(defun parse-openai-response (response)
  "Parse OpenAI API response into a message struct.
Returns (values message usage-plist) where usage-plist may be nil."
  (let* ((raw-choices (gethash "choices" response))
         (choices (if (listp raw-choices) raw-choices (coerce raw-choices 'list)))
         (first-choice (first choices)))
    (unless (and choices first-choice)
      (error 'llm-invalid-response
             :message "OpenAI response missing choices"
             :raw-response response))
    (let* ((msg (gethash "message" first-choice))
           (usage-ht (gethash "usage" response))
           (usage-plist (when usage-ht
                          (let* ((prompt-details (gethash "prompt_tokens_details" usage-ht))
                                 (cached-tokens (if prompt-details
                                                    (or (gethash "cached_tokens" prompt-details) 0)
                                                    0)))
                            (list :input-tokens (or (gethash "prompt_tokens" usage-ht) 0)
                                  :output-tokens (or (gethash "completion_tokens" usage-ht) 0)
                                  :cache-read-tokens cached-tokens
                                  :cache-write-tokens 0)))))
      (unless msg
        (error 'llm-invalid-response
               :message "OpenAI response missing message"
               :raw-response response))
      (let* ((content (gethash "content" msg))
             (raw-tool-calls (gethash "tool_calls" msg))
             (tool-calls nil))
        (when raw-tool-calls
          (let ((tcs (if (listp raw-tool-calls) raw-tool-calls (coerce raw-tool-calls 'list))))
            (loop for tc in tcs
                  for func = (gethash "function" tc)
                  do (push (make-tool-call
                            :id (gethash "id" tc)
                            :name (gethash "name" func)
                            :arguments (hash-to-alist
                                        (yason:parse (gethash "arguments" func)
                                                     :object-as :hash-table)))
                           tool-calls))))
        (log-and-track-usage usage-plist)
        (values
         (assistant-message content :tool-calls (nreverse tool-calls))
         usage-plist)))))


(defun complete-openai-streaming (client messages tools)
  "Stream OpenAI responses, invoking *streaming-text-callback* per text delta.
Returns a reconstructed assistant message."
  (let* ((openai-tools
           (when tools
             (mapcar (lambda (ts)
                       `(("type" . "function")
                         ("function"
                          . (("name" . ,(getf ts :name))
                             ("description" . ,(getf ts :description))
                             ("parameters" . ,(normalize-tool-parameters
                                               (getf ts :parameters)))))))
                     tools)))
         (body `(("model" . ,(client-model client))
                 ("max_completion_tokens" . ,(client-max-tokens client))
                 ("messages" . ,(messages-to-openai-format messages))
                 ("store" . t)
                 ("stream" . t)
                 ("stream_options" . (("include_usage" . t)))))
         (temp-pair (openai-temperature-pair client))
         (final-body (progn
                       (when temp-pair
                         (push temp-pair body))
                       (let ((with-tools (if openai-tools
                                             (append body `(("tools" . ,openai-tools)))
                                             body)))
                         (apply-openai-reasoning with-tools))))
         (text-parts nil)
         (tool-call-state (make-hash-table :test 'eql))
         (prompt-tokens nil)
         (completion-tokens nil)
         (cached-tokens nil))
    (labels ((normalize-tool-call-index (value)
               (cond
                 ((integerp value) value)
                 ((and (stringp value)
                       (ignore-errors (parse-integer value)))
                  (parse-integer value))
                 (t 0)))
             (ensure-tool-call-state (index)
               (or (gethash index tool-call-state)
                   (setf (gethash index tool-call-state)
                         (list :id nil :name nil :args-parts nil))))
             (record-tool-call-delta (tc)
               (let* ((index (normalize-tool-call-index (gethash "index" tc)))
                      (state (ensure-tool-call-state index))
                      (func (gethash "function" tc))
                      (tc-id (gethash "id" tc))
                      (tc-name (when func (gethash "name" func)))
                      (tc-args (when func (gethash "arguments" func))))
                 (when tc-id (setf (getf state :id) tc-id))
                 (when tc-name (setf (getf state :name) tc-name))
                 (when (and tc-args (not (equal tc-args :null)))
                   (push tc-args (getf state :args-parts)))))
             (finalize-tool-call (state)
               (let* ((tool-name (getf state :name))
                      (tool-id (getf state :id))
                      (parts (getf state :args-parts))
                      (json-str (if parts
                                    (format nil "~{~a~}" (nreverse parts))
                                    "{}"))
                      (args (handler-case
                                (let ((parsed-args (yason:parse json-str :object-as :hash-table)))
                                  (if (hash-table-p parsed-args)
                                      (hash-to-alist parsed-args)
                                      parsed-args))
                              (error () nil))))
                 (when tool-name
                   (make-tool-call :id tool-id :name tool-name :arguments args)))))
      (http-post-stream
       (format nil "~a/chat/completions" (client-base-url client))
       (openai-headers client)
       (alist-to-hash final-body)
        (lambda (event-type data-str)
          (declare (ignore event-type))
          (handler-case
              (let* ((data (yason:parse data-str :object-as :hash-table))
                     (choices (gethash "choices" data))
                     (choice (cond
                               ((vectorp choices)
                                (when (> (length choices) 0)
                                  (aref choices 0)))
                               ((listp choices)
                                (first choices))))
                     (delta (when choice (gethash "delta" choice)))
                     (usage (gethash "usage" data)))
                ;; Extract usage from final chunk
                (when usage
                  (setf prompt-tokens (gethash "prompt_tokens" usage)
                        completion-tokens (gethash "completion_tokens" usage))
                  (let ((prompt-details (gethash "prompt_tokens_details" usage)))
                    (when prompt-details
                      (setf cached-tokens (gethash "cached_tokens" prompt-details)))))
                (when delta
                  (let ((content (gethash "content" delta)))
                    (when (and content (not (equal content :null)))
                      (push content text-parts)
                      (funcall *streaming-text-callback* content)))
                  (let ((tool-calls-delta (gethash "tool_calls" delta)))
                    (when tool-calls-delta
                      (cond
                        ((vectorp tool-calls-delta)
                         (dotimes (i (length tool-calls-delta))
                           (record-tool-call-delta (aref tool-calls-delta i))))
                        ((listp tool-calls-delta)
                         (dolist (tc tool-calls-delta)
                           (record-tool-call-delta tc))))))))
            (error () nil)))
        (lambda () nil))
      (let* ((usage-plist (when (or prompt-tokens completion-tokens)
                            (list :input-tokens (or prompt-tokens 0)
                                  :output-tokens (or completion-tokens 0)
                                  :cache-read-tokens (or cached-tokens 0)
                                  :cache-write-tokens 0))))
        (log-and-track-usage usage-plist " (streaming)")
        (values
         (assistant-message
          (when text-parts
            (string-join "" (nreverse text-parts)))
          :tool-calls (let ((indices nil)
                            (tool-calls nil))
                        (maphash (lambda (k v)
                                   (declare (ignore v))
                                   (push k indices))
                                 tool-call-state)
                        (dolist (idx (sort indices #'<))
                          (let ((tc (finalize-tool-call (gethash idx tool-call-state))))
                            (when tc (push tc tool-calls))))
                        (nreverse tool-calls)))
         usage-plist)))))

(defmethod complete ((client openai-client) messages &key)
  "Send messages to OpenAI GPT API."
  (log-info "llm" "OpenAI complete (model: ~a, streaming: ~a)"
            (client-model client)
            (if *streaming-text-callback* "yes" "no"))
  (if *streaming-text-callback*
      (complete-openai-streaming client messages nil)
      (let* ((body `(("model" . ,(client-model client))
                     ("max_completion_tokens" . ,(client-max-tokens client))
                     ("store" . t)
                     ("messages" . ,(messages-to-openai-format messages))))
             (temp-pair (openai-temperature-pair client)))
        (when temp-pair
          (push temp-pair body))
        (let ((response (http-post-json
                         (format nil "~a/chat/completions"
                                 (client-base-url client))
                         (openai-headers client)
                         (alist-to-hash body))))
          (parse-openai-response response)))))

(defmethod complete-with-tools ((client openai-client) messages tools &key)
  "Send messages with tools to OpenAI GPT API."
  (log-info "llm" "OpenAI complete-with-tools (model: ~a, tools: ~a, streaming: ~a)"
            (client-model client)
            (length tools)
            (if *streaming-text-callback* "yes" "no"))
  (if *streaming-text-callback*
      (complete-openai-streaming client messages tools)
      (let* ((openai-tools
               (mapcar (lambda (ts)
                         `(("type" . "function")
                           ("function"
                            . (("name" . ,(getf ts :name))
                               ("description" . ,(getf ts :description))
                               ("parameters" . ,(normalize-tool-parameters
                                                 (getf ts :parameters)))))))
                       tools))
             (body `(("model" . ,(client-model client))
                     ("max_completion_tokens" . ,(client-max-tokens client))
                     ("messages" . ,(messages-to-openai-format messages))
                     ("store" . t)
                     ("tools" . ,openai-tools)))
             (temp-pair (openai-temperature-pair client)))
        (when temp-pair
          (push temp-pair body))
        (let ((response (http-post-json
                         (format nil "~a/chat/completions"
                                 (client-base-url client))
                         (openai-headers client)
                         (alist-to-hash body))))
          (parse-openai-response response)))))
