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
   :model "claude-sonnet-4-5-20250929")
  (:documentation "Client for Anthropic's Claude API."))

(defun oauth-token-p (api-key)
  "Return T if API-KEY is an OAuth access token."
  (and (stringp api-key)
       (>= (length api-key) (length *oauth-token-prefix*))
       (string= *oauth-token-prefix*
                (subseq api-key 0 (length *oauth-token-prefix*)))))

(defun make-anthropic-client (&key api-key
                                (model "claude-sonnet-4-5-20250929")
                                (max-tokens 4096)
                                (temperature 0.0))
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
   Uses Bearer auth for OAuth tokens, x-api-key for standard API keys."
  (let ((headers (list (cons "anthropic-version" (anthropic-api-version client))
                       (cons "User-Agent" "sibyl/0.1.0"))))
    (if (anthropic-oauth-p client)
        (cons (cons "Authorization"
                    (format nil "Bearer ~a" (client-api-key client)))
              (cons (cons "anthropic-beta" *oauth-beta-flag*)
                    headers))
        (cons (cons "x-api-key" (client-api-key client))
              headers))))

(defun messages-to-anthropic-format (messages)
  "Convert message structs to Anthropic API format.
   Returns (values system-prompt api-messages)."
  (let ((system nil)
        (api-msgs nil))
    (dolist (msg messages)
      (case (message-role msg)
        (:system
         (setf system (message-content msg)))
        (:user
         (push `(("role" . "user")
                 ("content" . ,(message-content msg)))
               api-msgs))
        (:assistant
         (if (message-tool-calls msg)
             ;; Assistant with tool use
             (let ((content-blocks nil))
               (when (message-content msg)
                 (push `(("type" . "text") ("text" . ,(message-content msg)))
                       content-blocks))
                (dolist (tc (message-tool-calls msg))
                  (push `(("type" . "tool_use")
                          ("id" . ,(tool-call-id tc))
                          ("name" . ,(tool-call-name tc))
                          ("input" . ,(or (tool-call-arguments tc)
                                          (make-hash-table :test 'equal))))
                        content-blocks))
               (push `(("role" . "assistant")
                       ("content" . ,(nreverse content-blocks)))
                     api-msgs))
             ;; Plain assistant message
             (push `(("role" . "assistant")
                     ("content" . ,(message-content msg)))
                   api-msgs)))
        (:tool
         (push `(("role" . "user")
                 ("content" . ((("type" . "tool_result")
                                ("tool_use_id" . ,(message-tool-call-id msg))
                                ("content" . ,(message-content msg))))))
               api-msgs))))
    (values system (nreverse api-msgs))))

(defun tools-to-anthropic-format (tools)
  "Convert tool schemas to Anthropic format."
  (mapcar (lambda (tool-schema)
            `(("name" . ,(getf tool-schema :name))
              ("description" . ,(getf tool-schema :description))
              ("input_schema" . ,(getf tool-schema :parameters))))
          tools))

(defun parse-anthropic-response (response)
  "Parse Anthropic API response into a message struct.
Returns (values message usage-plist) where usage-plist may be nil."
  (let* ((raw-blocks (gethash "content" response))
         (content-blocks (if (listp raw-blocks)
                             raw-blocks
                             (coerce raw-blocks 'list)))
         (text-parts nil)
         (thinking-parts nil)
         (tool-calls nil)
         ;; Extract usage
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
                  (push (gethash "thinking" block) thinking-parts))
                 ((string= block-type "tool_use")
                  (push (make-tool-call
                         :id (gethash "id" block)
                         :name (gethash "name" block)
                         :arguments (hash-to-alist
                                     (gethash "input" block)))
                        tool-calls)))))
    (values
     (assistant-message
      (when text-parts
        (string-join "" (nreverse text-parts)))
      :tool-calls (nreverse tool-calls)
      :thinking (when thinking-parts
                  (string-join "" (nreverse thinking-parts))))
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
                   :partial-json (gethash "partial_json" delta))))
          ((string= event-type "content_block_stop")
           (list :event event-type))
          ((string= event-type "message_stop")
           (list :event event-type))
          ((string= event-type "message_start")
           (let* ((message-obj (gethash "message" data))
                  (usage-obj (when message-obj (gethash "usage" message-obj))))
             (list :event event-type
                   :input-tokens (when usage-obj (gethash "input_tokens" usage-obj)))))
          ((string= event-type "message_delta")
           (let ((usage-obj (gethash "usage" data)))
             (list :event event-type
                   :output-tokens (when usage-obj (gethash "output_tokens" usage-obj)))))
          (t
           (list :event event-type :data data))))
    (error () nil)))

(defun complete-anthropic-streaming (client messages tools)
  "Stream Anthropic responses, invoking *streaming-text-callback* per text delta.
Returns a reconstructed assistant message."
  (multiple-value-bind (system api-messages)
      (messages-to-anthropic-format messages)
    (let* ((body `(("model" . ,(client-model client))
                   ("max_tokens" . ,(client-max-tokens client))
                   ("temperature" . ,(client-temperature client))
                   ("messages" . ,api-messages)
                   ("stream" . t)))
           (body-with-tools (if tools
                                (append body `(("tools" . ,(tools-to-anthropic-format tools))))
                                body))
           (final-body (if system
                           (append `(("system" . ,system)) body-with-tools)
                           body-with-tools))
           (text-parts nil)
           (thinking-parts nil)
           (tool-calls nil)
           (current-tool-id nil)
           (current-tool-name nil)
           (current-tool-input-parts nil)
           (input-tokens-from-start nil)
           (output-tokens-from-delta nil))
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
         (alist-to-hash final-body)
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
                              (funcall *streaming-text-callback* text))))
                         ((string= delta-type "thinking_delta")
                          (let ((thinking (getf parsed :thinking)))
                            (when thinking
                              (push thinking thinking-parts))))
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
                    (setf input-tokens-from-start (getf parsed :input-tokens)))
                   ((string= parsed-event "message_delta")
                    (setf output-tokens-from-delta (getf parsed :output-tokens))))))))
         (lambda () nil))
        (let* ((usage-plist (when (or input-tokens-from-start output-tokens-from-delta)
                              (list :input-tokens (or input-tokens-from-start 0)
                                    :output-tokens (or output-tokens-from-delta 0)
                                    :cache-read-tokens 0
                                    :cache-write-tokens 0))))
          (values
           (assistant-message
            (when text-parts
              (string-join "" (nreverse text-parts)))
            :tool-calls (nreverse tool-calls)
            :thinking (when thinking-parts
                        (string-join "" (nreverse thinking-parts))))
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
          (messages-to-anthropic-format messages)
        (let ((body `(("model" . ,(client-model client))
                      ("max_tokens" . ,(client-max-tokens client))
                      ("temperature" . ,(client-temperature client))
                      ("messages" . ,api-messages))))
          (when system
            (push (cons "system" system) body))
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
          (messages-to-anthropic-format messages)
        (let ((body `(("model" . ,(client-model client))
                      ("max_tokens" . ,(client-max-tokens client))
                      ("temperature" . ,(client-temperature client))
                      ("messages" . ,api-messages)
                      ("tools" . ,(tools-to-anthropic-format tools)))))
          (when system
            (push (cons "system" system) body))
          (let ((response (http-post-json
                           (anthropic-messages-url client)
                           (anthropic-headers client)
                           (alist-to-hash body))))
            (parse-anthropic-response response))))))

;;; ============================================================
;;; OpenAI (GPT)
;;; ============================================================

(defclass openai-client (llm-client)
  ()
  (:default-initargs
   :base-url "https://api.openai.com/v1"
   :model "gpt-5-mini")
  (:documentation "Client for OpenAI's GPT API."))

(defun make-openai-client (&key api-key
                             (model "gpt-5-mini")
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
  "Convert message structs to OpenAI API format."
  (mapcar (lambda (msg)
            (case (message-role msg)
              (:system
               `(("role" . "system") ("content" . ,(message-content msg))))
              (:user
               `(("role" . "user") ("content" . ,(message-content msg))))
              (:assistant
               (if (message-tool-calls msg)
                   `(("role" . "assistant")
                     ("content" . ,(or (message-content msg) :null))
                     ("tool_calls"
                      . ,(mapcar (lambda (tc)
                                   `(("id" . ,(tool-call-id tc))
                                     ("type" . "function")
                                     ("function"
                                      . (("name" . ,(tool-call-name tc))
                                         ("arguments"
                                          . ,(with-output-to-string (s)
                                               (yason:encode
                                                (alist-to-hash
                                                 (tool-call-arguments tc))
                                                s)))))))
                                 (message-tool-calls msg))))
                   `(("role" . "assistant")
                     ("content" . ,(message-content msg)))))
              (:tool
               `(("role" . "tool")
                 ("tool_call_id" . ,(message-tool-call-id msg))
                 ("content" . ,(message-content msg))))))
          messages))

(defun parse-openai-response (response)
  "Parse OpenAI API response into a message struct."
  (let* ((raw-choices (gethash "choices" response))
         (choices (if (listp raw-choices) raw-choices (coerce raw-choices 'list)))
         (first-choice (first choices))
         (msg (gethash "message" first-choice))
         (content (gethash "content" msg))
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
    (assistant-message content :tool-calls (nreverse tool-calls))))

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
                             ("parameters" . ,(getf ts :parameters))))))
                     tools)))
         (body `(("model" . ,(client-model client))
                 ("max_tokens" . ,(client-max-tokens client))
                 ("temperature" . ,(client-temperature client))
                 ("messages" . ,(messages-to-openai-format messages))
                 ("stream" . t)))
         (final-body (if openai-tools
                         (append body `(("tools" . ,openai-tools)))
                         body))
         (text-parts nil)
         (tool-calls nil)
         (current-tool-id nil)
         (current-tool-name nil)
         (current-tool-input-parts nil))
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
                    (delta (when choice (gethash "delta" choice))))
               (when delta
                 (let ((content (gethash "content" delta)))
                   (when (and content (not (equal content :null)))
                     (push content text-parts)
                     (funcall *streaming-text-callback* content)))
                 (let ((tool-calls-delta (gethash "tool_calls" delta)))
                   (when tool-calls-delta
                     (let* ((tc (cond
                                  ((vectorp tool-calls-delta)
                                   (when (> (length tool-calls-delta) 0)
                                     (aref tool-calls-delta 0)))
                                  ((listp tool-calls-delta)
                                   (first tool-calls-delta))))
                            (func (when tc (gethash "function" tc)))
                            (tc-id (when tc (gethash "id" tc)))
                            (tc-name (when func (gethash "name" func)))
                            (tc-args (when func (gethash "arguments" func))))
                       (when tc-id (setf current-tool-id tc-id))
                       (when tc-name (setf current-tool-name tc-name))
                       (when (and tc-args (not (equal tc-args :null)))
                         (push tc-args current-tool-input-parts)))))))
           (error () nil)))
       (lambda ()
         (finalize-tool-call)))
      (assistant-message
       (when text-parts
         (string-join "" (nreverse text-parts)))
       :tool-calls (nreverse tool-calls)))))

(defmethod complete ((client openai-client) messages &key)
  "Send messages to OpenAI GPT API."
  (log-info "llm" "OpenAI complete (model: ~a, streaming: ~a)"
            (client-model client)
            (if *streaming-text-callback* "yes" "no"))
  (if *streaming-text-callback*
      (complete-openai-streaming client messages nil)
      (let* ((body `(("model" . ,(client-model client))
                     ("max_tokens" . ,(client-max-tokens client))
                     ("temperature" . ,(client-temperature client))
                     ("messages" . ,(messages-to-openai-format messages))))
             (response (http-post-json
                        (format nil "~a/chat/completions"
                                (client-base-url client))
                        (openai-headers client)
                        (alist-to-hash body))))
        (parse-openai-response response))))

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
                               ("parameters" . ,(getf ts :parameters))))))
                       tools))
             (body `(("model" . ,(client-model client))
                     ("max_tokens" . ,(client-max-tokens client))
                     ("temperature" . ,(client-temperature client))
                     ("messages" . ,(messages-to-openai-format messages))
                     ("tools" . ,openai-tools)))
             (response (http-post-json
                        (format nil "~a/chat/completions"
                                (client-base-url client))
                        (openai-headers client)
                        (alist-to-hash body))))
        (parse-openai-response response))))
