;;;; memory.lisp — Conversation memory and context management
;;;; Manages the sliding context window for the agent.

(in-package #:sibyl.agent)

;;; ============================================================
;;; Memory — wraps conversation with context window management
;;; ============================================================

(defclass memory ()
  ((conversation :initarg :conversation
                 :accessor memory-conversation
                 :initform (make-conversation))
   (max-messages :initarg :max-messages
                 :accessor memory-max-messages
                 :initform 50
                 :type integer)
   (summary      :initarg :summary
                 :accessor memory-summary
                 :initform nil
                 :type (or string null)
                 :documentation "Compressed summary of older messages.")
   (compaction-strategy :initarg :compaction-strategy
                        :accessor memory-compaction-strategy
                        :initform :llm
                        :type keyword
                        :documentation "Compaction strategy: :simple or :llm")
   (compaction-client :initarg :compaction-client
                       :accessor memory-compaction-client
                       :initform nil
                       :documentation "LLM client for :llm compaction strategy (optional).
When nil and strategy is :llm, falls back to :simple text summarization.")
   (compaction-callback :initarg :compaction-callback
                         :accessor memory-compaction-callback
                         :initform nil
                         :type (or function null)
                         :documentation "Optional callback invoked after compaction with the summary text.")
   (context-window-size :initarg :context-window-size
                        :accessor memory-context-window-size
                        :initform nil
                        :type (or integer null)
                        :documentation "Maximum context window size in tokens.
When set, enables token-based proactive compaction.")
   (compaction-threshold :initarg :compaction-threshold
                         :accessor memory-compaction-threshold
                         :initform 0.80
                         :type number
                         :documentation "Token usage ratio (0.0–1.0) at which proactive compaction triggers."))
  (:documentation "Manages conversation history with context window limits."))

(defun make-memory (&key (max-messages 50) (compaction-strategy :llm)
                    context-window-size (compaction-threshold 0.80))
  "Create a new memory instance.
CONTEXT-WINDOW-SIZE enables token-based proactive compaction when non-nil.
COMPACTION-THRESHOLD is the ratio (0.0–1.0) at which compaction triggers."
  (make-instance 'memory
                 :max-messages max-messages
                 :compaction-strategy compaction-strategy
                 :context-window-size context-window-size
                 :compaction-threshold compaction-threshold))

(defgeneric memory-push (memory message)
  (:documentation "Add a message to memory, respecting context limits."))

(defmethod memory-push ((mem memory) message)
  "Add MESSAGE to memory. Triggers compaction if:
1. Message count exceeds max-messages (existing behavior)
2. Estimated token usage exceeds context-window-size * compaction-threshold (new)"
  (conversation-push (memory-conversation mem) message)
  ;; Message-count based compaction (existing)
  (when (> (conversation-length (memory-conversation mem))
           (memory-max-messages mem))
    (memory-compact mem))
  ;; Token-based proactive compaction (new)
  (when (memory-context-window-size mem)
    (let ((estimated (estimate-context-tokens mem))
          (threshold (floor (* (memory-context-window-size mem)
                               (memory-compaction-threshold mem)))))
      (when (> estimated threshold)
        (log-warn "agent" "Token-based compaction: ~a est. tokens > ~a threshold (~d% of ~a)"
                  estimated threshold
                  (round (* 100 (memory-compaction-threshold mem)))
                  (memory-context-window-size mem))
        (memory-compact mem))))
  message)

(defgeneric memory-context-window (memory &key system-prompt)
  (:documentation "Return the current context window as a list of messages."))

(defmethod memory-context-window ((mem memory) &key system-prompt)
  "Build the full context window for LLM submission.
   System prompt is returned as a list of content blocks for caching support.
   Static content (the base system prompt) is always the first block so it
   can be marked cache_control in a later task.  The dynamic summary, which
   changes every compaction, is kept as a separate second block so the first
   block remains cache-stable."
  (let ((messages nil))
    ;; System prompt first
    (when system-prompt
      (let ((content-blocks
              (if (memory-summary mem)
                  ;; Two blocks: static (cacheable) + dynamic summary
                  (list `(("type" . "text") ("text" . ,system-prompt))
                        `(("type" . "text")
                          ("text" . ,(format nil "## Previous conversation summary:~%~a"
                                             (memory-summary mem)))))
                  ;; One block: static only
                  (list `(("type" . "text") ("text" . ,system-prompt))))))
        (push (system-message content-blocks) messages)))
    ;; Current conversation messages
    (setf messages (append messages
                           (conversation-to-list
                            (memory-conversation mem))))
    messages))

;;; ============================================================
;;; Token estimation
;;; ============================================================

(defun %message-estimated-tokens (msg)
  "Estimate token count for a single message.
Uses rough heuristic of ~4 characters per token."
  (let ((total 0))
    ;; Content
    (let ((content (message-content msg)))
      (typecase content
        (string (incf total (ceiling (length content) 4)))
        (list   ; list of content blocks (system message format)
         (dolist (block content)
           (when (consp block)
             (let ((text (cdr (assoc "text" block :test #'string=))))
               (when (and text (stringp text))
                 (incf total (ceiling (length text) 4)))))))))
    ;; Tool calls (assistant messages with tool_use)
    (dolist (tc (message-tool-calls msg))
      (incf total (ceiling (length (tool-call-name tc)) 4))
      (let ((args (tool-call-arguments tc)))
        (when args
          (incf total (ceiling (length (princ-to-string args)) 4)))))
    ;; Thinking (extended thinking content)
    (let ((thinking (message-thinking msg)))
      (when (and thinking (stringp thinking) (plusp (length thinking)))
        (incf total (ceiling (length thinking) 4))))
    total))

(defgeneric estimate-context-tokens (memory &key system-prompt)
  (:documentation "Estimate the total token count of the current context window.
Returns an approximate count using ~4 characters per token heuristic."))

(defmethod estimate-context-tokens ((mem memory) &key system-prompt)
  "Estimate total tokens including system prompt, summary, and all conversation messages."
  (let ((total 0))
    ;; System prompt
    (when (and system-prompt (stringp system-prompt))
      (incf total (ceiling (length system-prompt) 4)))
    ;; Summary
    (when (memory-summary mem)
      (incf total (ceiling (length (memory-summary mem)) 4)))
    ;; Conversation messages
    (dolist (msg (conversation-to-list (memory-conversation mem)))
      (incf total (%message-estimated-tokens msg)))
    total))

;;; ============================================================
;;; Compaction helpers
;;; ============================================================

(defun simple-compaction-summary (mem to-summarize)
  "Create a simple text summary from TO-SUMMARIZE messages, prepending any
existing summary in MEM."
  (format nil "~@[Previous summary: ~a~%~]~
               Compacted ~a messages. Key points:~%~{- [~a] ~a~%~}"
          (memory-summary mem)
          (length to-summarize)
          (loop for msg in to-summarize
                collect (message-role msg)
                collect (truncate-string
                         (or (message-content msg) "(tool call)")
                         80))))

(defgeneric memory-compact (memory)
  (:documentation "Compact older messages into a summary."))

(defmethod memory-compact ((mem memory))
  "Compact older messages into a summary.
Strategy :simple uses text truncation.
Strategy :llm uses LLM summarization; falls back to :simple when no
compaction-client is set.  Adjusts the split point to avoid orphaning
tool_result messages from their corresponding assistant tool_use messages."
  (let* ((messages (conversation-to-list (memory-conversation mem)))
         (total (length messages))
         (keep-count (ceiling (memory-max-messages mem) 2))
         (split-idx (- total keep-count)))
    ;; Guard: can't compact when fewer messages than keep-count
    (when (<= split-idx 0)
      (return-from memory-compact nil))
    ;; Adjust split-idx forward past any :tool messages at the boundary.
    ;; A :tool message (tool_result) requires its preceding assistant
    ;; (tool_use) to be present in the conversation. If the assistant
    ;; was already moved into the summarized portion, its orphaned
    ;; tool_results must also be summarized to maintain API invariants.
    (loop while (and (< split-idx total)
                     (eq :tool (message-role (nth split-idx messages))))
          do (incf split-idx))
    ;; Guard: ensure we keep at least one message
    (when (>= split-idx total)
      (setf split-idx (max 0 (1- total))))
    (let ((to-summarize (subseq messages 0 split-idx))
          (to-keep (subseq messages split-idx)))
      (let ((summary-text
              (if (and (eq :llm (memory-compaction-strategy mem))
                       (memory-compaction-client mem))
                  ;; LLM-based summarization
                  (handler-case
                      (let* ((summary-prompt
                               (format nil
                                       "Summarize the following conversation in 200 tokens or less. ~
Focus on key decisions, facts, and context needed for continuity.~%~%~
~{~a: ~a~^~%~}"
                                       (loop for msg in to-summarize
                                             collect (string (message-role msg))
                                             collect (or (message-content msg) "(tool call)"))))
                             (prompt-messages (list (user-message summary-prompt))))
                        (multiple-value-bind (response usage)
                            (complete (memory-compaction-client mem) prompt-messages)
                          (declare (ignore usage))
                          (or (message-content response)
                              ;; Fallback if LLM returns empty content
                              (simple-compaction-summary mem to-summarize))))
                    (error (e)
                      (log-warn "agent" "Memory compaction failed: ~a, falling back to simple" e)
                      (simple-compaction-summary mem to-summarize)))
                  ;; Simple text summarization (:simple strategy or :llm without client)
                  (simple-compaction-summary mem to-summarize))))
        (setf (memory-summary mem) summary-text)
        ;; Replace conversation with only the most recent messages
        (conversation-clear (memory-conversation mem))
        (dolist (msg to-keep)
          (conversation-push (memory-conversation mem) msg))
        ;; Fire compaction callback if registered
        (when (memory-compaction-callback mem)
          (funcall (memory-compaction-callback mem) summary-text))))))

;;; ============================================================
;;; Conversation sanitizer — repair orphaned tool_use
;;; ============================================================

(defgeneric memory-sanitize (memory)
  (:documentation "Ensure every assistant tool_use has a matching tool_result.
Appends synthetic '[interrupted]' results for any orphaned tool_use IDs.
Returns the number of synthetic results added (0 when conversation is clean)."))

(defmethod memory-sanitize ((mem memory))
  "Scan conversation for orphaned tool_use messages and append synthetic
tool_result messages for each missing ID.  This prevents the Anthropic API
from rejecting the conversation with HTTP 400."
  (let* ((messages (conversation-to-list (memory-conversation mem)))
         ;; Collect all tool_use IDs from assistant messages.
         (use-ids (loop for msg in messages
                        when (and (eq :assistant (message-role msg))
                                  (message-tool-calls msg))
                          append (mapcar #'tool-call-id
                                         (message-tool-calls msg))))
         ;; Collect all tool_result IDs from :tool messages.
         (result-ids (loop for msg in messages
                           when (eq :tool (message-role msg))
                             collect (message-tool-call-id msg)))
         ;; Orphans = use-ids that have no matching result-id.
         (orphans (set-difference use-ids result-ids :test #'string=))
         (count 0))
    (dolist (id orphans count)
      (conversation-push
       (memory-conversation mem)
       (tool-result-message id "[interrupted — no result available]"))
      (incf count))))

;;; ============================================================
;;; Context overflow recovery — Phase 1: Deduplication
;;; ============================================================

(defgeneric memory-deduplicate-tool-results (memory)
  (:documentation "Replace duplicate tool result contents with a short placeholder.
Preserves tool_use/tool_result pairing (IDs are untouched).
Only deduplicates results whose content exceeds 200 characters.
Returns the number of deduplicated messages."))

(defmethod memory-deduplicate-tool-results ((mem memory))
  "Walk messages oldest→newest, replacing duplicate tool result content."
  (let* ((messages (conversation-to-list (memory-conversation mem)))
         (seen (make-hash-table :test 'equal))
         (deduped 0))
    (dolist (msg messages)
      (when (and (eq :tool (message-role msg))
                 (stringp (message-content msg))
                 (> (length (message-content msg)) 200))
        (let ((content (message-content msg)))
          (if (gethash content seen)
              (progn
                (setf (message-content msg) "[duplicate tool output removed]")
                (incf deduped))
              (setf (gethash content seen) t)))))
    (log-info "agent" "Deduplication: removed ~a duplicate(s)" deduped)
    deduped))

;;; ============================================================
;;; Context overflow recovery — Phase 2: Aggressive truncation
;;; ============================================================

(defgeneric memory-truncate-largest-outputs (memory &key target-ratio max-iterations)
  (:documentation "Truncate the largest tool output to TARGET-RATIO of its current size.
Repeats up to MAX-ITERATIONS times, stopping early when no output exceeds
200 characters. Returns the number of truncation passes performed."))

(defmethod memory-truncate-largest-outputs ((mem memory)
                                            &key (target-ratio 0.5)
                                                 (max-iterations 20))
  "Iteratively halve the largest tool result until under budget or exhausted."
  (let ((truncated 0))
    (dotimes (i max-iterations truncated)
      (let* ((messages (conversation-to-list (memory-conversation mem)))
             (max-len 0)
             (max-msg nil))
        ;; Find the largest tool result
        (dolist (msg messages)
          (when (and (eq :tool (message-role msg))
                     (stringp (message-content msg))
                     (> (length (message-content msg)) max-len))
            (setf max-len (length (message-content msg))
                  max-msg msg)))
        ;; Stop if nothing large enough to truncate
        (when (or (null max-msg) (<= max-len 200))
          (return truncated))
        ;; Truncate to target-ratio of current size (minimum 100 chars)
        (let ((new-len (max 100 (floor (* max-len target-ratio)))))
          (setf (message-content max-msg)
                (concatenate 'string
                             (subseq (message-content max-msg) 0 new-len)
                             "... [truncated]"))
          (incf truncated))))
    (log-info "agent" "Truncation: ~a pass(es) performed" truncated)
    truncated))

;;; ============================================================
;;; Reset
;;; ============================================================

(defgeneric memory-reset (memory)
  (:documentation "Clear all memory."))

(defmethod memory-reset ((mem memory))
  "Clear conversation and summary."
  (conversation-clear (memory-conversation mem))
  (setf (memory-summary mem) nil))
