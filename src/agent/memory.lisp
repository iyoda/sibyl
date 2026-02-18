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
                 :initform 100
                 :type integer)
   (summary      :initarg :summary
                 :accessor memory-summary
                 :initform nil
                 :type (or string null)
                 :documentation "Compressed summary of older messages."))
  (:documentation "Manages conversation history with context window limits."))

(defun make-memory (&key (max-messages 100))
  "Create a new memory instance."
  (make-instance 'memory :max-messages max-messages))

(defgeneric memory-push (memory message)
  (:documentation "Add a message to memory, respecting context limits."))

(defmethod memory-push ((mem memory) message)
  "Add MESSAGE to memory. If over limit, compact older messages."
  (conversation-push (memory-conversation mem) message)
  (when (> (conversation-length (memory-conversation mem))
           (memory-max-messages mem))
    (memory-compact mem))
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

(defgeneric memory-compact (memory)
  (:documentation "Compact older messages into a summary."))

(defmethod memory-compact ((mem memory))
  "Simple compaction: keep last N/2 messages, summarize the rest.
   In a full implementation, this would use the LLM to summarize.
   Adjusts the split point to avoid orphaning tool_result messages
   from their corresponding assistant tool_use messages."
  (let* ((messages (conversation-to-list (memory-conversation mem)))
         (total (length messages))
         (keep-count (ceiling (memory-max-messages mem) 2))
         (split-idx (- total keep-count)))
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
      ;; Simple textual summary (placeholder for LLM-based summarization)
      (let ((summary-text
              (format nil "~@[Previous summary: ~a~%~]~
                           Compacted ~a messages. Key points:~%~{- [~a] ~a~%~}"
                      (memory-summary mem)
                      (length to-summarize)
                      (loop for msg in to-summarize
                            collect (message-role msg)
                            collect (truncate-string
                                     (or (message-content msg) "(tool call)")
                                     80)))))
        (setf (memory-summary mem) summary-text)
        ;; Replace conversation with only recent messages
        (conversation-clear (memory-conversation mem))
        (dolist (msg to-keep)
          (conversation-push (memory-conversation mem) msg))))))

(defgeneric memory-reset (memory)
  (:documentation "Clear all memory."))

(defmethod memory-reset ((mem memory))
  "Clear conversation and summary."
  (conversation-clear (memory-conversation mem))
  (setf (memory-summary mem) nil))
