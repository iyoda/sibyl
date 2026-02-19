;;;; integration.lisp — Cache integration layer
;;;; Intercepts complete / complete-with-tools via CLOS :around methods.
;;;; Caches LLM responses keyed by (model, temperature, max-tokens, messages, tools).
;;;; Streaming requests bypass the cache.

(in-package #:sibyl.cache)

;;; ============================================================
;;; Global response cache
;;; ============================================================

(defvar *response-cache* nil
  "Global LRU cache instance for LLM responses.
   Lazily initialized by ENSURE-CACHE.")

(defun ensure-cache ()
  "Return *response-cache*, creating it if necessary."
  (or *response-cache*
      (setf *response-cache*
            (make-lru-cache :max-size *cache-max-entries*))))

(defun flush-response-cache ()
  "Flush all entries from the response cache."
  (when *response-cache*
    (cache-flush *response-cache*)))

(defun response-cache-stats ()
  "Return stats plist for the response cache, or NIL if not initialized."
  (when *response-cache*
    (cache-stats *response-cache*)))

;;; ============================================================
;;; Message serialization for cache key
;;; ============================================================

(defun %message-to-cache-alist (msg)
  "Serialize a message struct to a deterministic alist for cache key computation."
  (let ((result (list (cons "role" (symbol-name (message-role msg)))
                      (cons "content" (message-content msg)))))
    ;; Include tool-call-id for :tool role
    (when (message-tool-call-id msg)
      (push (cons "tool_call_id" (message-tool-call-id msg)) result))
    ;; Include tool-calls for :assistant role
    (when (message-tool-calls msg)
      (push (cons "tool_calls"
                  (mapcar (lambda (tc)
                            (list (cons "id"   (tool-call-id tc))
                                  (cons "name" (tool-call-name tc))
                                  (cons "arguments"
                                        (tool-call-arguments tc))))
                          (message-tool-calls msg)))
            result))
     ;; Include thinking if present
     (when (message-thinking msg)
       (push (cons "thinking" (message-thinking msg)) result))
     ;; Include thinking signature if present
     (when (message-thinking-signature msg)
       (push (cons "thinking_signature" (message-thinking-signature msg)) result))
     result))

(defun %tools-to-cache-alist (tools)
  "Normalize a tools schema list for cache key computation.
   TOOLS is a list of tool schema plists or alists as passed to complete-with-tools."
  ;; Tools are already JSON-like alists/plists — just pass through.
  ;; make-cache-key handles recursive normalization.
  tools)

(defun %compute-cache-key (client messages &optional tools)
  "Compute a SHA-256 cache key from CLIENT settings, MESSAGES, and optional TOOLS."
  (let ((request-alist
          (list (cons "model"      (client-model client))
                (cons "max_tokens" (client-max-tokens client))
                (cons "temperature" (client-temperature client))
                (cons "messages"   (mapcar #'%message-to-cache-alist messages)))))
    (when tools
      (push (cons "tools" (%tools-to-cache-alist tools)) request-alist))
    (make-cache-key request-alist)))

;;; ============================================================
;;; Cache intercept logic
;;; ============================================================

(defun %cache-lookup (cache key)
  "Look up KEY in CACHE. Returns (values hit-p message usage) on hit,
   (values nil nil nil) on miss."
  (multiple-value-bind (entry found) (cache-get cache key)
    (if found
        (let ((response (getf entry :response)))
          (values t
                  (getf response :message)
                  (getf response :usage)))
        (values nil nil nil))))

(defun %cache-store (cache key message usage)
  "Store a response (MESSAGE + USAGE) in CACHE under KEY."
  (cache-put cache key
             (list :message message :usage usage)
             :ttl *cache-ttl-seconds*))

;;; ============================================================
;;; :around methods on complete / complete-with-tools
;;; ============================================================

(defmethod complete :around ((client llm-client) messages &key)
  "Cache intercept for COMPLETE.
   Bypasses cache when: cache disabled, streaming active, or cache not initialized."
  (if (or (not *cache-enabled*)
          *streaming-text-callback*)
      ;; Bypass: streaming or disabled
      (call-next-method)
      ;; Cache path
      (let* ((cache (ensure-cache))
             (key   (%compute-cache-key client messages)))
        (multiple-value-bind (hit-p cached-message cached-usage)
            (%cache-lookup cache key)
          (if hit-p
              (progn
                (record-cache-hit)
                (log-debug "cache" "Cache HIT for complete (key ~a...)"
                           (subseq key 0 (min 12 (length key))))
                (values cached-message cached-usage))
              (progn
                (record-cache-miss)
                (multiple-value-bind (message usage) (call-next-method)
                  (log-debug "cache" "Cache MISS for complete (key ~a...)"
                             (subseq key 0 (min 12 (length key))))
                  (%cache-store cache key message usage)
                  (values message usage))))))))

(defmethod complete-with-tools :around ((client llm-client) messages tools &key)
  "Cache intercept for COMPLETE-WITH-TOOLS.
   Bypasses cache when: cache disabled, streaming active, or cache not initialized."
  (if (or (not *cache-enabled*)
          *streaming-text-callback*)
      ;; Bypass: streaming or disabled
      (call-next-method)
      ;; Cache path
      (let* ((cache (ensure-cache))
             (key   (%compute-cache-key client messages tools)))
        (multiple-value-bind (hit-p cached-message cached-usage)
            (%cache-lookup cache key)
          (if hit-p
              (progn
                (record-cache-hit)
                (log-debug "cache" "Cache HIT for complete-with-tools (key ~a...)"
                           (subseq key 0 (min 12 (length key))))
                (values cached-message cached-usage))
              (progn
                (record-cache-miss)
                (multiple-value-bind (message usage) (call-next-method)
                  (log-debug "cache" "Cache MISS for complete-with-tools (key ~a...)"
                             (subseq key 0 (min 12 (length key))))
                  (%cache-store cache key message usage)
                  (values message usage))))))))
