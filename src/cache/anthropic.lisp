;;;; anthropic.lisp — Anthropic Messages API cache adapter
;;;; Works with alists (sibyl request format) and hash-tables (yason-parsed responses).

(in-package #:sibyl.cache)

;;; ─────────────────────────────────────────────
;;;  Request normalization
;;; ─────────────────────────────────────────────

(defparameter *anthropic-ephemeral-fields*
  '("user" "metadata" "x-request-id")
  "Anthropic request fields that must not affect the cache key.")

(defun anthropic-normalize-request (request-alist)
  "Return a canonical alist suitable for cache-key generation.
Strips ephemeral / session-specific fields that should not affect caching."
  (remove-if (lambda (pair)
               (member (car pair) *anthropic-ephemeral-fields*
                       :test #'string-equal))
             request-alist))

;;; ─────────────────────────────────────────────
;;;  Response wrapping
;;; ─────────────────────────────────────────────

(defun anthropic-wrap-response (cached-response-string)
  "Re-wrap a cached JSON response string for replay.
Replaces the original id with a fresh one to avoid client-side dedup issues."
  (let* ((parsed (yason:parse cached-response-string :object-as :hash-table))
         (new-id (format nil "msg_cached_~36r" (random (expt 2 64)))))
    (setf (gethash "id" parsed) new-id)
    (with-output-to-string (s)
      (yason:encode parsed s))))

;;; ─────────────────────────────────────────────
;;;  Cache bypass detection
;;; ─────────────────────────────────────────────

(defparameter *anthropic-uncacheable-tool-types*
  '("bash" "computer" "file_write" "text_editor")
  "Tool types that indicate side-effectful operations (bypass cache).")

(defun anthropic-no-cache-p (request-alist headers)
  "Return T if this request must bypass the cache.
Rules:
  - Cache-Control: no-store header present
  - x-no-cache header present
  - tools list contains side-effectful tool types"
  (or (string-equal (cdr (assoc "cache-control" headers :test #'string-equal))
                    "no-store")
      (assoc "x-no-cache" headers :test #'string-equal)
      (let ((tools (cdr (assoc "tools" request-alist :test #'string=))))
        (and tools
             (some (lambda (tool)
                     (let ((type (if (hash-table-p tool)
                                     (gethash "type" tool)
                                     (cdr (assoc "type" tool :test #'string=)))))
                       (member type *anthropic-uncacheable-tool-types*
                               :test #'string-equal)))
                   tools)))))

;;; ─────────────────────────────────────────────
;;;  Server-side cache token extraction
;;; ─────────────────────────────────────────────

(defun anthropic-extract-server-cache-tokens (response)
  "Extract Anthropic server-side cache token counts from a response.
RESPONSE may be a hash-table (yason-parsed) or an alist.
Returns (values cache-read-tokens cache-creation-tokens)."
  (let ((usage (if (hash-table-p response)
                   (gethash "usage" response)
                   (cdr (assoc "usage" response :test #'string=)))))
    (if (hash-table-p usage)
        (values
         (or (gethash "cache_read_input_tokens" usage) 0)
         (or (gethash "cache_creation_input_tokens" usage) 0))
        (values
         (or (cdr (assoc "cache_read_input_tokens" usage :test #'string=)) 0)
         (or (cdr (assoc "cache_creation_input_tokens" usage :test #'string=)) 0)))))
