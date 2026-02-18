;;;; openai.lisp — OpenAI Chat Completions API cache adapter
;;;; Works with alists (sibyl request format) and hash-tables (yason-parsed responses).

(in-package #:sibyl.cache)

;;; ─────────────────────────────────────────────
;;;  Request normalization
;;; ─────────────────────────────────────────────

(defparameter *openai-ephemeral-fields*
  '("user" "n" "stream")
  "OpenAI request fields that must not affect the cache key.")

(defun openai-normalize-request (request-alist)
  "Return a canonical alist for cache-key generation.
Strips ephemeral fields."
  (remove-if (lambda (pair)
               (member (car pair) *openai-ephemeral-fields*
                       :test #'string-equal))
             request-alist))

;;; ─────────────────────────────────────────────
;;;  Response wrapping
;;; ─────────────────────────────────────────────

(defun openai-wrap-response (cached-response-string)
  "Re-wrap a cached JSON response string for replay."
  (let* ((parsed (yason:parse cached-response-string :object-as :hash-table))
         (new-id (format nil "chatcmpl-cached-~36r" (random (expt 2 64)))))
    (setf (gethash "id" parsed) new-id)
    ;; Update created timestamp
    (setf (gethash "created" parsed) (get-universal-time))
    (with-output-to-string (s)
      (yason:encode parsed s))))

;;; ─────────────────────────────────────────────
;;;  Cache bypass detection
;;; ─────────────────────────────────────────────

(defun openai-no-cache-p (request-alist headers)
  "Return T if this request must bypass the cache.
Rules:
  - Cache-Control: no-store header present
  - x-no-cache header present
  - stream is true (streaming responses are not cached)
  - n > 1 (stochastic, don't cache)"
  (or (string-equal (cdr (assoc "cache-control" headers :test #'string-equal))
                    "no-store")
      (assoc "x-no-cache" headers :test #'string-equal)
      ;; Streaming responses
      (cdr (assoc "stream" request-alist :test #'string=))
      ;; n > 1 means multiple completions (stochastic)
      (let ((n (cdr (assoc "n" request-alist :test #'string=))))
        (and n (> n 1)))))

;;; ─────────────────────────────────────────────
;;;  Server-side cache token extraction
;;; ─────────────────────────────────────────────

(defun openai-extract-server-cache-tokens (response)
  "Extract OpenAI server-side cached token counts from a response.
RESPONSE may be a hash-table (yason-parsed) or an alist.
Returns (values cached-tokens 0)."
  (let* ((usage (if (hash-table-p response)
                    (gethash "usage" response)
                    (cdr (assoc "usage" response :test #'string=))))
         (details (if (hash-table-p usage)
                      (gethash "prompt_tokens_details" usage)
                      (cdr (assoc "prompt_tokens_details" usage :test #'string=)))))
    (values
     (if (hash-table-p details)
         (or (gethash "cached_tokens" details) 0)
         (or (cdr (assoc "cached_tokens" details :test #'string=)) 0))
     0)))
