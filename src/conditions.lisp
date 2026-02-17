;;;; conditions.lisp — Condition hierarchy for Sibyl
;;;; Uses CL condition system for structured error handling with restarts.

(in-package #:sibyl.conditions)

;;; ============================================================
;;; Base conditions
;;; ============================================================

(define-condition sibyl-error (error)
  ((message :initarg :message
            :initform "Unknown Sibyl error"
            :reader sibyl-error-message))
  (:report (lambda (c s)
             (format s "Sibyl error: ~a" (sibyl-error-message c)))))

(define-condition sibyl-warning (warning)
  ((message :initarg :message
            :initform ""
            :reader sibyl-warning-message))
  (:report (lambda (c s)
             (format s "Sibyl warning: ~a" (sibyl-warning-message c)))))

;;; ============================================================
;;; LLM conditions
;;; ============================================================

(define-condition llm-error (sibyl-error)
  ()
  (:report (lambda (c s)
             (format s "LLM error: ~a" (sibyl-error-message c)))))

(define-condition llm-api-error (llm-error)
  ((status-code :initarg :status-code
                :initform nil
                :reader llm-error-status-code)
   (body :initarg :body
         :initform nil
         :reader llm-error-body))
  (:report (lambda (c s)
             (format s "LLM API error (HTTP ~a): ~a"
                     (llm-error-status-code c)
                     (sibyl-error-message c)))))

(define-condition llm-rate-limit-error (llm-api-error)
  ((retry-after :initarg :retry-after
                :initform nil
                :reader llm-error-retry-after))
  (:report (lambda (c s)
             (format s "LLM rate limited. Retry after: ~a seconds"
                     (or (llm-error-retry-after c) "unknown")))))

(define-condition llm-invalid-response (llm-error)
  ((raw-response :initarg :raw-response
                 :initform nil
                 :reader llm-error-raw-response))
  (:report (lambda (c s)
             (format s "Invalid LLM response: ~a" (sibyl-error-message c)))))

(define-condition llm-cancelled (llm-error)
  ()
  (:report (lambda (c s)
             (format s "LLM call cancelled: ~a" (sibyl-error-message c)))))

;;; ============================================================
;;; Tool conditions
;;; ============================================================

(define-condition tool-error (sibyl-error)
  ((tool-name :initarg :tool-name
              :initform nil
              :reader tool-error-tool-name))
  (:report (lambda (c s)
             (format s "Tool error [~a]: ~a"
                     (or (tool-error-tool-name c) "unknown")
                     (sibyl-error-message c)))))

(define-condition tool-not-found-error (tool-error)
  ()
  (:report (lambda (c s)
             (format s "Tool not found: ~a" (tool-error-tool-name c)))))

(define-condition tool-execution-error (tool-error)
  ((inner-error :initarg :inner-error
                :initform nil
                :reader tool-error-inner))
  (:report (lambda (c s)
             (format s "Tool execution failed [~a]: ~a"
                     (tool-error-tool-name c)
                     (sibyl-error-message c)))))

(define-condition tool-validation-error (tool-error)
  ((parameter :initarg :parameter
              :initform nil
              :reader tool-error-parameter))
  (:report (lambda (c s)
             (format s "Tool parameter validation failed [~a.~a]: ~a"
                     (tool-error-tool-name c)
                     (or (tool-error-parameter c) "?")
                     (sibyl-error-message c)))))

;;; ============================================================
;;; Config conditions
;;; ============================================================

(define-condition config-error (sibyl-error)
  ()
  (:report (lambda (c s)
             (format s "Config error: ~a" (sibyl-error-message c)))))

(define-condition config-missing-key-error (config-error)
  ((key :initarg :key
        :initform nil
        :reader config-error-key))
  (:report (lambda (c s)
             (format s "Missing config key: ~a" (config-error-key c)))))
