;;;; telemetry.lisp — Cache hit/miss statistics
;;;; Thread-safe counters using bordeaux-threads mutex.

(in-package #:sibyl.cache)

;;; ─────────────────────────────────────────────
;;;  Stats structure
;;; ─────────────────────────────────────────────

(defstruct cache-telemetry-stats
  (total-requests      0 :type integer)
  (client-hits         0 :type integer)
  (client-misses       0 :type integer)
  (tokens-saved        0 :type integer)
  (server-cache-tokens 0 :type integer))

(defvar *cache-stats* (make-cache-telemetry-stats))
(defvar *cache-stats-lock* (bt:make-lock "cache-telemetry-lock"))

(defmacro with-cache-stats-lock (&body body)
  `(bt:with-lock-held (*cache-stats-lock*) ,@body))

;;; ─────────────────────────────────────────────
;;;  Recording functions
;;; ─────────────────────────────────────────────

(defun record-cache-hit ()
  "Record a cache hit."
  (with-cache-stats-lock
    (incf (cache-telemetry-stats-total-requests *cache-stats*))
    (incf (cache-telemetry-stats-client-hits    *cache-stats*))))

(defun record-cache-miss ()
  "Record a cache miss."
  (with-cache-stats-lock
    (incf (cache-telemetry-stats-total-requests *cache-stats*))
    (incf (cache-telemetry-stats-client-misses  *cache-stats*))))

(defun record-server-cache-tokens (n)
  "Record N server-side cache tokens (e.g. Anthropic cache_read_input_tokens)."
  (with-cache-stats-lock
    (incf (cache-telemetry-stats-server-cache-tokens *cache-stats*) n)))

;;; ─────────────────────────────────────────────
;;;  Query functions
;;; ─────────────────────────────────────────────

(defun get-cache-telemetry ()
  "Return a snapshot plist of current cache statistics."
  (with-cache-stats-lock
    (list :total-requests      (cache-telemetry-stats-total-requests      *cache-stats*)
          :client-hits         (cache-telemetry-stats-client-hits         *cache-stats*)
          :client-misses       (cache-telemetry-stats-client-misses       *cache-stats*)
          :hit-rate            (let ((total (cache-telemetry-stats-total-requests *cache-stats*)))
                                 (if (zerop total) 0.0
                                     (float (/ (cache-telemetry-stats-client-hits *cache-stats*)
                                               total))))
          :tokens-saved        (cache-telemetry-stats-tokens-saved        *cache-stats*)
          :server-cache-tokens (cache-telemetry-stats-server-cache-tokens *cache-stats*))))

(defun reset-cache-telemetry ()
  "Reset all cache telemetry counters to zero."
  (with-cache-stats-lock
    (setf *cache-stats* (make-cache-telemetry-stats))))
