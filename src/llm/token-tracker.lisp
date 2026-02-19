;;;; token-tracker.lisp — Session-level token usage accumulator

(in-package #:sibyl.llm)

(defstruct token-tracker
  "Accumulates token usage across requests in a session."
  (input-tokens 0 :type integer)
  (output-tokens 0 :type integer)
  (cache-read-tokens 0 :type integer)
  (cache-write-tokens 0 :type integer)
  (request-count 0 :type integer)
  (cost-usd 0.0d0 :type double-float)
  (thinking-tokens 0 :type integer))

(defun tracker-add-usage (tracker usage-plist)
  "Add USAGE-PLIST (from API response) to TRACKER. Safe to call with nil."
  (when (and tracker usage-plist)
    (incf (token-tracker-input-tokens tracker)
          (or (getf usage-plist :input-tokens) 0))
    (incf (token-tracker-output-tokens tracker)
          (or (getf usage-plist :output-tokens) 0))
    (incf (token-tracker-cache-read-tokens tracker)
          (or (getf usage-plist :cache-read-tokens) 0))
    (incf (token-tracker-cache-write-tokens tracker)
          (or (getf usage-plist :cache-write-tokens) 0))
    (incf (token-tracker-thinking-tokens tracker)
          (or (getf usage-plist :thinking-tokens) 0))
    (incf (token-tracker-request-count tracker)))
  tracker)

(defun tracker-add-cost (tracker amount)
  "Add AMOUNT (in USD) to TRACKER's cost accumulator."
  (when tracker
    (incf (token-tracker-cost-usd tracker) amount))
  tracker)

(defun tracker-cache-hit-rate (tracker)
  "Calculate cache hit rate as a float 0.0-1.0. Returns 0.0 if no cached reads."
  (let ((read (token-tracker-cache-read-tokens tracker))
        (regular (token-tracker-input-tokens tracker)))
    (if (zerop (+ read regular))
        0.0
        (float (/ read (+ read regular))))))
