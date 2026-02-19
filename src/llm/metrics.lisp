;;;; metrics.lisp — Cost measurement data structures and session reporting
;;;; Phase 2: Data collection layer
;;;; Captures per-task cost records and aggregates them into session reports.

(in-package #:sibyl.llm)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Per-task cost record
;;; ─────────────────────────────────────────────────────────────────────────

(defstruct task-cost-record
  "Cost record for a single task execution."
  ;; Identity
  (task-description "" :type string)
  (model-name       "" :type string)
  (timestamp        0  :type integer)          ; universal-time
  ;; Token deltas (tokens consumed by this task)
  (input-tokens       0 :type integer)
  (output-tokens      0 :type integer)
  (cache-write-tokens 0 :type integer)
  (cache-read-tokens  0 :type integer)
  ;; Cost (USD)
  (actual-cost-usd   0.0d0 :type double-float))


;;; ─────────────────────────────────────────────────────────────────────────
;;; Token-tracker snapshot helpers
;;; ─────────────────────────────────────────────────────────────────────────

(defun snapshot-tracker (tracker)
  "Return a plist snapshot of TRACKER's current counters."
  (list :input       (token-tracker-input-tokens tracker)
        :output      (token-tracker-output-tokens tracker)
        :cache-write (token-tracker-cache-write-tokens tracker)
        :cache-read  (token-tracker-cache-read-tokens tracker)))


(defun tracker-delta (before-snapshot after-tracker)
  "Compute token deltas between BEFORE-SNAPSHOT (plist) and AFTER-TRACKER.
   Returns a plist with :input :output :cache-write :cache-read."
  (list :input       (- (token-tracker-input-tokens after-tracker)
                        (getf before-snapshot :input 0))
        :output      (- (token-tracker-output-tokens after-tracker)
                        (getf before-snapshot :output 0))
        :cache-write (- (token-tracker-cache-write-tokens after-tracker)
                        (getf before-snapshot :cache-write 0))
        :cache-read  (- (token-tracker-cache-read-tokens after-tracker)
                        (getf before-snapshot :cache-read 0))))


;;; ─────────────────────────────────────────────────────────────────────────
;;; Task cost record constructor
;;; ─────────────────────────────────────────────────────────────────────────

(defun make-task-cost-record-from-delta (task-description model-name delta-plist)
  "Build a TASK-COST-RECORD from a token delta plist.
   DELTA-PLIST has keys :input :output :cache-write :cache-read."
  (let* ((in  (getf delta-plist :input 0))
         (out (getf delta-plist :output 0))
         (cw  (getf delta-plist :cache-write 0))
         (cr  (getf delta-plist :cache-read 0))
         (actual   (getf (estimate-cost-usd model-name
                                            :input-tokens in
                                            :output-tokens out
                                            :cache-write-tokens cw
                                            :cache-read-tokens cr)
                         :total 0.0d0)))
    (make-task-cost-record
     :task-description task-description
     :model-name       model-name
     :timestamp        (get-universal-time)
     :input-tokens       in
     :output-tokens      out
     :cache-write-tokens cw
     :cache-read-tokens  cr
     :actual-cost-usd   (coerce actual 'double-float))))


;;; ─────────────────────────────────────────────────────────────────────────
;;; Session cost report
;;; ─────────────────────────────────────────────────────────────────────────

(defstruct session-cost-report
  "Aggregated cost report for an entire agent session."
  ;; Raw records
  (records nil :type list)
  ;; Totals
  (total-actual-cost-usd   0.0d0 :type double-float)
  ;; Token totals
  (total-input-tokens       0 :type integer)
  (total-output-tokens      0 :type integer)
  (total-cache-write-tokens 0 :type integer)
  (total-cache-read-tokens  0 :type integer)
  ;; Cache stats
  (cache-hit-rate 0.0d0 :type double-float)
  ;; Task count
  (task-count 0 :type integer))


(defun compute-session-report (records)
  "Build a SESSION-COST-REPORT from a list of TASK-COST-RECORDs."
  (let ((total-actual   0.0d0)
        (total-in  0) (total-out 0) (total-cw 0) (total-cr 0))
    (dolist (rec records)
      (incf total-actual   (task-cost-record-actual-cost-usd rec))
      (incf total-in  (task-cost-record-input-tokens rec))
      (incf total-out (task-cost-record-output-tokens rec))
      (incf total-cw  (task-cost-record-cache-write-tokens rec))
      (incf total-cr  (task-cost-record-cache-read-tokens rec)))
    (let* ((total-input-for-cache (+ total-in total-cr))
           (cache-hit-rate (if (zerop total-input-for-cache)
                               0.0d0
                               (float (/ total-cr total-input-for-cache) 0.0d0))))
      (make-session-cost-report
       :records                  records
       :total-actual-cost-usd    (coerce total-actual   'double-float)
       :total-input-tokens       total-in
       :total-output-tokens      total-out
       :total-cache-write-tokens total-cw
       :total-cache-read-tokens  total-cr
       :cache-hit-rate           (coerce cache-hit-rate 'double-float)
       :task-count               (length records)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Report formatting
;;; ─────────────────────────────────────────────────────────────────────────

(defun format-session-report (report &optional (stream t))
  "Print a human-readable session cost report to STREAM."
  (let ((actual   (session-cost-report-total-actual-cost-usd report))
        (tasks    (session-cost-report-task-count report))
        (cache-hr  (session-cost-report-cache-hit-rate report)))
    (format stream "~%╔══════════════════════════════════════════════╗~%")
    (format stream "║         SESSION COST REPORT                  ║~%")
    (format stream "╠══════════════════════════════════════════════╣~%")
    (format stream "║  Tasks processed : ~4d                       ║~%" tasks)
    (format stream "║  Actual cost     : $~,6f USD               ║~%" actual)
    (format stream "║  Cache hit rate  : ~,1f%%                     ║~%" (* 100 cache-hr))
    (format stream "╚══════════════════════════════════════════════╝~%"))
  report)
