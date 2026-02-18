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
  (tier-name        "" :type string)
  (timestamp        0  :type integer)          ; universal-time
  ;; Complexity
  (complexity-score nil)                        ; float or nil
  ;; Token deltas (tokens consumed by this task)
  (input-tokens       0 :type integer)
  (output-tokens      0 :type integer)
  (cache-write-tokens 0 :type integer)
  (cache-read-tokens  0 :type integer)
  ;; Cost (USD)
  (actual-cost-usd   0.0d0 :type double-float)
  (baseline-cost-usd 0.0d0 :type double-float)
  (savings-usd       0.0d0 :type double-float)
  (savings-pct       0.0d0 :type double-float))


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

(defun make-task-cost-record-from-delta
    (task-description model-name tier-name complexity-score delta-plist)
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
                         :total 0.0d0))
         (baseline (getf (estimate-baseline-cost-usd
                                            :input-tokens in
                                            :output-tokens out
                                            :cache-write-tokens cw
                                            :cache-read-tokens cr)
                         :total 0.0d0))
         (savings  (- baseline actual))
         (pct      (compute-savings-pct actual baseline)))
    (make-task-cost-record
     :task-description task-description
     :model-name       model-name
     :tier-name        tier-name
     :timestamp        (get-universal-time)
     :complexity-score complexity-score
     :input-tokens       in
     :output-tokens      out
     :cache-write-tokens cw
     :cache-read-tokens  cr
     :actual-cost-usd   (coerce actual 'double-float)
     :baseline-cost-usd (coerce baseline 'double-float)
     :savings-usd       (coerce savings 'double-float)
     :savings-pct       (coerce pct 'double-float))))


;;; ─────────────────────────────────────────────────────────────────────────
;;; Session cost report
;;; ─────────────────────────────────────────────────────────────────────────

(defstruct session-cost-report
  "Aggregated cost report for an entire agent session."
  ;; Raw records
  (records nil :type list)
  ;; Totals
  (total-actual-cost-usd   0.0d0 :type double-float)
  (total-baseline-cost-usd 0.0d0 :type double-float)
  (total-savings-usd       0.0d0 :type double-float)
  (total-savings-pct       0.0d0 :type double-float)
  ;; Token totals
  (total-input-tokens       0 :type integer)
  (total-output-tokens      0 :type integer)
  (total-cache-write-tokens 0 :type integer)
  (total-cache-read-tokens  0 :type integer)
  ;; Tier distribution (alist: tier-name -> count)
  (tier-distribution nil :type list)
  ;; Cache stats
  (cache-hit-rate 0.0d0 :type double-float)
  ;; Task count
  (task-count 0 :type integer))


(defun compute-session-report (records)
  "Build a SESSION-COST-REPORT from a list of TASK-COST-RECORDs."
  (let ((total-actual   0.0d0)
        (total-baseline 0.0d0)
        (total-in  0) (total-out 0) (total-cw 0) (total-cr 0)
        (tier-counts (make-hash-table :test #'equal)))
    (dolist (rec records)
      (incf total-actual   (task-cost-record-actual-cost-usd rec))
      (incf total-baseline (task-cost-record-baseline-cost-usd rec))
      (incf total-in  (task-cost-record-input-tokens rec))
      (incf total-out (task-cost-record-output-tokens rec))
      (incf total-cw  (task-cost-record-cache-write-tokens rec))
      (incf total-cr  (task-cost-record-cache-read-tokens rec))
      (incf (gethash (task-cost-record-tier-name rec) tier-counts 0)))
    (let* ((savings     (- total-baseline total-actual))
           (savings-pct (compute-savings-pct total-actual total-baseline))
           (total-input-for-cache (+ total-in total-cr))
           (cache-hit-rate (if (zerop total-input-for-cache)
                               0.0d0
                               (float (/ total-cr total-input-for-cache) 0.0d0)))
           (tier-dist (loop for k being the hash-keys of tier-counts
                            collect (cons k (gethash k tier-counts)))))
      (make-session-cost-report
       :records                  records
       :total-actual-cost-usd    (coerce total-actual   'double-float)
       :total-baseline-cost-usd  (coerce total-baseline 'double-float)
       :total-savings-usd        (coerce savings        'double-float)
       :total-savings-pct        (coerce savings-pct    'double-float)
       :total-input-tokens       total-in
       :total-output-tokens      total-out
       :total-cache-write-tokens total-cw
       :total-cache-read-tokens  total-cr
       :tier-distribution        tier-dist
       :cache-hit-rate           (coerce cache-hit-rate 'double-float)
       :task-count               (length records)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; A/B testing: provisioning and tier classification
;;; ─────────────────────────────────────────────────────────────────────────

(defstruct provisioning-stats
  "Provisioning classification stats for tier selection."
  (total 0 :type integer)
  (correct 0 :type integer)
  (over 0 :type integer)
  (under 0 :type integer)
  (unknown 0 :type integer)
  (accuracy 0.0d0 :type double-float)
  (over-rate 0.0d0 :type double-float)
  (under-rate 0.0d0 :type double-float))

(defstruct ab-test-report
  "A/B test report for comparing two tier selection strategies."
  (name-a "A" :type string)
  (name-b "B" :type string)
  (stats-a nil)
  (stats-b nil)
  (delta-accuracy 0.0d0 :type double-float)
  (delta-over-rate 0.0d0 :type double-float)
  (delta-under-rate 0.0d0 :type double-float)
  (results-a nil)
  (results-b nil))

(defun tier-rank (tier-name)
  "Return numeric rank for a tier name, or NIL if unknown."
  (cond
    ((string-equal tier-name "light") 1)
    ((string-equal tier-name "medium") 2)
    ((string-equal tier-name "heavy") 3)
    (t nil)))

(defun classify-provisioning (expected-tier predicted-tier)
  "Classify provisioning outcome relative to EXPECTED-TIER."
  (let ((expected (tier-rank expected-tier))
        (predicted (tier-rank predicted-tier)))
    (cond
      ((or (null expected) (null predicted)) :unknown)
      ((= expected predicted) :correct)
      ((> predicted expected) :over)
      (t :under))))

(defun compute-provisioning-stats (results &key (expected-key :expected) (predicted-key :predicted))
  "Compute provisioning stats from RESULTS (plist entries with expected/predicted tiers)."
  (let ((total 0)
        (correct 0)
        (over 0)
        (under 0)
        (unknown 0))
    (dolist (result results)
      (incf total)
      (case (classify-provisioning (getf result expected-key)
                                   (getf result predicted-key))
        (:correct (incf correct))
        (:over (incf over))
        (:under (incf under))
        (t (incf unknown))))
    (let* ((accuracy (if (zerop total) 0.0d0 (/ (float correct 0.0d0) total)))
           (over-rate (if (zerop total) 0.0d0 (/ (float over 0.0d0) total)))
           (under-rate (if (zerop total) 0.0d0 (/ (float under 0.0d0) total))))
      (make-provisioning-stats
       :total total
       :correct correct
       :over over
       :under under
       :unknown unknown
       :accuracy (coerce accuracy 'double-float)
       :over-rate (coerce over-rate 'double-float)
       :under-rate (coerce under-rate 'double-float)))))

(defun evaluate-provisioning (task-set predictor &key (expected-key :expected-tier))
  "Evaluate a PREDICTOR over TASK-SET.
PREDICTOR is a function (task-description entry) -> predicted-tier (and optional score).
Returns two values: results list and provisioning-stats."
  (let ((results '()))
    (dolist (entry task-set)
      (let* ((task-desc (getf entry :task))
             (expected (getf entry expected-key)))
        (multiple-value-bind (predicted score)
            (funcall predictor task-desc entry)
          (let ((result (list :task task-desc
                              :expected expected
                              :predicted predicted)))
            (when score
              (setf result (append result (list :score score))))
            (push result results)))))
    (let ((final-results (nreverse results)))
      (values final-results (compute-provisioning-stats final-results)))))

(defun make-adaptive-tier-predictor (selector)
  "Return a predictor function that uses SELECTOR's complexity analysis."
  (lambda (task-desc entry)
    (declare (ignore entry))
    (let ((analysis (analyze-task-complexity selector task-desc)))
      (values (recommended-tier analysis)
              (complexity-score analysis)))))

(defun make-fixed-tier-predictor (tier-name)
  "Return a predictor function that always yields TIER-NAME."
  (lambda (task-desc entry)
    (declare (ignore task-desc entry))
    tier-name))

(defun run-tier-ab-test (task-set predictor-a predictor-b
                         &key (name-a "A") (name-b "B") (expected-key :expected-tier))
  "Run an A/B test comparing two predictors over TASK-SET."
  (multiple-value-bind (results-a stats-a)
      (evaluate-provisioning task-set predictor-a :expected-key expected-key)
    (multiple-value-bind (results-b stats-b)
        (evaluate-provisioning task-set predictor-b :expected-key expected-key)
      (make-ab-test-report
       :name-a name-a
       :name-b name-b
       :stats-a stats-a
       :stats-b stats-b
       :delta-accuracy (- (provisioning-stats-accuracy stats-a)
                          (provisioning-stats-accuracy stats-b))
       :delta-over-rate (- (provisioning-stats-over-rate stats-a)
                           (provisioning-stats-over-rate stats-b))
       :delta-under-rate (- (provisioning-stats-under-rate stats-a)
                            (provisioning-stats-under-rate stats-b))
       :results-a results-a
       :results-b results-b))))

(defun run-adaptive-vs-baseline-ab-test (selector task-set
                                       &key (baseline-tier "medium")
                                            (name-a "adaptive")
                                            (name-b "baseline")
                                            (expected-key :expected-tier))
  "Compare adaptive tier selection to a fixed baseline tier."
  (run-tier-ab-test task-set
                    (make-adaptive-tier-predictor selector)
                    (make-fixed-tier-predictor baseline-tier)
                    :name-a name-a
                    :name-b name-b
                    :expected-key expected-key))

(defun format-provisioning-stats (stats &optional (stream t))
  "Print provisioning stats to STREAM."
  (format stream
          "Total: ~d | Accuracy: ~,1f%% | Over: ~d (~,1f%%) | Under: ~d (~,1f%%) | Unknown: ~d~%"
          (provisioning-stats-total stats)
          (* 100 (provisioning-stats-accuracy stats))
          (provisioning-stats-over stats)
          (* 100 (provisioning-stats-over-rate stats))
          (provisioning-stats-under stats)
          (* 100 (provisioning-stats-under-rate stats))
          (provisioning-stats-unknown stats)))

(defun format-ab-test-report (report &optional (stream t))
  "Print A/B test report to STREAM."
  (format stream "~%[A/B Test: ~a vs ~a]~%"
          (ab-test-report-name-a report)
          (ab-test-report-name-b report))
  (format stream "~%A: ~a~%" (ab-test-report-name-a report))
  (format-provisioning-stats (ab-test-report-stats-a report) stream)
  (format stream "~%B: ~a~%" (ab-test-report-name-b report))
  (format-provisioning-stats (ab-test-report-stats-b report) stream)
  (format stream "~%Δ Accuracy: ~,1f%% | Δ Over: ~,1f%% | Δ Under: ~,1f%%~%"
          (* 100 (ab-test-report-delta-accuracy report))
          (* 100 (ab-test-report-delta-over-rate report))
          (* 100 (ab-test-report-delta-under-rate report)))
  report)


;;; ─────────────────────────────────────────────────────────────────────────
;;; Report formatting
;;; ─────────────────────────────────────────────────────────────────────────

(defun format-session-report (report &optional (stream t))
  "Print a human-readable session cost report to STREAM."
  (let ((actual   (session-cost-report-total-actual-cost-usd report))
        (baseline (session-cost-report-total-baseline-cost-usd report))
        (savings  (session-cost-report-total-savings-usd report))
        (pct      (session-cost-report-total-savings-pct report))
        (tasks    (session-cost-report-task-count report))
        (tier-dist (session-cost-report-tier-distribution report))
        (cache-hr  (session-cost-report-cache-hit-rate report)))
    (format stream "~%╔══════════════════════════════════════════════╗~%")
    (format stream "║         SESSION COST REPORT                  ║~%")
    (format stream "╠══════════════════════════════════════════════╣~%")
    (format stream "║  Tasks processed : ~4d                       ║~%" tasks)
    (format stream "║  Actual cost     : $~,6f USD               ║~%" actual)
    (format stream "║  Baseline cost   : $~,6f USD               ║~%" baseline)
    (format stream "║  Savings         : $~,6f USD (~,1f%%)        ║~%" savings pct)
    (format stream "║  Cache hit rate  : ~,1f%%                     ║~%" (* 100 cache-hr))
    (format stream "╠══════════════════════════════════════════════╣~%")
    (format stream "║  Tier distribution:                          ║~%")
    (dolist (entry (sort (copy-list tier-dist) #'string< :key #'car))
      (let* ((tier  (car entry))
             (count (cdr entry))
             (bar   (make-string (min count 20) :initial-element #\█)))
        (format stream "║    ~8a : ~3d ~a~%" tier count bar)))
    (format stream "╚══════════════════════════════════════════════╝~%"))
  report)
