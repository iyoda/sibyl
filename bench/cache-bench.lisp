;;;; cache-bench.lisp — Cache effectiveness benchmark
;;;; Usage: sbcl --load bench/cache-bench.lisp

(ql:quickload :sibyl :silent t)

(defpackage #:sibyl.bench
  (:use #:cl))
(in-package #:sibyl.bench)

(format t "~%========================================~%")
(format t " Cache Effectiveness Benchmark~%")
(format t "========================================~%~%")

;;; ============================================================
;;; Mock client with simulated API latency
;;; ============================================================

(defvar *bench-call-count* 0)

(defclass bench-client (sibyl.llm:llm-client) ()
  (:default-initargs :api-key "bench" :model "bench-model"
                     :base-url "http://localhost" :max-tokens 1024 :temperature 0.0))

(defmethod sibyl.llm:complete ((client bench-client) messages &key)
  (incf *bench-call-count*)
  (sleep 0.05)  ; simulate 50ms API round-trip
  (values (sibyl.llm:assistant-message "Benchmark response")
          (list :input-tokens 500 :output-tokens 200)))

(defmethod sibyl.llm:complete-with-tools ((client bench-client) messages tools &key)
  (declare (ignore tools))
  (incf *bench-call-count*)
  (sleep 0.05)
  (values (sibyl.llm:assistant-message "Tool benchmark response")
          (list :input-tokens 800 :output-tokens 300)))

(defun elapsed-sec (start)
  (/ (float (- (get-internal-real-time) start))
     internal-time-units-per-second))

;;; ============================================================
;;; Benchmark 1: Repeated identical requests (complete)
;;; ============================================================

(format t "--- Test 1: Repeated Identical Requests (complete) ---~%")
(let ((sibyl.cache:*response-cache* nil)
      (sibyl.cache:*cache-enabled* t)
      (sibyl.llm:*streaming-text-callback* nil)
      (*bench-call-count* 0))
  (sibyl.cache:reset-cache-telemetry)
  (let* ((client (make-instance 'bench-client))
         (msgs (list (sibyl.llm:user-message "Explain recursion")))
         (n 20)
         (start (get-internal-real-time)))
    (dotimes (_ n) (sibyl.llm:complete client msgs))
    (let* ((elapsed (elapsed-sec start))
           (stats (sibyl.cache:get-cache-telemetry))
           (cstats (sibyl.cache:response-cache-stats)))
      (format t "  Requests:       ~a~%" n)
      (format t "  API calls:      ~a  (expected: 1)~%" *bench-call-count*)
      (format t "  Cache hits:     ~a~%" (getf stats :client-hits))
      (format t "  Cache misses:   ~a~%" (getf stats :client-misses))
      (format t "  Hit rate:       ~,1f%~%" (* 100 (getf stats :hit-rate)))
      (format t "  Wall time:      ~,3fs~%" elapsed)
      (format t "  No-cache time:  ~,3fs  (~a x 50ms)~%" (* n 0.05) n)
      (format t "  Speedup:        ~,1fx~%" (/ (* n 0.05) (max elapsed 0.001)))
      (format t "  Cache entries:  ~a~%~%" (getf cstats :size)))))

;;; ============================================================
;;; Benchmark 2: Mixed unique + repeated (realistic scenario)
;;; ============================================================

(format t "--- Test 2: Mixed Unique + Repeated (complete) ---~%")
(let ((sibyl.cache:*response-cache* nil)
      (sibyl.cache:*cache-enabled* t)
      (sibyl.llm:*streaming-text-callback* nil)
      (*bench-call-count* 0))
  (sibyl.cache:reset-cache-telemetry)
  (let* ((client (make-instance 'bench-client))
         (unique-msgs (loop for i from 1 to 5
                            collect (list (sibyl.llm:user-message
                                          (format nil "Question ~a" i)))))
         (start (get-internal-real-time)))
    ;; Round 1: 5 unique (all miss)
    (dolist (m unique-msgs) (sibyl.llm:complete client m))
    ;; Round 2: 5 repeated (all hit)
    (dolist (m unique-msgs) (sibyl.llm:complete client m))
    ;; Round 3: 5 repeated (all hit)
    (dolist (m unique-msgs) (sibyl.llm:complete client m))
    (let* ((elapsed (elapsed-sec start))
           (stats (sibyl.cache:get-cache-telemetry))
           (total 15))
      (format t "  Unique prompts: 5~%")
      (format t "  Total requests: ~a  (3 rounds x 5)~%" total)
      (format t "  API calls:      ~a  (expected: 5)~%" *bench-call-count*)
      (format t "  Cache hits:     ~a~%" (getf stats :client-hits))
      (format t "  Cache misses:   ~a~%" (getf stats :client-misses))
      (format t "  Hit rate:       ~,1f%~%" (* 100 (getf stats :hit-rate)))
      (format t "  Wall time:      ~,3fs~%" elapsed)
      (format t "  No-cache time:  ~,3fs~%" (* total 0.05))
      (format t "  Speedup:        ~,1fx~%" (/ (* total 0.05) (max elapsed 0.001)))
      (format t "  Tokens saved:   ~a input, ~a output~%~%"
              (* (getf stats :client-hits) 500)
              (* (getf stats :client-hits) 200)))))

;;; ============================================================
;;; Benchmark 3: complete-with-tools
;;; ============================================================

(format t "--- Test 3: Repeated Tool Calls (complete-with-tools) ---~%")
(let ((sibyl.cache:*response-cache* nil)
      (sibyl.cache:*cache-enabled* t)
      (sibyl.llm:*streaming-text-callback* nil)
      (*bench-call-count* 0))
  (sibyl.cache:reset-cache-telemetry)
  (let* ((client (make-instance 'bench-client))
         (msgs (list (sibyl.llm:user-message "Read file.txt")))
         (tools '((("name" . "read-file") ("description" . "Read a file")
                   ("input_schema" . (("type" . "object"))))))
         (n 10)
         (start (get-internal-real-time)))
    (dotimes (_ n) (sibyl.llm:complete-with-tools client msgs tools))
    (let* ((elapsed (elapsed-sec start))
           (stats (sibyl.cache:get-cache-telemetry)))
      (format t "  Requests:       ~a~%" n)
      (format t "  API calls:      ~a  (expected: 1)~%" *bench-call-count*)
      (format t "  Cache hits:     ~a~%" (getf stats :client-hits))
      (format t "  Hit rate:       ~,1f%~%" (* 100 (getf stats :hit-rate)))
      (format t "  Wall time:      ~,3fs~%" elapsed)
      (format t "  No-cache time:  ~,3fs~%" (* n 0.05))
      (format t "  Speedup:        ~,1fx~%~%" (/ (* n 0.05) (max elapsed 0.001))))))

;;; ============================================================
;;; Benchmark 4: Baseline — cache disabled
;;; ============================================================

(format t "--- Test 4: Baseline (cache disabled) ---~%")
(let ((sibyl.cache:*response-cache* nil)
      (sibyl.cache:*cache-enabled* nil)
      (sibyl.llm:*streaming-text-callback* nil)
      (*bench-call-count* 0))
  (sibyl.cache:reset-cache-telemetry)
  (let* ((client (make-instance 'bench-client))
         (msgs (list (sibyl.llm:user-message "Hello")))
         (n 10)
         (start (get-internal-real-time)))
    (dotimes (_ n) (sibyl.llm:complete client msgs))
    (let* ((elapsed (elapsed-sec start))
           (stats (sibyl.cache:get-cache-telemetry)))
      (format t "  Requests:       ~a~%" n)
      (format t "  API calls:      ~a  (all, no cache)~%" *bench-call-count*)
      (format t "  Cache hits:     ~a~%" (getf stats :client-hits))
      (format t "  Wall time:      ~,3fs~%~%" elapsed))))

;;; ============================================================
;;; Benchmark 5: Per-call latency comparison
;;; ============================================================

(format t "--- Test 5: Per-call Latency (cold vs warm) ---~%")
(let ((sibyl.cache:*response-cache* nil)
      (sibyl.cache:*cache-enabled* t)
      (sibyl.llm:*streaming-text-callback* nil)
      (*bench-call-count* 0))
  (sibyl.cache:reset-cache-telemetry)
  (let* ((client (make-instance 'bench-client))
         (msgs (list (sibyl.llm:user-message "Measure me"))))
    ;; Cold call (API)
    (let* ((t0 (get-internal-real-time))
           (_ (sibyl.llm:complete client msgs))
           (cold (elapsed-sec t0)))
      (declare (ignore _))
      ;; Warm calls (cache) — average of 100
      (let* ((t1 (get-internal-real-time))
             (warm-n 1000))
        (dotimes (_ warm-n) (sibyl.llm:complete client msgs))
        (let* ((warm-total (elapsed-sec t1))
               (warm-avg (/ warm-total warm-n)))
          (format t "  Cold call (API):    ~,4fs~%" cold)
          (format t "  Warm call (cache):  ~,7fs  (avg of ~a)~%" warm-avg warm-n)
          (format t "  Latency reduction:  ~,2f%~%" (* 100 (- 1.0 (/ warm-avg (max cold 0.00001)))))
          (format t "  Throughput (cache): ~,0f calls/sec~%~%"
                  (/ 1.0 (max warm-avg 0.0000001))))))))

;;; ============================================================
;;; Summary: Cost estimation for a typical agent session
;;; ============================================================

(format t "--- Estimated Savings for a Typical Agent Session ---~%")
(let* (;; Typical coding agent session parameters
       (total-requests 50)
       (repeat-rate 0.4)          ; 40% of requests are repeats (retry, re-plan)
       (cache-hits (round (* total-requests repeat-rate)))
       (cache-misses (- total-requests cache-hits))
       ;; Claude Sonnet pricing (per 1M tokens)
       (input-price-per-1m 3.0)   ; $3/M input
       (output-price-per-1m 15.0) ; $15/M output
       ;; Average tokens per request
       (avg-input-tokens 2000)
       (avg-output-tokens 800)
       ;; Cost calculation
       (input-tokens-saved (* cache-hits avg-input-tokens))
       (output-tokens-saved (* cache-hits avg-output-tokens))
       (input-cost-saved (* (/ input-tokens-saved 1000000.0) input-price-per-1m))
       (output-cost-saved (* (/ output-tokens-saved 1000000.0) output-price-per-1m))
       (total-cost-saved (+ input-cost-saved output-cost-saved))
       ;; Latency
       (avg-api-latency-sec 2.0)
       (time-saved-sec (* cache-hits avg-api-latency-sec)))
  (format t "  Assumptions:~%")
  (format t "    Requests/session:  ~a~%" total-requests)
  (format t "    Repeat rate:       ~,0f%~%" (* 100 repeat-rate))
  (format t "    Avg input tokens:  ~a~%" avg-input-tokens)
  (format t "    Avg output tokens: ~a~%" avg-output-tokens)
  (format t "    Avg API latency:   ~,1fs~%" avg-api-latency-sec)
  (format t "    Pricing:           $~,2f/M input, $~,2f/M output~%~%" input-price-per-1m output-price-per-1m)
  (format t "  Results:~%")
  (format t "    Cache hits:        ~a / ~a~%" cache-hits total-requests)
  (format t "    API calls saved:   ~a~%" cache-hits)
  (format t "    Input tokens saved:  ~:d~%" input-tokens-saved)
  (format t "    Output tokens saved: ~:d~%" output-tokens-saved)
  (format t "    Cost saved:        $~,4f  (input $~,4f + output $~,4f)~%"
          total-cost-saved input-cost-saved output-cost-saved)
  (format t "    Time saved:        ~,0fs  (~,1f min)~%"
          time-saved-sec (/ time-saved-sec 60.0)))

(format t "~%========================================~%")
(format t " Benchmark complete~%")
(format t "========================================~%")

(sb-ext:exit :code 0)
