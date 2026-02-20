;;;; display.lisp — Display formatting utilities for the REPL
;;;;
;;;; Provides formatting functions for costs, tokens, bytes, durations,
;;;; and ANSI color/style utilities.

(in-package #:sibyl.repl.display)

;;; ============================================================
;;; Configuration
;;; ============================================================

(defparameter *use-colors* t
  "When T, enable ANSI color codes in output. When NIL, output plain text.")

;;; ============================================================
;;; Formatting functions
;;; ============================================================

(defun format-cost (cost-usd &optional (stream *standard-output*))
  "Format USD cost with 3 decimal places.
   Examples: $0.012, $1.500"
  (format stream "$~,3f" cost-usd))

(defun format-tokens-compact (token-count &optional (stream *standard-output*))
  "Format token count with comma separators.
   Examples: 1,234, 12,345,678"
  (format stream "~:d" token-count))

(defun format-bytes-human (bytes &optional (stream *standard-output*))
  "Format byte count in human-readable form.
   Examples: 500 B, 14.9 KB, 1.0 MB"
  (cond
    ((< bytes 1024)
     (format stream "~d B" bytes))
    ((< bytes (* 1024 1024))
     (format stream "~,1f KB" (/ bytes 1024.0)))
    (t
     (format stream "~,1f MB" (/ bytes (* 1024.0 1024.0))))))

(defun format-duration (seconds &optional (stream *standard-output*))
  "Format duration in seconds.
   Examples: 0.02s, 1.2s, 2m 3s"
  (if (< seconds 60)
      (format stream "~,2fs" seconds)
      (let ((minutes (floor seconds 60))
            (secs (mod seconds 60)))
        (format stream "~dm ~ds" minutes (floor secs)))))

(defun format-context-percentage (percentage &optional (stream *standard-output*))
  "Format context usage percentage with color coding.
   Green: < 60%, Yellow: 60-80%, Red: > 80%"
  (let ((color (cond
                 ((< percentage 60) :green)
                 ((< percentage 80) :yellow)
                 (t :red))))
    (if *use-colors*
        (format-colored (format nil "~d%" (round percentage)) color stream)
        (format stream "~d%" (round percentage)))))

(defun format-separator-line (&optional (width 80) (stream *standard-output*))
  "Format a horizontal separator line using the ─ character."
  (format stream "~v@{~A~:*~}" width "─"))

(defun format-dim-text (text &optional (stream *standard-output*))
  "Format text with dim ANSI styling (ESC[2m).
   Respects *use-colors*."
  (if *use-colors*
      (format stream "~C[2m~A~C[0m" #\Escape text #\Escape)
      (format stream "~A" text)))

(defun format-bold-text (text &optional (stream *standard-output*))
  "Format text with bold ANSI styling (ESC[1m).
   Respects *use-colors*."
  (if *use-colors*
      (format stream "~C[1m~A~C[0m" #\Escape text #\Escape)
      (format stream "~A" text)))

(defun format-colored (text color &optional (stream *standard-output*) &key dim bold)
  "Format text with ANSI color codes and optional modifiers.
   COLOR can be :red, :green, :blue, :yellow, :cyan, :magenta, :white, :black.
   Keyword arguments:
     :dim  — Apply dim styling (ESC[2m)
     :bold — Apply bold styling (ESC[1m)
   Respects *use-colors*."
  (if *use-colors*
      (let ((color-code (case color
                          (:black 30)
                          (:red 31)
                          (:green 32)
                          (:yellow 33)
                          (:blue 34)
                          (:magenta 35)
                          (:cyan 36)
                          (:white 37)
                          (t 37))))
        (format stream "~C[~A~Am~A~C[0m"
                #\Escape
                (cond
                  ((and dim bold) "2;1;")
                  (dim "2;")
                  (bold "1;")
                  (t ""))
                color-code
                text
                #\Escape))
      (format stream "~A" text)))

;;; ============================================================
;;; Tool execution display
;;; ============================================================

(defun %extract-tool-args (tool-call &key (max-length 50))
  "Extract primary argument values from a tool-call for display.
   Returns a string like '(ls -la)' or '(src/repl.lisp)'.
   Used by format-tool-start-line and format-tool-result-line."
  (let* ((args (sibyl.llm:tool-call-arguments tool-call))
         (tool-name (sibyl.llm:tool-call-name tool-call))
         ;; Primary argument keys per tool (priority order)
         (primary-keys
           (cond
             ((string= tool-name "shell")           '("command"))
             ((string= tool-name "read-file")       '("path"))
             ((string= tool-name "write-file")      '("path"))
             ((string= tool-name "eval-form")       '("form"))
             ((string= tool-name "grep")            '("pattern" "path"))
             ((string= tool-name "list-directory")  '("path"))
             ((string= tool-name "file-info")       '("path"))
             ((string= tool-name "read-sexp")       '("path" "name"))
             ((string= tool-name "describe-symbol") '("symbol"))
             ((string= tool-name "who-calls")       '("function"))
             ((string= tool-name "safe-redefine")   '("name"))
             ((string= tool-name "sync-to-file")    '("name" "file"))
             ((string= tool-name "run-tests")       '("suite" "test"))
             ((string= tool-name "write-test")      '("name"))
             ((string= tool-name "add-definition")  '("file"))
             ((string= tool-name "create-module")   '("path"))
             (t nil)))
         ;; Extract values
         (values-to-show
           (if primary-keys
               (remove nil
                 (mapcar (lambda (k)
                           (let ((pair (assoc k args :test #'string=)))
                             (when pair
                               (let ((v (cdr pair)))
                                 (if (stringp v) v (format nil "~a" v))))))
                         primary-keys))
               ;; Fallback: first argument value
               (when args
                 (let ((v (cdr (first args))))
                   (list (if (stringp v) v (format nil "~a" v)))))))
         ;; Join values
         (summary (if values-to-show
                      (format nil "~{~a~^ ~}" values-to-show)
                      "")))
    (cond
      ((string= summary "") "")
      ;; Value already has parentheses (Lisp forms) — display as-is
      ((and (> (length summary) 0) (char= (char summary 0) #\())
       (if (> (length summary) max-length)
           (format nil "~a..." (subseq summary 0 max-length))
           summary))
      ;; Normal case — wrap in parentheses
      (t
       (if (> (length summary) max-length)
           (format nil "(~a...)" (subseq summary 0 max-length))
           (format nil "(~a)" summary))))))

(defun format-tool-start-line (tool-call)
  "Format tool execution start line.
   Returns a string like '🔧 read-file ...' for display during execution."
  (format nil "🔧 ~a ..." (sibyl.llm:tool-call-name tool-call)))

(defun format-tool-result-line (tool-call result elapsed-seconds bytes)
  "Format tool execution result line with timing and size.
   Returns a string with ✓ (success) or ✗ (error), tool name, args, time, and size.
   Examples:
     ✓ read-file (src/repl.lisp) 0.02s 14.9 KB
     ✗ shell (bad-cmd) 1.20s Error

   Error detection: checks only the first 20 characters of result to avoid
   false positives when tool output contains 'Error:' as part of code/text."
  (let* ((tool-name (sibyl.llm:tool-call-name tool-call))
         (is-error (and (stringp result)
                        (let ((limit (min 20 (length result))))
                          (or (search "Error:" result :end2 limit)
                              (search "Error " result :end2 limit)))))
         (icon (if is-error "✗" "✓"))
         (colored-icon (if *use-colors*
                           (with-output-to-string (s)
                             (format-colored icon (if is-error :red :green) s))
                           icon))
         (args-str (%extract-tool-args tool-call))
         (time-str (with-output-to-string (s) (format-duration elapsed-seconds s)))
         (size-str (if (and (not is-error) (> bytes 0))
                       (with-output-to-string (s) (format-bytes-human bytes s))
                       nil)))
    (format nil "~a ~a ~a ~a~@[ ~a~]"
            colored-icon
            tool-name
            args-str
            time-str
            (if is-error "Error" size-str))))

;;; ============================================================
;;; Multi-line turn footer
;;; ============================================================

(defun format-turn-footer (&key (stream *standard-output*)
                                model elapsed-seconds
                                input-tokens output-tokens
                                thinking-tokens
                                cache-read-tokens cache-write-tokens
                                context-percentage
                                cost-usd)
  "Format a rich multi-line turn footer with model stats, tokens, and optional cache info.
   
   Base display (always shown):
     ─────────────────────────────────────────────
     model · duration · cost · context%
     Tokens  In N  Out M
   
   With thinking tokens:
     Tokens  In N  Out M  Thinking T
   
   With cache (when cache-read-tokens > 0):
     Cache   Read N (saved $X.XXX)  Write M
   
   Parameters:
     :model               — Model name string
     :elapsed-seconds     — Duration float
     :input-tokens        — Input token count
     :output-tokens       — Output token count
     :thinking-tokens     — Optional thinking token count (default 0)
     :cache-read-tokens   — Optional cache read tokens (default 0)
     :cache-write-tokens  — Optional cache write tokens (default 0)
     :context-percentage  — Context usage percentage (0-100)
     :cost-usd            — Total cost in USD (nil for Ollama)"
  (let ((thinking-tokens (or thinking-tokens 0))
        (cache-read-tokens (or cache-read-tokens 0))
        (cache-write-tokens (or cache-write-tokens 0)))
    
    ;; Separator line
    (format-separator-line 45 stream)
    (format stream "~%")
    
    ;; First line: model · duration · cost · context%
    (format stream " ~a · " model)
    (format-duration elapsed-seconds stream)
    (when cost-usd
      (format stream " · ")
      (format-cost cost-usd stream))
    (format stream "    ")
    (format-context-percentage context-percentage stream)
    (format stream " mem~%")
    
    ;; Second line: Tokens In/Out (with optional Thinking)
    (format stream " Tokens  In ")
    (format-tokens-compact input-tokens stream)
    (format stream "  Out ")
    (format-tokens-compact output-tokens stream)
    (when (plusp thinking-tokens)
      (format stream "  Thinking ")
      (format-tokens-compact thinking-tokens stream))
    (format stream "~%")
    
    ;; Third line (conditional): Cache info
    (when (plusp cache-read-tokens)
      ;; Calculate cache savings: difference between cache-read cost and regular input cost
      (let* ((pricing (sibyl.llm:lookup-model-pricing model))
             (price-input (getf pricing :input 3.00))
             (price-cr (getf pricing :cache-read 0.30))
             (cache-savings (* cache-read-tokens (/ (- price-input price-cr) 1000000.0d0))))
        (format stream " Cache   Read ")
        (format-tokens-compact cache-read-tokens stream)
        ;; Only show savings if positive
        (when (and cache-savings (plusp cache-savings))
          (format stream " (saved ")
          (format-cost cache-savings stream)
          (format stream ")"))
        (format stream "  Write ")
        (format-tokens-compact cache-write-tokens stream)
        (format stream "~%")))))

;;; ============================================================
;;; Session summary display
;;; ============================================================

(defun format-session-summary (tracker model-name &optional (stream *standard-output*))
  "Format a rich session summary with cumulative cost, cache savings, and token totals.
   
   Display format:
     ═══════════════════════════════════════════════
      Session Summary (model-name)
     ═══════════════════════════════════════════════
      Requests    N
      Total Cost  $X.XXX
     
      Tokens
        Input       N
        Output      M
        Thinking    T  (omitted if 0)
        Cache Read  R  (saved $X.XXX)
        Cache Write W
     
      Cache Performance
        Server Hit Rate  XX.X%
        Response Cache   H / T (XX.X%)
        Entries          E / M
     ═══════════════════════════════════════════════
   
   Parameters:
     tracker     — Token tracker instance
     model-name  — Model name string
     stream      — Output stream (default *standard-output*)"
  (let* ((input-tokens      (sibyl.llm::token-tracker-input-tokens tracker))
         (output-tokens     (sibyl.llm::token-tracker-output-tokens tracker))
         (thinking-tokens   (sibyl.llm::token-tracker-thinking-tokens tracker))
         (cache-read-tokens (sibyl.llm::token-tracker-cache-read-tokens tracker))
         (cache-write-tokens (sibyl.llm::token-tracker-cache-write-tokens tracker))
         (request-count     (sibyl.llm::token-tracker-request-count tracker))
         (total-cost        (sibyl.llm::token-tracker-cost-usd tracker))
         ;; Server cache hit rate
         (server-hit-rate   (sibyl.llm::tracker-cache-hit-rate tracker))
         ;; Cache savings calculation: difference between cache-read cost and regular input cost
         (pricing           (sibyl.llm:lookup-model-pricing model-name))
         (price-input       (getf pricing :input 3.00))
         (price-cr          (getf pricing :cache-read 0.30))
         (cache-savings     (* cache-read-tokens (/ (- price-input price-cr) 1000000.0d0)))
         ;; Response cache stats
         (rcache-stats      (sibyl.cache:response-cache-stats))
         (cache-entries     (if rcache-stats (getf rcache-stats :size) 0))
         (cache-max         (if rcache-stats (getf rcache-stats :max-size) 0))
         (cache-hits        (if rcache-stats (getf rcache-stats :hits) 0))
         (cache-misses      (if rcache-stats (getf rcache-stats :misses) 0))
         (cache-total       (+ cache-hits cache-misses))
         (cache-hit-rate    (if (zerop cache-total) 0.0
                                (float (/ cache-hits cache-total)))))
    
    ;; Top separator
    (format stream "~v@{~A~:*~}~%" 47 "═")
    
    ;; Title line
    (format stream " ")
    (if *use-colors*
        (format-colored "Session Summary" :cyan stream :bold t)
        (format stream "Session Summary"))
    (format stream " (~a)~%" model-name)
    
    ;; Separator
    (format stream "~v@{~A~:*~}~%" 47 "═")
    
    ;; Requests and Total Cost
    (format stream " Requests    ~:d~%" request-count)
    (format stream " Total Cost  ")
    (if *use-colors*
        (format-colored (with-output-to-string (s) (format-cost total-cost s))
                       :green stream)
        (format-cost total-cost stream))
    (format stream "~%~%")
    
    ;; Tokens section
    (format stream " Tokens~%")
    (format stream "   Input       ")
    (format-tokens-compact input-tokens stream)
    (format stream "~%")
    
    (format stream "   Output      ")
    (format-tokens-compact output-tokens stream)
    (format stream "~%")
    
    ;; Thinking tokens (only if > 0)
    (when (plusp thinking-tokens)
      (format stream "   Thinking    ")
      (format-tokens-compact thinking-tokens stream)
      (format stream "~%"))
    
    ;; Cache tokens
    (format stream "   Cache Read  ")
    (format-tokens-compact cache-read-tokens stream)
    ;; Show savings if positive
    (when (and cache-savings (plusp cache-savings))
      (format stream "  (saved ")
      (if *use-colors*
          (format-colored (with-output-to-string (s) (format-cost cache-savings s))
                         :green stream)
          (format-cost cache-savings stream))
      (format stream ")"))
    (format stream "~%")
    
    (format stream "   Cache Write ")
    (format-tokens-compact cache-write-tokens stream)
    (format stream "~%~%")
    
    ;; Cache Performance section
    (format stream " Cache Performance~%")
    (format stream "   Server Hit Rate  ~,1f%~%" (* server-hit-rate 100.0))
    (format stream "   Response Cache   ~:d / ~:d (~,1f%)~%"
            cache-hits cache-total (* cache-hit-rate 100.0))
    (format stream "   Entries          ~:d / ~:d~%"
            cache-entries cache-max)
    
    ;; Bottom separator
    (format stream "~v@{~A~:*~}~%" 47 "═")))
