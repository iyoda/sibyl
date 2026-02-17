;;;; Unified logging system for Sibyl

(in-package #:sibyl.logging)

;;; Log levels
(defparameter *log-levels*
  '((:trace . 0)
    (:debug . 1)
    (:info . 2)
    (:warn . 3)
    (:error . 4)
    (:fatal . 5))
  "Available log levels with numeric priorities")

(defparameter *log-level* :info
  "Current minimum log level")

(defparameter *log-stream* *standard-output*
  "Stream for log output")

(defparameter *log-format* :standard
  "Log format: :standard, :json, or :structured")

(defparameter *enable-colors* t
  "Enable colored output for log levels")

;;; Log level utilities
(defun log-level-priority (level)
  "Get numeric priority for log level"
  (or (cdr (assoc level *log-levels*))
      (error "Unknown log level: ~a" level)))

(defun should-log-p (level)
  "Check if message at LEVEL should be logged"
  (>= (log-level-priority level)
      (log-level-priority *log-level*)))

;;; Color codes for log levels
(defparameter *log-colors*
  '((:trace . 37)   ; white
    (:debug . 36)   ; cyan
    (:info . 32)    ; green
    (:warn . 33)    ; yellow
    (:error . 31)   ; red
    (:fatal . 35))  ; magenta
  "ANSI color codes for log levels")

(defun format-colored-level (level)
  "Format log level with color"
  (if *enable-colors*
      (let ((color-code (cdr (assoc level *log-colors*))))
        (format nil "~C[~Am~5A~C[0m" #\Escape color-code level #\Escape))
      (format nil "~5A" level)))

;;; Core logging function
(defun log-message (level component message &rest args)
  "Log a message at the specified level"
  (when (should-log-p level)
    (let ((timestamp (timestamp-now))
          (formatted-message (if args
                                 (apply #'format nil message args)
                                 message)))
      (case *log-format*
        (:json
         (format *log-stream* "{\"timestamp\":\"~a\",\"level\":\"~a\",\"component\":\"~a\",\"message\":\"~a\"}~%"
                 timestamp level component formatted-message))
        (:structured
         (format *log-stream* "[~a] ~a ~a: ~a~%"
                 timestamp (format-colored-level level) component formatted-message))
        (t ; :standard
         (format *log-stream* "~a [~a] ~a: ~a~%"
                 timestamp (format-colored-level level) component formatted-message)))
      (force-output *log-stream*))))

;;; Convenience logging functions
(defun log-trace (component message &rest args)
  "Log a trace message"
  (apply #'log-message :trace component message args))

(defun log-debug (component message &rest args)
  "Log a debug message"
  (apply #'log-message :debug component message args))

(defun log-info (component message &rest args)
  "Log an info message"
  (apply #'log-message :info component message args))

(defun log-warn (component message &rest args)
  "Log a warning message"
  (apply #'log-message :warn component message args))

(defun log-error (component message &rest args)
  "Log an error message"
  (apply #'log-message :error component message args))

(defun log-fatal (component message &rest args)
  "Log a fatal message"
  (apply #'log-message :fatal component message args))

;;; Utility macros and functions
(defmacro with-logging-context ((component &key (level *log-level*) (stream *log-stream*) (format *log-format*)) &body body)
  "Execute BODY with logging context bound"
  `(let ((*log-level* ,level)
         (*log-stream* ,stream)
         (*log-format* ,format))
     (log-debug ,component "Entering logging context")
     (unwind-protect
         (progn ,@body)
       (log-debug ,component "Exiting logging context"))))

(defmacro log-execution-time ((component operation-name) &body body)
  "Log execution time for BODY"
  (let ((start-time (gensym "START-TIME"))
        (result (gensym "RESULT")))
    `(let ((,start-time (get-internal-real-time)))
       (log-debug ,component "Starting ~a" ,operation-name)
       (let ((,result (progn ,@body)))
         (let ((elapsed (/ (- (get-internal-real-time) ,start-time)
                          internal-time-units-per-second)))
           (log-info ,component "Completed ~a in ~,3f seconds" ,operation-name elapsed))
         ,result))))