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

(defvar *current-request-context* nil
  "Current request context as plist. Fields: :request-id :agent-name :step-number :user-input-prefix")

(defvar *request-counter* 0
  "Atomic counter for generating unique request IDs.")

(defvar *component-log-levels* (make-hash-table :test 'equal)
  "Per-component log level overrides. Maps component-name (string) to log-level keyword.")

;;; Log level utilities
(defun log-level-priority (level)
  "Get numeric priority for log level"
  (or (cdr (assoc level *log-levels*))
      (error "Unknown log level: ~a" level)))

(defun should-log-p (level)
  "Check if message at LEVEL should be logged"
  (>= (log-level-priority level)
      (log-level-priority *log-level*)))

(defun component-log-level (component)
  "Get effective log level for COMPONENT. Falls back to *log-level* if not set."
  (let ((normalized (string-downcase component)))
    (or (gethash normalized *component-log-levels*)
        *log-level*)))

(defun (setf component-log-level) (level component)
  "Set log level for a specific COMPONENT."
  (let ((normalized (string-downcase component)))
    (setf (gethash normalized *component-log-levels*) level)))

(defun reset-component-log-levels ()
  "Clear all per-component log level overrides."
  (clrhash *component-log-levels*))

(defun %json-escape (string)
  "Escape special characters for JSON string output."
  (with-output-to-string (out)
    (loop for ch across string do
      (case ch
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (t (write-char ch out))))))

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

(defun %generate-request-id ()
  "Generate a unique request ID using atomic increment."
  (format nil "req-~a" (incf *request-counter*)))

(defmacro with-request-context ((&key request-id agent-name step-number user-input) &body body)
  "Execute BODY with request context bound for log correlation.
   CONTEXT fields: :request-id :agent-name :step-number :user-input-prefix
   Nested calls: explicit values override, unspecified fields inherit from outer context.
   When no :request-id is given and there is no outer context, auto-generates one as req-N."
  (let ((new-ctx (gensym "NEW-CTX"))
        (outer-ctx (gensym "OUTER-CTX")))
    `(let* ((,outer-ctx *current-request-context*)
            (,new-ctx (list ,@(when request-id `(:request-id ,request-id))
                            ,@(when agent-name `(:agent-name ,agent-name))
                            ,@(when step-number `(:step-number ,step-number))
                            ,@(when user-input
                                `(:user-input-prefix
                                  (let ((inp ,user-input))
                                    (if (and inp (> (length inp) 50))
                                        (subseq inp 0 50)
                                        inp))))))
            (*current-request-context*
             (if ,outer-ctx
                 (append ,new-ctx ,outer-ctx)
                 ;; No outer context: ensure request-id is present
                 (if (getf ,new-ctx :request-id)
                     ,new-ctx
                     (list* :request-id (%generate-request-id) ,new-ctx)))))
       ,@body)))

;;; Core logging function
(defun log-message (level component message &rest args)
  "Log a message at the specified level"
  (let* ((component-name (string-downcase (string component)))
         (component-threshold (component-log-level component-name)))
    (when (>= (log-level-priority level)
              (log-level-priority component-threshold))
      (let* ((timestamp (timestamp-now))
             (formatted-message (if args
                                    (apply #'format nil message args)
                                    message))
             (ctx *current-request-context*)
             (request-id (getf ctx :request-id))
             (agent-name (getf ctx :agent-name))
             (step-number (getf ctx :step-number))
             (user-input-prefix (getf ctx :user-input-prefix))
             (standard-context
               (if ctx
                   (with-output-to-string (out)
                     (write-string " [" out)
                     (when request-id
                       (write-string (princ-to-string request-id) out))
                     (when (and request-id step-number)
                       (write-char #\Space out))
                     (when step-number
                       (format out "step:~a" step-number))
                     (write-char #\] out))
                   ""))
             (structured-context
               (if ctx
                   (with-output-to-string (out)
                     (write-string " [" out)
                      (let ((wrote-any nil))
                        (when request-id
                         (write-string (princ-to-string request-id) out)
                         (setf wrote-any t))
                       (when agent-name
                         (when wrote-any
                           (write-char #\Space out))
                         (format out "agent:~a" agent-name)
                         (setf wrote-any t))
                       (when step-number
                         (when wrote-any
                           (write-char #\Space out))
                         (format out "step:~a" step-number)))
                     (write-char #\] out))
                   ""))
             (escaped-component (%json-escape component-name))
             (escaped-message (%json-escape (princ-to-string formatted-message)))
             (json-context
               (when ctx
                 (with-output-to-string (json-ctx)
                   (when request-id
                     (format json-ctx ",\"request-id\":\"~a\"" (%json-escape (princ-to-string request-id))))
                   (when agent-name
                     (format json-ctx ",\"agent-name\":\"~a\"" (%json-escape (princ-to-string agent-name))))
                   (when step-number
                     (format json-ctx ",\"step-number\":~a" step-number))
                   (when user-input-prefix
                     (format json-ctx ",\"user-input-prefix\":\"~a\""
                             (%json-escape (princ-to-string user-input-prefix))))))))
        (case *log-format*
          (:json
           (format *log-stream* "{\"timestamp\":\"~a\",\"level\":\"~a\",\"component\":\"~a\",\"message\":\"~a\"~a}~%"
                   timestamp
                   level
                   escaped-component
                   escaped-message
                   (or json-context "")))
          (:structured
           (format *log-stream* "[~a] ~a ~a~a: ~a~%"
                   timestamp (format-colored-level level) component-name structured-context formatted-message))
          (t
           (format *log-stream* "~a [~a] ~a~a: ~a~%"
                   timestamp (format-colored-level level) component-name standard-context formatted-message)))
        (force-output *log-stream*)))))

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
