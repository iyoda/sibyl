;;;; spinner.lisp — Background-thread spinner animation for "thinking..." feedback
;;;;
;;;; Provides a braille-dot spinner that runs in a background thread,
;;;; displaying animated feedback during LLM calls.

(in-package #:sibyl.repl.spinner)

;;; ============================================================
;;; Spinner frames (braille dots, hardcoded)
;;; ============================================================

(defparameter *spinner-frames*
  #("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Braille dot animation frames for the spinner.")

;;; ============================================================
;;; Spinner struct
;;; ============================================================

(defstruct spinner
  "Background-thread spinner animation state.
   THREAD     — the bordeaux-threads thread running the animation loop.
   ACTIVE     — T while animation is running, NIL after stop-spinner.
   MESSAGE    — text displayed next to the spinner frame (changeable mid-run).
   STOP-FLAG  — set to T by stop-spinner to signal the thread to exit.
   LOCK       — recursive lock protecting MESSAGE updates.
   OUTPUT     — stream captured at start-spinner time; used by both the thread
                and stop-spinner to write escape sequences.
   START-TIME — internal-real-time at spinner creation, for elapsed display."
  thread
  (active     nil)
  (message    "Thinking...")
  (stop-flag  nil)
  (lock       (bt:make-recursive-lock "spinner-lock"))
  output
  (start-time (get-internal-real-time)))

;;; ============================================================
;;; Public API
;;; ============================================================

(defun spinner-elapsed-seconds (s)
  "Return elapsed seconds since the spinner was started."
  (/ (- (get-internal-real-time) (spinner-start-time s))
     (float internal-time-units-per-second)))

(defun start-spinner (&optional (message "Thinking..."))
  "Start a spinner animation in a background thread.
   Returns the spinner struct.

   *standard-output* is captured at call time — NOT inside the thread body —
   because dynamic bindings are thread-local in Common Lisp.

   The spinner displays elapsed seconds next to the message, updated each frame."
  (let* ((out        *standard-output*)
         (start-time (get-internal-real-time))   ; capture start time in closure
         (s          (make-spinner :message message :output out))
         (thread
           (bt:make-thread
            (lambda ()
              (flet ((%write-safe (stream fmt &rest args)
                       "Write to STREAM, returning T on success or NIL on failure.
                        Catches any stream error (LAYOUT-INVALID, closed stream, etc.)
                        so the spinner thread never crashes the SBCL process."
                       (handler-case
                           (progn (apply #'format stream fmt args)
                                  (force-output stream)
                                  t)
                         (error () nil))))
                (unwind-protect
                     (loop for i = 1 then (mod (1+ i) (length *spinner-frames*))
                           while (not (spinner-stop-flag s))
                           do (let* ((msg     (bt:with-recursive-lock-held ((spinner-lock s))
                                                (spinner-message s)))
                                     (elapsed (/ (- (get-internal-real-time) start-time)
                                                 (float internal-time-units-per-second))))
                                (unless (%write-safe out "~C~C[2K~A ~A (~,1fs)"
                                                    #\Return #\Escape
                                                    (aref *spinner-frames* i)
                                                    msg
                                                    elapsed)
                                  ;; Stream is dead — stop spinning to prevent further errors.
                                  (setf (spinner-stop-flag s) t)
                                  (return)))
                              (loop repeat 2
                                    while (not (spinner-stop-flag s))
                                    do (sleep 0.05)))
                  ;; Cleanup: best-effort line clear; ignore failures.
                  (%write-safe out "~C~C[2K" #\Return #\Escape))))
            :name "sibyl-spinner")))
    ;; Render the first frame immediately so users don't see a blank gap
    ;; before the background thread gets scheduled.
    (handler-case
        (let ((msg (bt:with-recursive-lock-held ((spinner-lock s))
                     (spinner-message s))))
          (format out "~C~C[2K~A ~A (~,1fs)"
                  #\Return #\Escape
                  (aref *spinner-frames* 0)
                  msg
                  0.0)
          (force-output out))
      (error ()
        (setf (spinner-stop-flag s) t)))
    (setf (spinner-thread s) thread
          (spinner-active s) t)
    s))

(defun stop-spinner (s)
  "Stop the spinner animation.
   Sets the stop flag, joins the thread to guarantee all writes
   (including unwind-protect cleanup) have completed, then clears
   the terminal line and marks the spinner inactive."
  (setf (spinner-stop-flag s) t)
  ;; join-thread guarantees the spinner thread has completed all writes
  ;; before we return.  Polling with thread-alive-p left a race window
  ;; where the thread's final escape-sequence write could interleave
  ;; with the caller's next output.
  (let ((thread (spinner-thread s)))
    (when (and thread (bt:thread-alive-p thread))
      (handler-case (bt:join-thread thread)
        (error ()
          ;; Fallback: poll if join fails (thread already joined, etc.)
          (loop repeat 10
                while (bt:thread-alive-p thread)
                do (sleep 0.05))))))
  ;; Belt-and-suspenders: clear line in case unwind-protect already ran.
  ;; Guard against invalid/closed streams — the thread's cleanup may have
  ;; already written this, or the stream may have been invalidated.
  (let ((out (spinner-output s)))
    (handler-case
        (progn (format out "~C~C[2K" #\Return #\Escape)
               (force-output out))
      (error () nil)))
  (setf (spinner-active s) nil)
  s)

(defun spinner-active-p (s)
  "Return T if the spinner thread is currently running, NIL after stop-spinner."
  (spinner-active s))

(defun update-spinner-message (s new-message)
  "Change the message displayed next to the spinner frame while it is running.
   Thread-safe: protected by the spinner's internal lock."
  (bt:with-recursive-lock-held ((spinner-lock s))
    (setf (spinner-message s) new-message))
  s)
