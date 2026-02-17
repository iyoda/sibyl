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
   THREAD  — the bordeaux-threads thread running the animation loop.
   ACTIVE  — T while animation is running, NIL after stop-spinner.
   MESSAGE — text displayed next to the spinner frame (changeable mid-run).
   STOP-FLAG — set to T by stop-spinner to signal the thread to exit.
   LOCK    — recursive lock protecting MESSAGE updates.
   OUTPUT  — stream captured at start-spinner time; used by both the thread
              and stop-spinner to write escape sequences."
  thread
  (active    nil)
  (message   "Thinking...")
  (stop-flag nil)
  (lock      (bt:make-recursive-lock "spinner-lock"))
  output)

;;; ============================================================
;;; Public API
;;; ============================================================

(defun start-spinner (&optional (message "Thinking..."))
  "Start a spinner animation in a background thread.
   Returns the spinner struct.

   *standard-output* is captured at call time — NOT inside the thread body —
   because dynamic bindings are thread-local in Common Lisp."
  (let* ((out *standard-output*)          ; capture before thread creation
         (s   (make-spinner :message message :output out))
         (thread
           (bt:make-thread
            (lambda ()
              (unwind-protect
                   ;; Animation loop — runs until stop-flag is set
                   (loop for i = 0 then (mod (1+ i) (length *spinner-frames*))
                         while (not (spinner-stop-flag s))
                         do (let ((msg (bt:with-recursive-lock-held ((spinner-lock s))
                                         (spinner-message s))))
                              (format out "~C~C[2K~A ~A"
                                      #\Return #\Escape
                                      (aref *spinner-frames* i)
                                      msg)
                              (force-output out))
                            ;; Two 50ms sleeps with flag check — keeps stop latency ≤100ms
                            (loop repeat 2
                                  while (not (spinner-stop-flag s))
                                  do (sleep 0.05)))
                ;; Cleanup: always clear the spinner line, even on error
                (format out "~C~C[2K" #\Return #\Escape)
                (force-output out)))
            :name "sibyl-spinner")))
    (setf (spinner-thread s) thread
          (spinner-active s) t)
    s))

(defun stop-spinner (s)
  "Stop the spinner animation.
   Sets the stop flag, waits up to 500ms for the thread to exit,
   then clears the terminal line and marks the spinner inactive."
  (setf (spinner-stop-flag s) t)
  ;; Wait for thread to die — max 10 × 50ms = 500ms
  (loop repeat 10
        while (bt:thread-alive-p (spinner-thread s))
        do (sleep 0.05))
  ;; Belt-and-suspenders: clear line in case unwind-protect already ran
  (let ((out (spinner-output s)))
    (format out "~C~C[2K" #\Return #\Escape)
    (force-output out))
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
