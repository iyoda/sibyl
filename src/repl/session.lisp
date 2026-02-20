;;;; session.lisp --- REPL session serialization helpers

(in-package #:sibyl.repl)

(defun %normalize-session-id-suffix (suffix)
  "Normalize SUFFIX into a lowercase kebab-case fragment for session IDs.
Returns NIL when SUFFIX is empty or has no ASCII alnum chars."
  (when (and (stringp suffix) (string/= suffix ""))
    (let ((out (make-string-output-stream))
          (last-was-hyphen nil))
      (labels ((emit-hyphen ()
                 (unless last-was-hyphen
                   (write-char #\- out)
                   (setf last-was-hyphen t)))
               (emit-char (ch)
                 (write-char ch out)
                 (setf last-was-hyphen nil)))
        (loop for ch across (string-downcase suffix) do
          (cond
            ((or (and (char>= ch #\a) (char<= ch #\z))
                 (and (char>= ch #\0) (char<= ch #\9)))
             (emit-char ch))
            ((or (char= ch #\Space)
                 (char= ch #\Tab)
                 (char= ch #\Newline)
                 (char= ch #\_)
                 (char= ch #\-)
                 (char= ch #\/)
                 (char= ch #\.)
                 (char= ch #\+))
             (emit-hyphen)))))
      (let* ((raw (get-output-stream-string out))
             (trimmed (string-trim '(#\-) raw)))
        (when (string/= trimmed "")
          (if (> (length trimmed) 40)
              (subseq trimmed 0 40)
              trimmed))))))

(defun generate-session-id (&key suffix)
  "Generate a session ID in session-YYYYMMDD-HHMMSS-NNNNNN format.
When SUFFIX is provided, append a normalized readable fragment."
  (multiple-value-bind (second minute hour day month year)
      (get-decoded-time)
    (let* ((base (format nil "session-~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d-~6,'0d"
                         year month day hour minute second (random 1000000)))
           (slug (%normalize-session-id-suffix suffix)))
      (if slug
          (format nil "~a-~a" base slug)
          base))))

(defun tool-call->sexp (tool-call)
  "Serialize TOOL-CALL object to a plist."
  (when tool-call
    (list :id (sibyl.llm:tool-call-id tool-call)
          :name (sibyl.llm:tool-call-name tool-call)
          :arguments (sibyl.llm:tool-call-arguments tool-call))))

(defun sexp->tool-call (sexp)
  "Deserialize plist SEXP to a tool-call object."
  (when sexp
    (sibyl.llm:make-tool-call
     :id (or (getf sexp :id) "")
     :name (or (getf sexp :name) "")
     :arguments (getf sexp :arguments))))

(defun message->sexp (message)
  "Serialize MESSAGE struct to a plist for session persistence."
  (when message
    (let ((result (list :role (sibyl.llm:message-role message)
                        :content (sibyl.llm:message-content message)
                        :timestamp (sibyl.llm:message-timestamp message)))
          (tool-calls (sibyl.llm:message-tool-calls message))
          (tool-call-id (sibyl.llm:message-tool-call-id message)))
      (when tool-calls
        (setf result (append result
                             (list :tool-calls
                                   (remove nil
                                           (mapcar #'tool-call->sexp tool-calls))))))
      (when tool-call-id
        (setf result (append result (list :tool-call-id tool-call-id))))
      result)))

(defun sexp->message (sexp)
  "Deserialize plist SEXP back to a message struct."
  (when sexp
    (let ((missing (gensym "MISSING-")))
      (sibyl.llm::%make-message
       :role (or (getf sexp :role) :user)
       :content (let ((value (getf sexp :content missing)))
                  (if (eq value missing) "" value))
       :tool-calls (let ((tool-call-sexps (getf sexp :tool-calls)))
                     (when tool-call-sexps
                       (remove nil (mapcar #'sexp->tool-call tool-call-sexps))))
       :tool-call-id (getf sexp :tool-call-id)
       :timestamp (or (getf sexp :timestamp) "")
       :thinking nil
       :thinking-signature nil))))

(defun session->sexp (session-id messages summary &key (command-count 0))
  "Serialize session values to a tagged session S-expression."
  (let* ((now (sibyl.util:timestamp-now))
         (message-list (remove nil (or messages nil)))
         (persisted-messages
          (remove nil
                  (mapcar #'message->sexp
                          (remove-if (lambda (message)
                                        (eq (sibyl.llm:message-role message) :system))
                                     message-list))))
         (created-at (let ((timestamp (getf (first persisted-messages) :timestamp)))
                       (if (and (stringp timestamp) (plusp (length timestamp)))
                           timestamp
                           now))))
    (list :repl-session
          (list :session-version 1)
          :id session-id
          :created-at created-at
          :last-modified now
          :message-count (length persisted-messages)
          :command-count (or command-count 0)
          :summary summary
          :messages persisted-messages)))

(defun sexp->session (sexp)
  "Deserialize SEXP produced by SESSION->SEXP.
Returns (values messages summary command-count)."
  (labels ((%empty ()
             (values nil nil 0)))
    (if (and (consp sexp) (eq (first sexp) :repl-session))
        (let ((version-form (second sexp))
              (payload (cddr sexp)))
          (if (and (consp version-form)
                   (eq (first version-form) :session-version)
                   (eql (second version-form) 1))
              (let ((message-sexps (getf payload :messages)))
                (values (remove nil
                                (mapcar #'sexp->message
                                        (if (listp message-sexps)
                                            message-sexps
                                            nil)))
                        (getf payload :summary)
                        (or (getf payload :command-count) 0)))
               (%empty)))
        (%empty))))

;;; ============================================================
;;; File I/O — session persistence to disk
;;; ============================================================

(defparameter *default-session-directory*
  (merge-pathnames ".sibyl/sessions/" (user-homedir-pathname))
  "Default directory for persisted sessions.")

(defvar *session-save-lock* (bt:make-lock "session-save")
  "Lock protecting concurrent session file writes.")

(defun %session-directory (&optional directory)
  "Resolve DIRECTORY, defaulting to *default-session-directory*."
  (uiop:ensure-directory-pathname
   (or directory *default-session-directory*)))

(defun %session-file-path (session-id &optional directory)
  "Return pathname for session file: <dir>/<session-id>/session.lisp."
  (merge-pathnames "session.lisp"
                   (merge-pathnames (format nil "~a/" session-id)
                                    (%session-directory directory))))

(defun %index-file-path (&optional directory)
  "Return pathname for the index file: <dir>/index.lisp."
  (merge-pathnames "index.lisp" (%session-directory directory)))

(defun %session-index-entry (session-sexp)
  "Build an index entry plist from a full session sexp."
  (let ((payload (cddr session-sexp)))
    (list :id (getf payload :id)
          :created-at (getf payload :created-at)
          :last-modified (getf payload :last-modified)
          :message-count (or (getf payload :message-count) 0))))

(defun %load-session-index (&key directory)
  "Load the session index file. Returns list of plists or NIL."
  (let ((path (%index-file-path directory)))
    (when (probe-file path)
      (handler-case
          (let ((*read-eval* nil))
            (uiop:safe-read-file-form path))
        (error () nil)))))

(defun %save-session-index (index &key directory)
  "Write INDEX (list of summary plists) to the index file."
  (let ((path (%index-file-path directory)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output :if-exists :supersede
                     :if-does-not-exist :create)
      (let ((*print-case* :downcase))
        (prin1 index stream)
        (terpri stream)))))

(defun %update-session-index (session-id session-sexp &key directory)
  "Add or replace the index entry for SESSION-ID."
  (let* ((index (or (%load-session-index :directory directory) nil))
         (new-entry (%session-index-entry session-sexp))
         (updated (cons new-entry
                        (remove-if (lambda (e) (string= (getf e :id) session-id))
                                   index))))
    (%save-session-index updated :directory directory)))

(defun %rebuild-session-index (&key directory)
  "Scan session directories and rebuild the index from scratch."
  (let* ((dir (%session-directory directory))
         (subdirs (when (probe-file dir) (uiop:subdirectories dir)))
         (entries nil))
    (dolist (subdir subdirs)
      (let ((session-file (merge-pathnames "session.lisp" subdir)))
        (when (probe-file session-file)
          (handler-case
              (let* ((*read-eval* nil)
                     (sexp (uiop:safe-read-file-form session-file)))
                (when (and (consp sexp) (eq (first sexp) :repl-session))
                  (push (%session-index-entry sexp) entries)))
            (error () nil)))))
    (when entries
      (%save-session-index entries :directory directory))
    entries))

(defun save-session (session-id messages summary &key directory (command-count 0))
  "Persist session to disk atomically. Thread-safe via *session-save-lock*.
Returns the session file pathname."
  (bt:with-lock-held (*session-save-lock*)
    (let* ((path (%session-file-path session-id directory))
           (dir (uiop:pathname-directory-pathname path))
           (tmp-path (merge-pathnames "session.tmp" dir))
           (sexp (session->sexp session-id messages summary
                                :command-count command-count)))
      (ensure-directories-exist path)
      ;; Write to temp file first
      (with-open-file (stream tmp-path
                       :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
        (let ((*print-case* :downcase) (*print-pretty* nil))
          (prin1 sexp stream)
          (terpri stream)))
      ;; Atomic rename
      (rename-file tmp-path path)
      ;; Update index
      (%update-session-index session-id sexp :directory directory)
      path)))

(defun load-session (session-id &key directory)
  "Load session by ID.
Returns (values messages summary command-count), or NIL if not found/corrupt."
  (let ((path (%session-file-path session-id directory)))
    (if (probe-file path)
        (handler-case
            (let ((*read-eval* nil))
              (sexp->session (uiop:safe-read-file-form path)))
          (error (e)
            (format *error-output* "~&Warning: corrupt session ~a: ~a~%" session-id e)
            nil))
        (progn
          (format *error-output* "~&Warning: session ~a not found~%" session-id)
          nil))))

(defun list-sessions (&key directory)
  "Return list of session metadata plists from the index.
Rebuilds index from disk if the index file is missing."
  (or (%load-session-index :directory directory)
      (%rebuild-session-index :directory directory)))

;;; ============================================================
;;; Auto-save timer
;;; ============================================================

(defvar *auto-save-thread* nil
  "Background thread for periodic session auto-save, or NIL when not running.")

(defvar *auto-save-stop-flag* nil
  "When T, signals the auto-save thread to exit.")

(defun start-auto-save-timer (get-state-fn interval)
  "Start a background thread that calls SAVE-SESSION every INTERVAL seconds.
GET-STATE-FN is a closure returning (values session-id messages summary command-count).
Uses 1-second granular sleep for responsive shutdown."
  (setf *auto-save-stop-flag* nil)
  (setf *auto-save-thread*
        (bt:make-thread
         (lambda ()
           (loop
             ;; Granular sleep: check stop-flag every second
             (let ((waited 0))
               (loop while (and (not *auto-save-stop-flag*)
                                (< waited interval))
                     do (sleep 1)
                        (incf waited)))
             (when *auto-save-stop-flag* (return))
             ;; Perform auto-save
             (handler-case
                 (multiple-value-bind (session-id messages summary command-count)
                     (funcall get-state-fn)
                   (when (and session-id messages)
                     (save-session session-id messages summary
                                   :command-count (or command-count 0))))
               (error (e)
                 (ignore-errors
                   (format *error-output*
                           "~&Warning: auto-save failed: ~a~%" e))))))
         :name "session-auto-save")))

(defun stop-auto-save-timer ()
  "Stop the auto-save timer thread. Blocks until thread exits (5s timeout)."
  (setf *auto-save-stop-flag* t)
  (when (and *auto-save-thread*
             (bt:thread-alive-p *auto-save-thread*))
    (handler-case
        (bt:join-thread *auto-save-thread*)
      (error () nil)))
  (setf *auto-save-thread* nil))
