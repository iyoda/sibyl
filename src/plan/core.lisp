;;;; src/tools/planning.lisp — Plan persistence system for Sibyl
;;;;
;;;; Provides a lightweight S-expression-based plan store under
;;;; ~/.sibyl/plans/ (or a caller-supplied directory).
;;;;
;;;; Data model:
;;;;   plan  — top-level plist with :id :title :description :status
;;;;            :created-at :updated-at :phases :notes
;;;;   phase — plist with :id :title :status :tasks
;;;;   task  — plist with :id :title :status :notes
;;;;
;;;; File layout:
;;;;   <dir>/index.lisp          — list of (:id :title :status :updated-at)
;;;;   <dir>/plan-<id>.lisp      — full plan plist

(in-package #:sibyl.plan)

;;; ── Default directory ────────────────────────────────────────────────────

(defparameter *default-plan-directory*
  (merge-pathnames ".sibyl/plans/" (user-homedir-pathname))
  "Default directory for persisted plans.")

;;; ── ID generation ────────────────────────────────────────────────────────

(defun %generate-plan-id ()
  "Generate a unique plan ID using timestamp + random suffix."
  (format nil "plan-~a-~a"
          (get-universal-time)
          (random 1000000)))

(defun %generate-task-id ()
  "Generate a unique task ID."
  (format nil "task-~a-~a"
          (get-universal-time)
          (random 1000000)))

(defun %generate-phase-id ()
  "Generate a sequential phase ID (integer)."
  (get-universal-time))

;;; ── Data constructors ────────────────────────────────────────────────────

(defun make-task (&key title (status :pending) notes)
  "Create a task plist.
TITLE   — required string describing the task.
STATUS  — keyword: :pending :in-progress :done :skipped (default :pending).
NOTES   — optional string."
  (unless (and (stringp title) (string/= title ""))
    (error "make-task: :title must be a non-empty string"))
  (list :id (%generate-task-id)
        :title title
        :status status
        :notes notes))

(defun make-phase (&key title (status :pending) tasks)
  "Create a phase plist.
TITLE  — required string.
STATUS — keyword: :pending :in-progress :completed (default :pending).
TASKS  — list of task plists."
  (unless (and (stringp title) (string/= title ""))
    (error "make-phase: :title must be a non-empty string"))
  (list :id (%generate-phase-id)
        :title title
        :status status
        :tasks (or tasks nil)))

(defun make-plan (&key title description (status :draft) phases notes)
  "Create a plan plist.
TITLE       — required string.
DESCRIPTION — optional string.
STATUS      — keyword: :draft :in-progress :completed :abandoned (default :draft).
PHASES      — list of phase plists.
NOTES       — optional string."
  (unless (and (stringp title) (string/= title ""))
    (error "make-plan: :title must be a non-empty string"))
  (let ((now (sibyl.util:timestamp-now)))
    (list :id (%generate-plan-id)
          :title title
          :description (or description "")
          :status status
          :created-at now
          :updated-at now
          :phases (or phases nil)
          :notes (or notes ""))))

;;; ── Status transitions ───────────────────────────────────────────────────

(defun plan-update-status (plan new-status)
  "Return a copy of PLAN with :status set to NEW-STATUS and :updated-at refreshed."
  (let ((updated (copy-list plan)))
    (setf (getf updated :status) new-status)
    (setf (getf updated :updated-at) (sibyl.util:timestamp-now))
    updated))

(defun %update-task-in-list (tasks task-id new-status)
  "Return a new list of tasks with the matching task updated."
  (mapcar (lambda (task)
            (if (string= (getf task :id) task-id)
                (let ((updated (copy-list task)))
                  (setf (getf updated :status) new-status)
                  updated)
                task))
          tasks))

(defun %update-task-in-phases (phases task-id new-status)
  "Return a new list of phases with the matching task updated."
  (mapcar (lambda (phase)
            (let ((updated-tasks
                   (%update-task-in-list (getf phase :tasks) task-id new-status)))
              (let ((updated-phase (copy-list phase)))
                (setf (getf updated-phase :tasks) updated-tasks)
                updated-phase)))
          phases))

(defun plan-update-task-status (plan task-id new-status)
  "Return a copy of PLAN with the task identified by TASK-ID set to NEW-STATUS.
Also refreshes :updated-at on the plan."
  (let ((updated (copy-list plan)))
    (setf (getf updated :phases)
          (%update-task-in-phases (getf plan :phases) task-id new-status))
    (setf (getf updated :updated-at) (sibyl.util:timestamp-now))
    updated))

;;; ── Serialization ────────────────────────────────────────────────────────

(defun plan->sexp (plan)
  "Convert PLAN plist to a serializable S-expression (a tagged list)."
  ;; We wrap in (:plan ...) so the file is self-describing.
  (list :plan plan))

(defun sexp->plan (sexp)
  "Restore a plan plist from a serialized S-expression produced by plan->sexp."
  (when (and (consp sexp) (eq :plan (first sexp)))
    (second sexp)))

;;; ── Path helpers ─────────────────────────────────────────────────────────

(defun %plan-directory (directory)
  "Resolve DIRECTORY to a pathname, defaulting to *default-plan-directory*."
  (uiop:ensure-directory-pathname
   (or directory *default-plan-directory*)))

(defun %plan-file-path (plan-id directory)
  "Return the pathname for the plan file of PLAN-ID in DIRECTORY."
  (merge-pathnames (format nil "~a.lisp" plan-id)
                   (%plan-directory directory)))

(defun %index-file-path (directory)
  "Return the pathname for the index file in DIRECTORY."
  (merge-pathnames "index.lisp" (%plan-directory directory)))

;;; ── Index management ─────────────────────────────────────────────────────

(defun load-plan-index (&key directory)
  "Load the plan index from DIRECTORY.
Returns a list of plists with :id :title :status :updated-at,
or NIL if the index does not exist."
  (let ((path (%index-file-path directory)))
    (when (probe-file path)
      (handler-case
          (let ((*read-eval* nil))
            (uiop:safe-read-file-form path))
        (error () nil)))))

(defun %save-plan-index (index directory)
  "Write INDEX (list of summary plists) to the index file in DIRECTORY."
  (let ((path (%index-file-path directory)))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (let ((*print-case* :downcase))
        (prin1 index stream)
        (terpri stream)))))

(defun %index-entry (plan)
  "Build a summary plist for the index from a full PLAN plist."
  (list :id (getf plan :id)
        :title (getf plan :title)
        :status (getf plan :status)
        :updated-at (getf plan :updated-at)))

(defun %update-index (plan directory)
  "Add or replace the index entry for PLAN in DIRECTORY."
  (let* ((index (or (load-plan-index :directory directory) nil))
         (plan-id (getf plan :id))
         (new-entry (%index-entry plan))
         (updated-index
          (cons new-entry
                (remove-if (lambda (e) (string= (getf e :id) plan-id))
                           index))))
    (%save-plan-index updated-index directory)))

;;; ── File I/O ─────────────────────────────────────────────────────────────

(defun save-plan (plan &key directory)
  "Persist PLAN to disk in DIRECTORY (default: *default-plan-directory*).
Creates the directory if needed. Updates the index file.
Returns the plan file pathname."
  (let* ((dir (%plan-directory directory))
         (path (%plan-file-path (getf plan :id) directory)))
    (ensure-directories-exist dir)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (let ((*print-case* :downcase)
            (*print-pretty* t))
        (prin1 (plan->sexp plan) stream)
        (terpri stream)))
    (%update-index plan directory)
    path))

(defun load-plan (plan-id &key directory)
  "Load a plan by PLAN-ID from DIRECTORY.
Returns the plan plist, or NIL if not found."
  (let ((path (%plan-file-path plan-id directory)))
    (when (probe-file path)
      (handler-case
          (let ((*read-eval* nil))
            (sexp->plan (uiop:safe-read-file-form path)))
        (error () nil)))))

(defun list-plans (&key directory status)
  "Return a list of full plan plists from DIRECTORY.
If STATUS is non-nil, only plans with that status keyword are returned.
Plans are sorted by :updated-at descending (most recent first)."
  (let* ((index (load-plan-index :directory directory))
         (filtered (if status
                       (remove-if-not
                        (lambda (e) (eq status (getf e :status)))
                        index)
                       index))
         ;; Sort by updated-at descending
         (sorted (sort (copy-list filtered)
                       #'string>
                       :key (lambda (e) (or (getf e :updated-at) "")))))
    ;; Load full plans for each index entry
    (remove nil
            (mapcar (lambda (entry)
                      (load-plan (getf entry :id) :directory directory))
                    sorted))))

;;; ── Plan mutation ─────────────────────────────────────────────────────────

(defun plan-add-phase (plan phase)
  "Return a copy of PLAN with PHASE appended to :phases and :updated-at refreshed."
  (let ((updated (copy-list plan)))
    (setf (getf updated :phases)
          (append (getf plan :phases) (list phase)))
    (setf (getf updated :updated-at) (sibyl.util:timestamp-now))
    updated))

(defun plan-add-task (plan phase-id task)
  "Return a copy of PLAN with TASK appended to the phase identified by PHASE-ID.
Refreshes :updated-at. Signals an error if PHASE-ID is not found."
  (let* ((phases (getf plan :phases))
         (found nil)
         (new-phases
           (mapcar (lambda (phase)
                     (cond
                       ((eql (getf phase :id) phase-id)
                        (setf found t)
                        (let ((updated-phase (copy-list phase)))
                          (setf (getf updated-phase :tasks)
                                (append (getf phase :tasks) (list task)))
                          updated-phase))
                       ((and (stringp phase-id)
                             (stringp (getf phase :id))
                             (string= (getf phase :id) phase-id))
                        (setf found t)
                        (let ((updated-phase (copy-list phase)))
                          (setf (getf updated-phase :tasks)
                                (append (getf phase :tasks) (list task)))
                          updated-phase))
                       (t phase)))
                   phases)))
    (unless found
      (error "Phase not found: ~a" phase-id))
    (let ((updated (copy-list plan)))
      (setf (getf updated :phases) new-phases)
      (setf (getf updated :updated-at) (sibyl.util:timestamp-now))
      updated)))

;;; ── Deletion ─────────────────────────────────────────────────────────────

(defun %remove-from-index (plan-id directory)
  "Remove the entry for PLAN-ID from the index file in DIRECTORY."
  (let* ((index (or (load-plan-index :directory directory) nil))
         (updated (remove-if (lambda (e) (string= (getf e :id) plan-id))
                             index)))
    (%save-plan-index updated directory)))

(defun delete-plan (plan-id &key directory)
  "Delete a plan by PLAN-ID from DIRECTORY.
Removes the plan file and its index entry.
Returns T on success, NIL if not found."
  (let ((path (%plan-file-path plan-id directory)))
    (when (probe-file path)
      (delete-file path)
      (%remove-from-index plan-id (%plan-directory directory))
      t)))

;;; ── Formatting helpers ───────────────────────────────────────────────────

(defun %status-label (status)
  "Return a human-readable label for a status keyword."
  (case status
    (:draft       "Draft")
    (:in-progress "In Progress")
    (:completed   "Completed")
    (:abandoned   "Abandoned")
    (:pending     "Pending")
    (:done        "Done")
    (:skipped     "Skipped")
    (t (string-downcase (symbol-name status)))))

(defun %format-task (task &optional (indent "      "))
  "Format a single task for display."
  (format nil "~a[~a] ~a~@[ — ~a~]"
          indent
          (%status-label (getf task :status))
          (getf task :title)
          (let ((notes (getf task :notes)))
            (when (and notes (string/= notes "")) notes))))

(defun %format-phase (phase)
  "Format a phase and its tasks for display."
  (with-output-to-string (s)
    (format s "    Phase ~a: ~a [~a]~%"
            (getf phase :id)
            (getf phase :title)
            (%status-label (getf phase :status)))
    (dolist (task (getf phase :tasks))
      (format s "~a~%" (%format-task task)))))

(defun format-plan (plan)
  "Return a human-readable string representation of PLAN."
  (with-output-to-string (s)
    (format s "Plan: ~a~%" (getf plan :title))
    (format s "  ID:          ~a~%" (getf plan :id))
    (format s "  Status:      ~a~%" (%status-label (getf plan :status)))
    (format s "  Created:     ~a~%" (getf plan :created-at))
    (format s "  Updated:     ~a~%" (getf plan :updated-at))
    (let ((desc (getf plan :description)))
      (when (and desc (string/= desc ""))
        (format s "  Description: ~a~%" desc)))
    (let ((notes (getf plan :notes)))
      (when (and notes (string/= notes ""))
        (format s "  Notes:       ~a~%" notes)))
    (let ((phases (getf plan :phases)))
      (when phases
        (format s "  Phases (~a):~%" (length phases))
        (dolist (phase phases)
          (write-string (%format-phase phase) s))))))

;;; ── deftool registrations ────────────────────────────────────────────────
;;; These are defined in sibyl.tools package via the tools protocol.
;;; We use a helper macro to stay in sibyl.plan but register into sibyl.tools.

