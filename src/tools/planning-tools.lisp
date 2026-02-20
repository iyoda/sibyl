;;;; src/tools/planning-tools.lisp — deftool wrappers for the plan system
;;;;
;;;; Registers save-plan, load-plan, list-plans, update-plan-task
;;;; into the sibyl.tools registry.

(in-package #:sibyl.tools)

;;; ── Helper: resolve plan directory ──────────────────────────────────────

(defun %planning-resolve-dir (args-dir)
  "Return a pathname for the plan directory.
If ARGS-DIR is a non-empty string, use it; otherwise fall back to
sibyl.plan:*default-plan-directory*."
  (if (and args-dir (stringp args-dir) (string/= args-dir ""))
      (uiop:ensure-directory-pathname args-dir)
      sibyl.plan:*default-plan-directory*))

;;; ── save-plan ────────────────────────────────────────────────────────────

(deftool "save-plan"
  (:description
   "Save a development plan to disk for persistence.
Creates a new plan or updates an existing one (when plan-id is supplied).
Returns the plan ID."
   :parameters
   ((:name "title"       :type "string"  :required t   :description "Plan title")
    (:name "description" :type "string"  :required nil :description "Plan description")
    (:name "phases"      :type "string"  :required nil :description
     "S-expression string: list of phase plists (from make-phase)")
    (:name "notes"       :type "string"  :required nil :description "Free-form notes")
    (:name "id-suffix"   :type "string"  :required nil :description
     "Optional short suffix added to new plan IDs (e.g. auto-model-select)")
    (:name "plan-id"     :type "string"  :required nil :description
     "Existing plan ID to update (omit to create new)")
    (:name "plan-dir"    :type "string"  :required nil :description
     "Directory for plan storage (default: ~/.sibyl/plans/)")))
  (block save-plan
    (let* ((title       (getf args :title))
           (description (or (getf args :description) ""))
           (phases-str  (getf args :phases))
           (notes       (or (getf args :notes) ""))
           (id-suffix   (getf args :id-suffix))
           (plan-id     (getf args :plan-id))
           (dir         (%planning-resolve-dir (getf args :plan-dir))))
      (unless (and title (stringp title) (string/= title ""))
        (error "save-plan: :title is required"))
      ;; Parse phases if provided
      (let* ((phases
              (when (and phases-str (stringp phases-str)
                         (string/= (string-trim '(#\  #\Tab #\Newline) phases-str) ""))
                (handler-case
                    (let ((*read-eval* nil))
                      (read-from-string phases-str))
                  (error (e)
                    (error "save-plan: invalid :phases S-expression: ~a" e)))))
             ;; Load existing plan if plan-id given, else create fresh
             (plan
              (if (and plan-id (stringp plan-id) (string/= plan-id ""))
                  (let ((existing (sibyl.plan:load-plan plan-id :directory dir)))
                    (if existing
                        (let ((updated (copy-list existing)))
                          (setf (getf updated :title) title)
                          (setf (getf updated :description) description)
                          (when phases
                            (setf (getf updated :phases) phases))
                          (when (string/= notes "")
                            (setf (getf updated :notes) notes))
                          (setf (getf updated :updated-at)
                                (sibyl.util:timestamp-now))
                          updated)
                        (error "save-plan: plan not found: ~a" plan-id)))
                  (sibyl.plan:make-plan :title title
                                        :description description
                                        :phases phases
                                        :notes notes
                                        :id-suffix id-suffix))))
        (sibyl.plan:save-plan plan :directory dir)
        (format nil "~a" (getf plan :id))))))

;;; ── load-plan ────────────────────────────────────────────────────────────

(deftool "load-plan"
  (:description
   "Load a saved plan by ID and return its details."
   :parameters
   ((:name "plan-id"  :type "string" :required t   :description "Plan ID to load")
    (:name "plan-dir" :type "string" :required nil :description
     "Directory for plan storage (default: ~/.sibyl/plans/)")))
  (block load-plan
    (let* ((plan-id (getf args :plan-id))
           (dir     (%planning-resolve-dir (getf args :plan-dir))))
      (unless (and plan-id (stringp plan-id) (string/= plan-id ""))
        (error "load-plan: :plan-id is required"))
      (let ((plan (sibyl.plan:load-plan plan-id :directory dir)))
        (if plan
            (sibyl.plan:format-plan plan)
            (format nil "Plan not found: ~a" plan-id))))))

;;; ── list-plans ───────────────────────────────────────────────────────────

(deftool "list-plans"
  (:description
   "List all saved plans with their status. Optionally filter by status."
   :parameters
   ((:name "status"   :type "string" :required nil :description
     "Filter by status: draft, in-progress, completed, abandoned")
    (:name "plan-dir" :type "string" :required nil :description
     "Directory for plan storage (default: ~/.sibyl/plans/)")))
  (block list-plans
    (let* ((status-str (getf args :status))
           (dir        (%planning-resolve-dir (getf args :plan-dir)))
           (status-kw
            (when (and status-str (stringp status-str)
                       (string/= status-str ""))
              (intern (string-upcase status-str) :keyword)))
           (plans (sibyl.plan:list-plans :directory dir :status status-kw)))
      (if (null plans)
          (if status-kw
              (format nil "No plans with status '~a'." status-str)
              "No plans saved yet.")
          (with-output-to-string (s)
            (format s "Plans (~a):~%~%" (length plans))
            (dolist (plan plans)
              (format s "  [~a] ~a — ~a~%"
                      (sibyl.plan::%status-label (getf plan :status))
                      (getf plan :id)
                      (getf plan :title))
              (let ((desc (getf plan :description)))
                (when (and desc (string/= desc ""))
                  (format s "       ~a~%" desc)))
              (format s "       Updated: ~a~%~%" (getf plan :updated-at))))))))

;;; ── delete-plan ───────────────────────────────────────────────────────────

(deftool "delete-plan"
  (:description
   "Delete a saved plan by ID. Removes the plan file and its index entry."
   :parameters
   ((:name "plan-id"  :type "string" :required t   :description "Plan ID to delete")
    (:name "plan-dir" :type "string" :required nil :description
     "Directory for plan storage (default: ~/.sibyl/plans/)")))
  (block delete-plan
    (let* ((plan-id (getf args :plan-id))
           (dir     (%planning-resolve-dir (getf args :plan-dir))))
      (unless (and plan-id (stringp plan-id) (string/= plan-id ""))
        (error "delete-plan: :plan-id is required"))
      (if (sibyl.plan:delete-plan plan-id :directory dir)
          (format nil "Plan ~a deleted." plan-id)
          (format nil "Plan not found: ~a" plan-id)))))

;;; ── update-plan-status ───────────────────────────────────────────────────

(deftool "update-plan-status"
  (:description
   "Update the overall status of a plan (not individual tasks)."
   :parameters
   ((:name "plan-id"  :type "string" :required t :description "Plan ID")
    (:name "status"   :type "string" :required t :description
     "New status: draft, in-progress, completed, abandoned")
    (:name "plan-dir" :type "string" :required nil :description
     "Directory for plan storage (default: ~/.sibyl/plans/)")))
  (block update-plan-status
    (let* ((plan-id    (getf args :plan-id))
           (status-str (getf args :status))
           (dir        (%planning-resolve-dir (getf args :plan-dir))))
      (unless (and plan-id (stringp plan-id) (string/= plan-id ""))
        (error "update-plan-status: :plan-id is required"))
      (unless (and status-str (stringp status-str) (string/= status-str ""))
        (error "update-plan-status: :status is required"))
      (let* ((status-kw (intern (string-upcase status-str) :keyword))
             (plan (sibyl.plan:load-plan plan-id :directory dir)))
        (unless plan
          (error "update-plan-status: plan not found: ~a" plan-id))
        (let ((updated (sibyl.plan:plan-update-status plan status-kw)))
          (sibyl.plan:save-plan updated :directory dir)
          (format nil "Plan ~a status updated to ~a." plan-id status-str))))))

;;; ── add-phase ────────────────────────────────────────────────────────────

(deftool "add-phase"
  (:description
   "Add a new phase to an existing plan."
   :parameters
   ((:name "plan-id"  :type "string" :required t :description "Plan ID")
    (:name "title"    :type "string" :required t :description "Phase title")
    (:name "tasks"    :type "string" :required nil :description
     "S-expression string: list of task plists (from make-task)")
    (:name "plan-dir" :type "string" :required nil :description
     "Directory for plan storage (default: ~/.sibyl/plans/)")))
  (block add-phase
    (let* ((plan-id   (getf args :plan-id))
           (title     (getf args :title))
           (tasks-str (getf args :tasks))
           (dir       (%planning-resolve-dir (getf args :plan-dir))))
      (unless (and plan-id (stringp plan-id) (string/= plan-id ""))
        (error "add-phase: :plan-id is required"))
      (unless (and title (stringp title) (string/= title ""))
        (error "add-phase: :title is required"))
      (let* ((tasks (when (and tasks-str (stringp tasks-str)
                               (string/= (string-trim '(#\  #\Tab #\Newline) tasks-str) ""))
                      (handler-case
                          (let ((*read-eval* nil))
                            (read-from-string tasks-str))
                        (error (e)
                          (error "add-phase: invalid :tasks S-expression: ~a" e)))))
             (plan (sibyl.plan:load-plan plan-id :directory dir)))
        (unless plan
          (error "add-phase: plan not found: ~a" plan-id))
        (let* ((phase   (sibyl.plan:make-phase :title title :tasks tasks))
               (updated (sibyl.plan:plan-add-phase plan phase)))
          (sibyl.plan:save-plan updated :directory dir)
          (format nil "Phase '~a' added to plan ~a." title plan-id))))))

;;; ── add-task ─────────────────────────────────────────────────────────────

(deftool "add-task"
  (:description
   "Add a new task to an existing phase within a plan."
   :parameters
   ((:name "plan-id"  :type "string" :required t :description "Plan ID")
    (:name "phase-id" :type "string" :required t :description "Phase ID to add task to")
    (:name "title"    :type "string" :required t :description "Task title")
    (:name "notes"    :type "string" :required nil :description "Optional task notes")
    (:name "plan-dir" :type "string" :required nil :description
     "Directory for plan storage (default: ~/.sibyl/plans/)")))
  (block add-task
    (let* ((plan-id   (getf args :plan-id))
           (phase-str (getf args :phase-id))
           (title     (getf args :title))
           (notes     (getf args :notes))
           (dir       (%planning-resolve-dir (getf args :plan-dir))))
      (unless (and plan-id (stringp plan-id) (string/= plan-id ""))
        (error "add-task: :plan-id is required"))
      (unless (and phase-str (stringp phase-str) (string/= phase-str ""))
        (error "add-task: :phase-id is required"))
      (unless (and title (stringp title) (string/= title ""))
        (error "add-task: :title is required"))
      (let ((plan (sibyl.plan:load-plan plan-id :directory dir)))
        (unless plan
          (error "add-task: plan not found: ~a" plan-id))
        ;; phase-id may be integer (stored) or string (from tool arg)
        (let* ((phase-id (handler-case (parse-integer phase-str)
                           (error () phase-str)))
               (task    (sibyl.plan:make-task :title title :notes notes))
               (updated (sibyl.plan:plan-add-task plan phase-id task)))
          (sibyl.plan:save-plan updated :directory dir)
          (format nil "Task '~a' added to phase ~a in plan ~a." title phase-str plan-id))))))

;;; ── update-plan-task ─────────────────────────────────────────────────────

(deftool "update-plan-task"
  (:description
   "Update the status of a specific task within a saved plan."
   :parameters
   ((:name "plan-id"  :type "string" :required t :description "Plan ID")
    (:name "task-id"  :type "string" :required t :description "Task ID to update")
    (:name "status"   :type "string" :required t :description
     "New status: pending, in-progress, done, skipped")
    (:name "plan-dir" :type "string" :required nil :description
     "Directory for plan storage (default: ~/.sibyl/plans/)")))
  (block update-plan-task
    (let* ((plan-id    (getf args :plan-id))
           (task-id    (getf args :task-id))
           (status-str (getf args :status))
           (dir        (%planning-resolve-dir (getf args :plan-dir))))
      (unless (and plan-id (stringp plan-id) (string/= plan-id ""))
        (error "update-plan-task: :plan-id is required"))
      (unless (and task-id (stringp task-id) (string/= task-id ""))
        (error "update-plan-task: :task-id is required"))
      (unless (and status-str (stringp status-str) (string/= status-str ""))
        (error "update-plan-task: :status is required"))
      (let* ((status-kw (intern (string-upcase status-str) :keyword))
             (plan (sibyl.plan:load-plan plan-id :directory dir)))
        (unless plan
          (error "update-plan-task: plan not found: ~a" plan-id))
        (let ((updated (sibyl.plan:plan-update-task-status plan task-id status-kw)))
          (sibyl.plan:save-plan updated :directory dir)
          (format nil "Task ~a updated to ~a in plan ~a."
                  task-id status-str plan-id))))))
