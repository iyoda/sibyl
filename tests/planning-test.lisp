;;;; tests/planning-test.lisp — Tests for the plan persistence system

(in-package #:sibyl.tests)

(def-suite planning-tests
  :description "Tests for the plan persistence system."
  :in sibyl-tests)

(in-suite planning-tests)

;;; ── Helpers ──────────────────────────────────────────────────────────────

(defun make-temp-plan-dir ()
  "Return a temporary directory path for plan tests."
  (let ((path (merge-pathnames
               (format nil ".sibyl-test-plans-~a/" (get-universal-time))
               (uiop:temporary-directory))))
    (ensure-directories-exist path)
    path))

(defun cleanup-plan-dir (dir)
  "Remove a temporary plan directory."
  (when (probe-file dir)
    (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))

;;; ── Unit: plan data structure ────────────────────────────────────────────

(test plan-make-basic
  "make-plan returns a plist with required fields."
  (let ((plan (sibyl.plan:make-plan :title "Test Plan"
                                    :description "A test plan")))
    (is (stringp (getf plan :id)))
    (is (string= "Test Plan" (getf plan :title)))
    (is (string= "A test plan" (getf plan :description)))
    (is (eq :draft (getf plan :status)))
    (is (stringp (getf plan :created-at)))
    (is (stringp (getf plan :updated-at)))
    (is (null (getf plan :phases)))))

(test plan-make-with-phases
  "make-plan accepts :phases keyword."
  (let* ((task  (sibyl.plan:make-task :title "Write tests"))
         (phase (sibyl.plan:make-phase :title "Phase 1" :tasks (list task)))
         (plan  (sibyl.plan:make-plan :title "Plan with phases"
                                      :phases (list phase))))
    (is (= 1 (length (getf plan :phases))))
    (is (string= "Phase 1" (getf (first (getf plan :phases)) :title)))))

(test plan-make-task
  "make-task returns a plist with required fields."
  (let ((task (sibyl.plan:make-task :title "Implement feature")))
    (is (stringp (getf task :id)))
    (is (string= "Implement feature" (getf task :title)))
    (is (eq :pending (getf task :status)))))

(test plan-make-phase
  "make-phase returns a plist with required fields."
  (let ((phase (sibyl.plan:make-phase :title "Phase 1")))
    (is (integerp (getf phase :id)))
    (is (string= "Phase 1" (getf phase :title)))
    (is (eq :pending (getf phase :status)))
    (is (null (getf phase :tasks)))))

;;; ── Unit: status transitions ─────────────────────────────────────────────

(test plan-update-status
  "plan-update-status returns updated plan with new status."
  (let* ((plan    (sibyl.plan:make-plan :title "Test"))
         (updated (sibyl.plan:plan-update-status plan :in-progress)))
    (is (eq :in-progress (getf updated :status)))
    (is (stringp (getf updated :updated-at)))))

(test plan-update-task-status
  "plan-update-task-status marks a task done."
  (let* ((task    (sibyl.plan:make-task :title "Task A"))
         (task-id (getf task :id))
         (phase   (sibyl.plan:make-phase :title "P1" :tasks (list task)))
         (plan    (sibyl.plan:make-plan :title "P" :phases (list phase)))
         (updated (sibyl.plan:plan-update-task-status plan task-id :done)))
    (let* ((phases (getf updated :phases))
           (tasks  (getf (first phases) :tasks))
           (found  (find task-id tasks
                         :key (lambda (tk) (getf tk :id))
                         :test #'string=)))
      (is (not (null found)))
      (is (eq :done (getf found :status))))))

;;; ── Unit: serialization ──────────────────────────────────────────────────

(test plan-serialize-roundtrip
  "plan->sexp and sexp->plan are inverse operations."
  (let* ((task     (sibyl.plan:make-task :title "T1"))
         (phase    (sibyl.plan:make-phase :title "P1" :tasks (list task)))
         (plan     (sibyl.plan:make-plan :title "Roundtrip"
                                         :description "desc"
                                         :phases (list phase)))
         (sexp     (sibyl.plan:plan->sexp plan))
         (restored (sibyl.plan:sexp->plan sexp)))
    (is (string= (getf plan :id)          (getf restored :id)))
    (is (string= (getf plan :title)       (getf restored :title)))
    (is (string= (getf plan :description) (getf restored :description)))
    (is (eq      (getf plan :status)      (getf restored :status)))
    (is (= 1 (length (getf restored :phases))))))

;;; ── Unit: file I/O ───────────────────────────────────────────────────────

(test plan-save-and-load
  "save-plan writes to disk; load-plan reads it back."
  (let* ((dir     (make-temp-plan-dir))
         (plan    (sibyl.plan:make-plan :title "Saved Plan"
                                        :description "Persisted"))
         (plan-id (getf plan :id)))
    (unwind-protect
        (progn
          (sibyl.plan:save-plan plan :directory dir)
          (let ((loaded (sibyl.plan:load-plan plan-id :directory dir)))
            (is (not (null loaded)))
            (is (string= plan-id (getf loaded :id)))
            (is (string= "Saved Plan" (getf loaded :title)))))
      (cleanup-plan-dir dir))))

(test plan-list-plans
  "list-plans returns all saved plans."
  (let* ((dir    (make-temp-plan-dir))
         (plan-a (sibyl.plan:make-plan :title "Plan A"))
         (plan-b (sibyl.plan:make-plan :title "Plan B")))
    (unwind-protect
        (progn
          (sibyl.plan:save-plan plan-a :directory dir)
          (sibyl.plan:save-plan plan-b :directory dir)
          (let ((plans (sibyl.plan:list-plans :directory dir)))
            (is (= 2 (length plans)))
            (let ((titles (mapcar (lambda (p) (getf p :title)) plans)))
              (is (member "Plan A" titles :test #'string=))
              (is (member "Plan B" titles :test #'string=)))))
      (cleanup-plan-dir dir))))

(test plan-list-plans-filter-status
  "list-plans with :status filters correctly."
  (let* ((dir    (make-temp-plan-dir))
         (draft  (sibyl.plan:make-plan :title "Draft"))
         (active (sibyl.plan:plan-update-status
                  (sibyl.plan:make-plan :title "Active") :in-progress)))
    (unwind-protect
        (progn
          (sibyl.plan:save-plan draft  :directory dir)
          (sibyl.plan:save-plan active :directory dir)
          (let ((drafts  (sibyl.plan:list-plans :directory dir :status :draft))
                (actives (sibyl.plan:list-plans :directory dir :status :in-progress)))
            (is (= 1 (length drafts)))
            (is (= 1 (length actives)))
            (is (string= "Draft"  (getf (first drafts)  :title)))
            (is (string= "Active" (getf (first actives) :title)))))
      (cleanup-plan-dir dir))))

(test plan-load-nonexistent
  "load-plan returns nil for unknown plan-id."
  (let ((dir (make-temp-plan-dir)))
    (unwind-protect
        (is (null (sibyl.plan:load-plan "nonexistent-id" :directory dir)))
      (cleanup-plan-dir dir))))

;;; ── Unit: index ──────────────────────────────────────────────────────────

(test plan-index-updated-on-save
  "Saving a plan updates the index file."
  (let* ((dir  (make-temp-plan-dir))
         (plan (sibyl.plan:make-plan :title "Indexed Plan")))
    (unwind-protect
        (progn
          (sibyl.plan:save-plan plan :directory dir)
          (let ((index (sibyl.plan:load-plan-index :directory dir)))
            (is (not (null index)))
            (is (find (getf plan :id) index
                      :key  (lambda (e) (getf e :id))
                      :test #'string=))))
      (cleanup-plan-dir dir))))

;;; ── Unit: plan mutation ───────────────────────────────────────────────────

(test plan-add-phase
  "plan-add-phase appends a phase to the plan."
  (let* ((plan    (sibyl.plan:make-plan :title "P"))
         (phase   (sibyl.plan:make-phase :title "New Phase"))
         (updated (sibyl.plan:plan-add-phase plan phase)))
    (is (= 1 (length (getf updated :phases))))
    (is (string= "New Phase" (getf (first (getf updated :phases)) :title)))
    (is (stringp (getf updated :updated-at)))))

(test plan-add-phase-appends
  "plan-add-phase appends to existing phases, not prepends."
  (let* ((phase1  (sibyl.plan:make-phase :title "Phase 1"))
         (plan    (sibyl.plan:make-plan :title "P" :phases (list phase1)))
         (phase2  (sibyl.plan:make-phase :title "Phase 2"))
         (updated (sibyl.plan:plan-add-phase plan phase2)))
    (is (= 2 (length (getf updated :phases))))
    (is (string= "Phase 1" (getf (first (getf updated :phases)) :title)))
    (is (string= "Phase 2" (getf (second (getf updated :phases)) :title)))))

(test plan-add-task
  "plan-add-task appends a task to the correct phase."
  (let* ((phase   (sibyl.plan:make-phase :title "P1"))
         (phase-id (getf phase :id))
         (plan    (sibyl.plan:make-plan :title "P" :phases (list phase)))
         (task    (sibyl.plan:make-task :title "New Task"))
         (updated (sibyl.plan:plan-add-task plan phase-id task)))
    (let* ((phases (getf updated :phases))
           (tasks  (getf (first phases) :tasks)))
      (is (= 1 (length tasks)))
      (is (string= "New Task" (getf (first tasks) :title))))))

(test plan-add-task-bad-phase
  "plan-add-task signals error for unknown phase-id."
  (let* ((plan (sibyl.plan:make-plan :title "P"))
         (task (sibyl.plan:make-task :title "T")))
    (signals error (sibyl.plan:plan-add-task plan 999 task))))

;;; ── Unit: deletion ───────────────────────────────────────────────────────

(test plan-delete
  "delete-plan removes the plan file and index entry."
  (let* ((dir     (make-temp-plan-dir))
         (plan    (sibyl.plan:make-plan :title "To Delete"))
         (plan-id (getf plan :id)))
    (unwind-protect
        (progn
          (sibyl.plan:save-plan plan :directory dir)
          (is (not (null (sibyl.plan:load-plan plan-id :directory dir))))
          (is (sibyl.plan:delete-plan plan-id :directory dir))
          (is (null (sibyl.plan:load-plan plan-id :directory dir)))
          ;; Index should be empty
          (let ((index (sibyl.plan:load-plan-index :directory dir)))
            (is (null (find plan-id index
                            :key (lambda (e) (getf e :id))
                            :test #'string=)))))
      (cleanup-plan-dir dir))))

(test plan-delete-nonexistent
  "delete-plan returns NIL for unknown plan-id."
  (let ((dir (make-temp-plan-dir)))
    (unwind-protect
        (is (null (sibyl.plan:delete-plan "nonexistent" :directory dir)))
      (cleanup-plan-dir dir))))

;;; ── Integration: tools ───────────────────────────────────────────────────

(test tool-save-plan-basic
  "save-plan tool creates a plan and returns its ID."
  (let* ((dir    (make-temp-plan-dir))
         (result (sibyl.tools:execute-tool
                  "save-plan"
                  (list (cons "title"       "Tool Test Plan")
                        (cons "description" "Created via tool")
                        (cons "plan-dir"    (namestring dir))))))
    (unwind-protect
        (progn
          (is (stringp result))
          (is (search "plan-" result)))
      (cleanup-plan-dir dir))))

(test tool-list-plans-basic
  "list-plans tool returns formatted plan list."
  (let* ((dir  (make-temp-plan-dir))
         (plan (sibyl.plan:make-plan :title "Listed Plan")))
    (unwind-protect
        (progn
          (sibyl.plan:save-plan plan :directory dir)
          (let ((result (sibyl.tools:execute-tool
                         "list-plans"
                         (list (cons "plan-dir" (namestring dir))))))
            (is (stringp result))
            (is (search "Listed Plan" result))))
      (cleanup-plan-dir dir))))

(test tool-load-plan-basic
  "load-plan tool returns plan details."
  (let* ((dir     (make-temp-plan-dir))
         (plan    (sibyl.plan:make-plan :title "Loaded Plan"
                                        :description "Details here"))
         (plan-id (getf plan :id)))
    (unwind-protect
        (progn
          (sibyl.plan:save-plan plan :directory dir)
          (let ((result (sibyl.tools:execute-tool
                         "load-plan"
                         (list (cons "plan-id"  plan-id)
                               (cons "plan-dir" (namestring dir))))))
            (is (stringp result))
            (is (search "Loaded Plan"  result))
            (is (search "Details here" result))))
      (cleanup-plan-dir dir))))

(test tool-update-plan-task-basic
  "update-plan-task tool marks a task done."
  (let* ((dir     (make-temp-plan-dir))
         (task    (sibyl.plan:make-task :title "My Task"))
         (task-id (getf task :id))
         (phase   (sibyl.plan:make-phase :title "P1" :tasks (list task)))
         (plan    (sibyl.plan:make-plan :title "Task Plan" :phases (list phase)))
         (plan-id (getf plan :id)))
    (unwind-protect
        (progn
          (sibyl.plan:save-plan plan :directory dir)
          (let ((result (sibyl.tools:execute-tool
                         "update-plan-task"
                         (list (cons "plan-id"  plan-id)
                               (cons "task-id"  task-id)
                               (cons "status"   "done")
                               (cons "plan-dir" (namestring dir))))))
            (is (stringp result))
            (is (search "done" (string-downcase result)))
            ;; Verify persisted
            (let* ((loaded (sibyl.plan:load-plan plan-id :directory dir))
                   (phases (getf loaded :phases))
                   (tasks  (getf (first phases) :tasks))
                   (found  (find task-id tasks
                                 :key  (lambda (tk) (getf tk :id))
                                 :test #'string=)))
              (is (eq :done (getf found :status))))))
      (cleanup-plan-dir dir))))

;;; ── Integration: new tools ───────────────────────────────────────────────

(test tool-delete-plan-basic
  "delete-plan tool removes a plan."
  (let* ((dir     (make-temp-plan-dir))
         (plan    (sibyl.plan:make-plan :title "To Delete"))
         (plan-id (getf plan :id)))
    (unwind-protect
        (progn
          (sibyl.plan:save-plan plan :directory dir)
          (let ((result (sibyl.tools:execute-tool
                         "delete-plan"
                         (list (cons "plan-id" plan-id)
                               (cons "plan-dir" (namestring dir))))))
            (is (stringp result))
            (is (search "deleted" (string-downcase result))))
          ;; Verify removed
          (is (null (sibyl.plan:load-plan plan-id :directory dir))))
      (cleanup-plan-dir dir))))

(test tool-update-plan-status-basic
  "update-plan-status tool changes plan status."
  (let* ((dir     (make-temp-plan-dir))
         (plan    (sibyl.plan:make-plan :title "Status Test"))
         (plan-id (getf plan :id)))
    (unwind-protect
        (progn
          (sibyl.plan:save-plan plan :directory dir)
          (let ((result (sibyl.tools:execute-tool
                         "update-plan-status"
                         (list (cons "plan-id" plan-id)
                               (cons "status" "in-progress")
                               (cons "plan-dir" (namestring dir))))))
            (is (stringp result))
            (is (search "in-progress" (string-downcase result))))
          ;; Verify persisted
          (let ((loaded (sibyl.plan:load-plan plan-id :directory dir)))
            (is (eq :in-progress (getf loaded :status)))))
      (cleanup-plan-dir dir))))

(test tool-add-phase-basic
  "add-phase tool adds a phase to a plan."
  (let* ((dir     (make-temp-plan-dir))
         (plan    (sibyl.plan:make-plan :title "Phase Test"))
         (plan-id (getf plan :id)))
    (unwind-protect
        (progn
          (sibyl.plan:save-plan plan :directory dir)
          (let ((result (sibyl.tools:execute-tool
                         "add-phase"
                         (list (cons "plan-id" plan-id)
                               (cons "title" "New Phase")
                               (cons "plan-dir" (namestring dir))))))
            (is (stringp result))
            (is (search "New Phase" result)))
          ;; Verify persisted
          (let* ((loaded (sibyl.plan:load-plan plan-id :directory dir))
                 (phases (getf loaded :phases)))
            (is (= 1 (length phases)))
            (is (string= "New Phase" (getf (first phases) :title)))))
      (cleanup-plan-dir dir))))

(test tool-add-task-basic
  "add-task tool adds a task to a phase."
  (let* ((dir      (make-temp-plan-dir))
         (phase    (sibyl.plan:make-phase :title "P1"))
         (phase-id (getf phase :id))
         (plan     (sibyl.plan:make-plan :title "Task Test" :phases (list phase)))
         (plan-id  (getf plan :id)))
    (unwind-protect
        (progn
          (sibyl.plan:save-plan plan :directory dir)
          (let ((result (sibyl.tools:execute-tool
                         "add-task"
                         (list (cons "plan-id"  plan-id)
                               (cons "phase-id" (princ-to-string phase-id))
                               (cons "title"    "New Task")
                               (cons "plan-dir" (namestring dir))))))
            (is (stringp result))
            (is (search "New Task" result)))
          ;; Verify persisted
          (let* ((loaded (sibyl.plan:load-plan plan-id :directory dir))
                 (phases (getf loaded :phases))
                 (tasks  (getf (first phases) :tasks)))
            (is (= 1 (length tasks)))
            (is (string= "New Task" (getf (first tasks) :title)))))
      (cleanup-plan-dir dir))))

;;; ── Regression: *print-pretty* corruption fix ────────────────────────────

(test plan-save-load-roundtrip-long-notes
  "save-plan and load-plan roundtrip with long multi-line UTF-8 notes.
Regression test for *print-pretty* t causing line-wrap corruption."
  (let* ((dir (make-temp-plan-dir))
         (long-notes
           (format nil
                   "実装の核心は %generate-unified-diff 関数。~%~
- 変更前の内容を (uiop:with-temporary-file ...) で /tmp に書き出す~%~
- (uiop:run-program (list \"diff\" \"-u\" old new) ...) で diff を取得~%~
- diff の exit code は変更あり=1, エラー=2 なので正常に扱う~%~
- レスポンス形式: 成功メッセージ + 改行 + \"```diff\" + diff文字列 + \"```\""))
         (plan (sibyl.plan:make-plan
                :title "ラウンドトリップテスト"
                :description "save-plan → load-plan の往復テスト"
                :notes long-notes))
         (plan-id (getf plan :id)))
    (unwind-protect
        (progn
          (sibyl.plan:save-plan plan :directory dir)
          (let ((loaded (sibyl.plan:load-plan plan-id :directory dir)))
            (is (not (null loaded))
                "load-plan should return a plan plist")
            (is (string= (getf loaded :title) "ラウンドトリップテスト")
                "title should roundtrip correctly")
            (is (string= (getf loaded :notes) long-notes)
                "long UTF-8 notes should roundtrip without corruption")))
      (cleanup-plan-dir dir))))
