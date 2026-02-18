;;;; provisioning-test.lisp — Tests for provisioning stats and A/B evaluation

(in-package #:sibyl.tests)

(def-suite provisioning-tests
  :description "Provisioning stats and A/B test helpers"
  :in sibyl-tests)

(in-suite provisioning-tests)

(test provisioning-stats-classification
  "compute-provisioning-stats classifies over/under/correct."
  (let* ((results (list
                   (list :expected "light"  :predicted "light")
                   (list :expected "light"  :predicted "medium")
                   (list :expected "heavy"  :predicted "medium")
                   (list :expected "medium" :predicted "heavy")
                   (list :expected "medium" :predicted "light")))
         (stats (sibyl.llm:compute-provisioning-stats results)))
    (is (= 5 (sibyl.llm:provisioning-stats-total stats)))
    (is (= 1 (sibyl.llm:provisioning-stats-correct stats)))
    (is (= 2 (sibyl.llm:provisioning-stats-over stats)))
    (is (= 2 (sibyl.llm:provisioning-stats-under stats)))
    (is (< (abs (- 0.2d0 (sibyl.llm:provisioning-stats-accuracy stats))) 1.0d-6))
    (is (< (abs (- 0.4d0 (sibyl.llm:provisioning-stats-over-rate stats))) 1.0d-6))
    (is (< (abs (- 0.4d0 (sibyl.llm:provisioning-stats-under-rate stats))) 1.0d-6))))

(test tier-ab-test-helpers
  "run-tier-ab-test returns consistent stats for simple predictors."
  (let* ((task-set (list (list :task "A" :expected-tier "light")
                         (list :task "B" :expected-tier "medium")
                         (list :task "C" :expected-tier "heavy")))
         (predictor-a (lambda (task entry)
                        (declare (ignore task))
                        (getf entry :expected-tier)))
         (predictor-b (lambda (task entry)
                        (declare (ignore task entry))
                        "medium"))
         (report (sibyl.llm:run-tier-ab-test task-set predictor-a predictor-b
                                             :name-a "perfect"
                                             :name-b "baseline")))
    (is (string= "perfect" (sibyl.llm:ab-test-report-name-a report)))
    (is (string= "baseline" (sibyl.llm:ab-test-report-name-b report)))
    (is (= 1.0d0 (sibyl.llm:provisioning-stats-accuracy
                  (sibyl.llm:ab-test-report-stats-a report))))
    (is (> (sibyl.llm:provisioning-stats-accuracy
            (sibyl.llm:ab-test-report-stats-a report))
           (sibyl.llm:provisioning-stats-accuracy
            (sibyl.llm:ab-test-report-stats-b report))))))
