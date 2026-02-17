;;;; client-test.lisp — Tests for LLM client utilities

(in-package #:sibyl.tests)

(in-suite sibyl-tests)

(test to-json-value-alist-keys
  "to-json-value normalizes alist keys and recurses values."
  (let* ((input '((:model . "gpt")
                  (:max_tokens . 10)
                  ("nested" . ((:a . 1) ("b" . 2)))))
         (result (sibyl.llm::to-json-value input)))
    (is (hash-table-p result))
    (is (string= "gpt" (gethash "model" result)))
    (is (= 10 (gethash "max_tokens" result)))
    (let ((nested (gethash "nested" result)))
      (is (hash-table-p nested))
      (is (= 1 (gethash "a" nested)))
      (is (= 2 (gethash "b" nested))))))
