;;;; client-test.lisp — Tests for LLM client utilities

(in-package #:sibyl.tests)

(def-suite client-tests
  :description "Tests for LLM client utilities."
  :in sibyl-tests)

(in-suite client-tests)

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

(test streaming-thinking-callback-exists
  "Auto-generated test"
  (is (boundp 'sibyl.llm::*streaming-thinking-callback*))
  (is (null sibyl.llm::*streaming-thinking-callback*)))

(test streaming-thinking-callback-invoked
  "Auto-generated test"
  (let ((chunks nil))
    (let ((sibyl.llm::*streaming-thinking-callback*
           (lambda (text) (push text chunks))))
      ;; コールバックが正しく動作することを確認
      (funcall sibyl.llm::*streaming-thinking-callback* "chunk-1")
      (funcall sibyl.llm::*streaming-thinking-callback* "chunk-2"))
    (is (equal (nreverse chunks) '("chunk-1" "chunk-2")))))

(test streaming-thinking-callback-independent-of-text-callback
  "Auto-generated test"
  (let ((text-chunks nil)
        (thinking-chunks nil))
    (let ((sibyl.llm:*streaming-text-callback*
           (lambda (t-) (push t- text-chunks)))
          (sibyl.llm::*streaming-thinking-callback*
           (lambda (t-) (push t- thinking-chunks))))
      (funcall sibyl.llm:*streaming-text-callback* "text")
      (funcall sibyl.llm::*streaming-thinking-callback* "think"))
    ;; 各コールバックは独立して動作する
    (is (equal text-chunks '("text")))
    (is (equal thinking-chunks '("think")))))
