(in-package :sibyl.tests)

;;; Test suite for REPL display formatting functions

(def-suite repl-display-tests
  :description "Tests for REPL display formatting utilities"
  :in sibyl-tests)

(in-suite repl-display-tests)

;;; ============================================================
;;; format-cost tests
;;; ============================================================

(test format-cost-basic
  "format-cost formats USD with 3 decimal places"
  (is (string= "$0.012" (with-output-to-string (s) (sibyl.repl.display:format-cost 0.012 s))))
  (is (string= "$1.500" (with-output-to-string (s) (sibyl.repl.display:format-cost 1.5 s))))
  (is (string= "$0.000" (with-output-to-string (s) (sibyl.repl.display:format-cost 0.0 s)))))

;;; ============================================================
;;; format-tokens-compact tests
;;; ============================================================

(test format-tokens-compact-basic
  "format-tokens-compact formats with comma separators"
  (is (string= "1,234" (with-output-to-string (s) (sibyl.repl.display:format-tokens-compact 1234 s))))
  (is (string= "12,345,678" (with-output-to-string (s) (sibyl.repl.display:format-tokens-compact 12345678 s))))
  (is (string= "0" (with-output-to-string (s) (sibyl.repl.display:format-tokens-compact 0 s)))))

;;; ============================================================
;;; format-bytes-human tests
;;; ============================================================

(test format-bytes-human-basic
  "format-bytes-human formats bytes, KB, MB"
  (is (string= "500B" (with-output-to-string (s) (sibyl.repl.display:format-bytes-human 500 s))))
  (is (string= "1.0KB" (with-output-to-string (s) (sibyl.repl.display:format-bytes-human 1024 s))))
  (is (string= "14.9KB" (with-output-to-string (s) (sibyl.repl.display:format-bytes-human 15258 s))))
  (is (string= "1.0MB" (with-output-to-string (s) (sibyl.repl.display:format-bytes-human (* 1024 1024) s)))))

;;; ============================================================
;;; format-duration tests
;;; ============================================================

(test format-duration-basic
  "format-duration formats seconds and minutes"
  (is (string= "0.02s" (with-output-to-string (s) (sibyl.repl.display:format-duration 0.02 s))))
  (is (string= "1.20s" (with-output-to-string (s) (sibyl.repl.display:format-duration 1.2 s))))
  (is (string= "2m 3s" (with-output-to-string (s) (sibyl.repl.display:format-duration 123 s))))
  (is (string= "1m 0s" (with-output-to-string (s) (sibyl.repl.display:format-duration 60 s)))))

;;; ============================================================
;;; format-context-percentage tests
;;; ============================================================

(test format-context-percentage-colors-enabled
  "format-context-percentage uses color codes when *use-colors* is T"
  (let ((sibyl.repl.display:*use-colors* t))
    ;; Green: < 60%
    (let ((output (with-output-to-string (s) (sibyl.repl.display:format-context-percentage 42 s))))
      (is (search (string #\Escape) output))
      (is (search "42%" output)))
    ;; Yellow: 60-80%
    (let ((output (with-output-to-string (s) (sibyl.repl.display:format-context-percentage 75 s))))
      (is (search (string #\Escape) output))
      (is (search "75%" output)))
    ;; Red: > 80%
    (let ((output (with-output-to-string (s) (sibyl.repl.display:format-context-percentage 92 s))))
      (is (search (string #\Escape) output))
      (is (search "92%" output)))))

(test format-context-percentage-colors-disabled
  "format-context-percentage outputs plain text when *use-colors* is NIL"
  (let ((sibyl.repl.display:*use-colors* nil))
    (is (string= "42%" (with-output-to-string (s) (sibyl.repl.display:format-context-percentage 42 s))))
    (is (string= "75%" (with-output-to-string (s) (sibyl.repl.display:format-context-percentage 75 s))))
    (is (string= "92%" (with-output-to-string (s) (sibyl.repl.display:format-context-percentage 92 s))))))

;;; ============================================================
;;; format-separator-line tests
;;; ============================================================

(test format-separator-line-basic
  "format-separator-line outputs horizontal rule"
  (let ((output (with-output-to-string (s) (sibyl.repl.display:format-separator-line 10 s))))
    (is (= 10 (length output)))
    (is (every (lambda (ch) (char= ch #\─)) output))))

;;; ============================================================
;;; format-dim-text tests
;;; ============================================================

(test format-dim-text-colors-enabled
  "format-dim-text uses ANSI dim code when *use-colors* is T"
  (let ((sibyl.repl.display:*use-colors* t))
    (let ((output (with-output-to-string (s) (sibyl.repl.display:format-dim-text "test" s))))
      (is (search (string #\Escape) output))
      (is (search "[2m" output))
      (is (search "test" output)))))

(test format-dim-text-colors-disabled
  "format-dim-text outputs plain text when *use-colors* is NIL"
  (let ((sibyl.repl.display:*use-colors* nil))
    (is (string= "test" (with-output-to-string (s) (sibyl.repl.display:format-dim-text "test" s))))))

;;; ============================================================
;;; format-bold-text tests
;;; ============================================================

(test format-bold-text-colors-enabled
  "format-bold-text uses ANSI bold code when *use-colors* is T"
  (let ((sibyl.repl.display:*use-colors* t))
    (let ((output (with-output-to-string (s) (sibyl.repl.display:format-bold-text "test" s))))
      (is (search (string #\Escape) output))
      (is (search "[1m" output))
      (is (search "test" output)))))

(test format-bold-text-colors-disabled
  "format-bold-text outputs plain text when *use-colors* is NIL"
  (let ((sibyl.repl.display:*use-colors* nil))
    (is (string= "test" (with-output-to-string (s) (sibyl.repl.display:format-bold-text "test" s))))))

;;; ============================================================
;;; format-colored tests
;;; ============================================================

(test format-colored-basic
  "format-colored applies color codes when *use-colors* is T"
  (let ((sibyl.repl.display:*use-colors* t))
    (let ((output (with-output-to-string (s) (sibyl.repl.display:format-colored "test" :red s))))
      (is (search (string #\Escape) output))
      (is (search "31m" output))  ; red color code
      (is (search "test" output)))))

(test format-colored-with-modifiers
  "format-colored applies dim and bold modifiers"
  (let ((sibyl.repl.display:*use-colors* t))
    ;; Dim modifier
    (let ((output (with-output-to-string (s) (sibyl.repl.display:format-colored "test" :green s :dim t))))
      (is (search "2;" output))
      (is (search "32m" output)))
    ;; Bold modifier
    (let ((output (with-output-to-string (s) (sibyl.repl.display:format-colored "test" :blue s :bold t))))
      (is (search "1;" output))
      (is (search "34m" output)))
    ;; Both modifiers
    (let ((output (with-output-to-string (s) (sibyl.repl.display:format-colored "test" :yellow s :dim t :bold t))))
      (is (search "2;1;" output))
      (is (search "33m" output)))))

(test format-colored-colors-disabled
  "format-colored outputs plain text when *use-colors* is NIL"
  (let ((sibyl.repl.display:*use-colors* nil))
    (is (string= "test" (with-output-to-string (s) (sibyl.repl.display:format-colored "test" :red s))))
    (is (string= "test" (with-output-to-string (s) (sibyl.repl.display:format-colored "test" :green s :dim t :bold t))))))

;;; ============================================================
;;; format-tool-start-line tests
;;; ============================================================

(test format-tool-start-line-basic
  "format-tool-start-line produces start line with tool name"
  (let ((tc (sibyl.llm:make-tool-call :name "read-file" :id "tc1"
                                       :arguments '(("path" . "src/repl.lisp")))))
    (let ((output (sibyl.repl.display:format-tool-start-line tc)))
      (is (string= "🔧 read-file ..." output)))))

(test format-tool-start-line-with-args
  "format-tool-start-line shows args for tools with primary keys"
  (let ((tc1 (sibyl.llm:make-tool-call :name "shell" :id "tc2"
                                        :arguments '(("command" . "ls -la"))))
        (tc2 (sibyl.llm:make-tool-call :name "grep" :id "tc3"
                                        :arguments '(("pattern" . "defun") ("path" . "src/")))))
    (is (string= "🔧 shell ..." (sibyl.repl.display:format-tool-start-line tc1)))
    (is (string= "🔧 grep ..." (sibyl.repl.display:format-tool-start-line tc2)))))

(test format-tool-start-line-no-args
  "format-tool-start-line handles tools with no arguments"
  (let ((tc (sibyl.llm:make-tool-call :name "list-directory" :id "tc4"
                                       :arguments nil)))
    (is (string= "🔧 list-directory ..." (sibyl.repl.display:format-tool-start-line tc)))))

;;; ============================================================
;;; format-tool-result-line tests
;;; ============================================================

(test format-tool-result-line-success-basic
  "format-tool-result-line produces success line with ✓"
  (let ((tc (sibyl.llm:make-tool-call :name "read-file" :id "tc5"
                                       :arguments '(("path" . "src/repl.lisp"))))
        (sibyl.repl.display:*use-colors* nil))
    (let ((output (sibyl.repl.display:format-tool-result-line tc "content..." 0.02 15258)))
      (is (search "✓" output))
      (is (search "read-file" output))
      (is (search "(src/repl.lisp)" output))
      (is (search "0.02s" output))
      (is (search "14.9KB" output)))))

(test format-tool-result-line-success-colors
  "format-tool-result-line uses green ✓ when *use-colors* is T"
  (let ((tc (sibyl.llm:make-tool-call :name "shell" :id "tc6"
                                       :arguments '(("command" . "pwd"))))
        (sibyl.repl.display:*use-colors* t))
    (let ((output (sibyl.repl.display:format-tool-result-line tc "output" 1.2 500)))
      (is (search "✓" output))
      (is (search (string #\Escape) output))  ; ANSI code present
      (is (search "32m" output))              ; green color code
      (is (search "1.20s" output))
      (is (search "500B" output)))))

(test format-tool-result-line-time-size-colors-customizable
  "format-tool-result-line applies configured colors to elapsed time and size."
  (let ((tc (sibyl.llm:make-tool-call :name "shell" :id "tc6b"
                                       :arguments '(("command" . "pwd"))))
        (sibyl.repl.display:*use-colors* t)
        (sibyl.repl.display::*tool-result-time-color* :magenta)
        (sibyl.repl.display::*tool-result-size-color* :yellow))
    (let ((output (sibyl.repl.display:format-tool-result-line tc "output" 1.2 500)))
      (is (search "35m" output)) ; magenta time
      (is (search "33m" output))))) ; yellow size

(test format-tool-result-line-failure-basic
  "format-tool-result-line produces failure line with ✗"
  (let ((tc (sibyl.llm:make-tool-call :name "shell" :id "tc7"
                                       :arguments '(("command" . "bad-cmd"))))
        (sibyl.repl.display:*use-colors* nil))
    (let ((output (sibyl.repl.display:format-tool-result-line tc "Error: command not found" 1.2 0)))
      (is (search "✗" output))
      (is (search "shell" output))
      (is (search "(bad-cmd)" output))
      (is (search "1.20s" output))
      (is (search "Error" output)))))

(test format-tool-result-line-failure-colors
  "format-tool-result-line uses red ✗ when *use-colors* is T"
  (let ((tc (sibyl.llm:make-tool-call :name "read-file" :id "tc8"
                                       :arguments '(("path" . "nonexistent.txt"))))
        (sibyl.repl.display:*use-colors* t))
    (let ((output (sibyl.repl.display:format-tool-result-line tc "Error: file not found" 0.5 0)))
      (is (search "✗" output))
      (is (search (string #\Escape) output))  ; ANSI code present
      (is (search "31m" output))              ; red color code
      (is (search "Error" output)))))

(test format-tool-result-line-no-size
  "format-tool-result-line omits size when bytes is 0 or error"
  (let ((tc (sibyl.llm:make-tool-call :name "shell" :id "tc9"
                                       :arguments '(("command" . "echo"))))
        (sibyl.repl.display:*use-colors* nil))
    (let ((output (sibyl.repl.display:format-tool-result-line tc "Error: failed" 0.1 0)))
      (is (search "✗" output))
      (is (search "0.10s" output))
      (is (search "Error" output))
      (is (not (search "B" output)))  ; no size indicator
      (is (not (search "KB" output))))))

(test format-tool-result-line-eval-form-error-is-single-line
  "eval-form output should keep args/error labels compact and single-line."
  (let ((tc (sibyl.llm:make-tool-call
             :name "eval-form" :id "tc9a"
             :arguments
             '(("form" . ";; Find the pending task ID in Phase 1
(let ((plan 1))
  plan)"))))
        (sibyl.repl.display:*use-colors* nil))
    (let ((output (sibyl.repl.display:format-tool-result-line
                   tc
                   "Error: blocked unsafe form during eval"
                   0.00
                   0)))
      (is (search "✗" output))
      (is (search "eval-form" output))
      (is (search "(;; Find the pending task ID in Phase 1" output))
      (is (not (search (string #\Newline) output)))
      (is (search "Error: blocked unsafe form" output)))))

(test format-tool-result-line-run-tests-failures
  "run-tests with failed tests should show failure count instead of generic Error."
  (let ((tc (sibyl.llm:make-tool-call :name "run-tests" :id "tc9b"
                                      :arguments '(("suite" . "sibyl-tests"))))
        (sibyl.repl.display:*use-colors* nil)
        (result "{\"total\":42,\"passed\":40,\"failed\":2,\"failures\":[]}"))
    (let ((output (sibyl.repl.display:format-tool-result-line tc result 0.02 0)))
      (is (search "✗" output))
      (is (search "run-tests" output))
      (is (search "(sibyl-tests)" output))
      (is (search "0.02s" output))
      (is (search "2/42 failed" output))
      (is (not (search " Error" output))))))

(test format-tool-result-line-multiple-args
  "format-tool-result-line shows multiple args for grep tool"
  (let ((tc (sibyl.llm:make-tool-call :name "grep" :id "tc10"
                                       :arguments '(("pattern" . "defun") ("path" . "src/"))))
        (sibyl.repl.display:*use-colors* nil))
    (let ((output (sibyl.repl.display:format-tool-result-line tc "matched lines" 0.3 1024)))
      (is (search "✓" output))
      (is (search "grep" output))
      (is (search "(defun src/)" output))
      (is (search "0.30s" output))
      (is (search "1.0KB" output)))))
