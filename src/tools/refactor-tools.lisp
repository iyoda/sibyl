;;;; refactor-tools.lisp — Refactoring tools: rename-symbol, extract-function

(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package #:sibyl.tools))

;;; ============================================================
;;; rename-symbol — Rename a symbol across a source file
;;; ============================================================

(defun %rename-symbol-in-token (token old-name new-name)
  "Replace OLD-NAME with NEW-NAME in TOKEN if it matches exactly
   (case-insensitive, ignoring package prefix)."
  (let* ((colon-pos (position #\: token :from-end t))
         (bare (if colon-pos
                   (subseq token (1+ colon-pos))
                   token))
         (prefix (if colon-pos
                     (subseq token 0 (1+ colon-pos))
                     "")))
    (if (string-equal bare old-name)
        (concatenate 'string prefix new-name)
        token)))

(defun %rename-symbol-process-line (line old-name new-name)
  "Process a single LINE, replacing symbol tokens matching OLD-NAME with NEW-NAME.
   Preserves strings and comments."
  (let ((result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (i 0)
        (len (length line)))
    (loop while (< i len)
          for ch = (char line i)
          do (cond
               ;; Line comment: copy rest verbatim
               ((char= ch #\;)
                (loop while (< i len)
                      do (vector-push-extend (char line i) result)
                         (incf i))
                (return))
               ;; String literal: copy verbatim including escape sequences
               ((char= ch #\")
                (vector-push-extend ch result)
                (incf i)
                (loop while (< i len)
                      for sc = (char line i)
                      do (vector-push-extend sc result)
                         (incf i)
                      until (and (char= sc #\")
                                 (or (= i 1)
                                     (not (char= (char line (- i 2)) #\\))))
                      finally (return)))
               ;; Symbol token: collect and maybe rename
               ((or (alphanumericp ch)
                    (member ch '(#\- #\_ #\% #\* #\+ #\/ #\< #\> #\= #\? #\! #\& #\^ #\~)
                            :test #'char=)
                    (char= ch #\:))
                (let ((token-start i))
                  (loop while (and (< i len)
                                   (let ((c (char line i)))
                                     (or (alphanumericp c)
                                         (member c '(#\- #\_ #\% #\* #\+ #\/ #\< #\> #\= #\? #\! #\& #\^ #\~ #\:)
                                                 :test #'char=))))
                        do (incf i))
                  (let* ((token (subseq line token-start i))
                         (renamed (%rename-symbol-in-token token old-name new-name)))
                    (loop for c across renamed do (vector-push-extend c result)))))
               ;; Other character: copy verbatim
               (t
                (vector-push-extend ch result)
                (incf i))))
    (coerce result 'string)))

(defun %rename-symbol-process-file (content old-name new-name)
  "Process entire file CONTENT, renaming OLD-NAME to NEW-NAME in all non-string, non-comment positions."
  (let* ((lines (%tools-split-lines content))
         (processed (mapcar (lambda (line)
                              (%rename-symbol-process-line line old-name new-name))
                            lines)))
    (format nil "~{~a~^~%~}" processed)))

(defun %rename-symbol-count-changes (old-content new-content)
  "Count how many lines differ between OLD-CONTENT and NEW-CONTENT."
  (let ((old-lines (%tools-split-lines old-content))
        (new-lines (%tools-split-lines new-content))
        (count 0))
    (mapc (lambda (o n) (unless (string= o n) (incf count)))
          old-lines new-lines)
    count))

(deftool "rename-symbol"
    (:description "Rename a symbol across a Lisp source file. Replaces all non-string,
non-comment occurrences of OLD-NAME with NEW-NAME in the specified file.
Returns a summary of changes made."
     :category :code
     :parameters ((:name "file" :type "string" :required t
                   :description "Path to the Lisp source file")
                  (:name "old-name" :type "string" :required t
                   :description "Current symbol name (bare, without package prefix)")
                  (:name "new-name" :type "string" :required t
                   :description "New symbol name")))
  (block rename-symbol
    (let* ((file-path (getf args :file))
           (old (getf args :old-name))
           (new (getf args :new-name)))
      (unless file-path
        (error 'sibyl.conditions:tool-execution-error
               :tool-name "rename-symbol"
               :message "file parameter is required"
               :inner-error nil))
      (unless old
        (error 'sibyl.conditions:tool-execution-error
               :tool-name "rename-symbol"
               :message "old-name parameter is required"
               :inner-error nil))
      (unless new
        (error 'sibyl.conditions:tool-execution-error
               :tool-name "rename-symbol"
               :message "new-name parameter is required"
               :inner-error nil))
      (unless (probe-file file-path)
        (error 'sibyl.conditions:tool-execution-error
               :tool-name "rename-symbol"
               :message (format nil "File not found: ~a" file-path)
               :inner-error nil))
      (let* ((content (uiop:read-file-string file-path))
             (new-content (%rename-symbol-process-file content old new))
             (changed-lines (%rename-symbol-count-changes content new-content)))
        (if (zerop changed-lines)
            (format nil "No occurrences of ~s found in ~a" old file-path)
            (progn
              (with-open-file (out file-path :direction :output
                                             :if-exists :supersede
                                             :external-format :utf-8)
                (write-string new-content out))
              (format nil "Renamed ~s -> ~s in ~a (~d line~:p changed)"
                      old new file-path changed-lines)))))))

;;; ============================================================
;;; extract-function — Extract a code block into a new function
;;; ============================================================

(defun %extract-function-build-new-fn (extracted-lines new-name param-list docstring)
  "Build a new defun form string from EXTRACTED-LINES."
  (let ((params-str (if param-list
                        (format nil "~{~a~^ ~}" param-list)
                        ""))
        (doc-str (if (and docstring (not (string= docstring "")))
                     (format nil "~%  ~s" docstring)
                     ""))
        (body-str (format nil "~{~a~^~%  ~}" extracted-lines)))
    (format nil "(defun ~a (~a)~a~%  ~a)" new-name params-str doc-str body-str)))

(deftool "extract-function"
    (:description "Extract a range of lines from within a function body into a new named function.
The original function is updated to call the new function.
Returns a summary of the extraction."
     :category :code
     :parameters ((:name "file" :type "string" :required t
                   :description "Path to the Lisp source file")
                  (:name "source-function" :type "string" :required t
                   :description "Name of the function to extract from")
                  (:name "start-line" :type "integer" :required t
                   :description "1-based start line of the code to extract (within the file)")
                  (:name "end-line" :type "integer" :required t
                   :description "1-based end line of the code to extract (within the file)")
                  (:name "new-function" :type "string" :required t
                   :description "Name for the extracted function")
                  (:name "params" :type "string" :required nil
                   :description "Space-separated parameter names for the new function (optional)")
                  (:name "docstring" :type "string" :required nil
                   :description "Docstring for the new function (optional)")))
  (block extract-function
    (let* ((file-path (getf args :file))
           (src-fn (getf args :source-function))
           (new-fn (getf args :new-function))
           (s-line (getf args :start-line))
           (e-line (getf args :end-line))
           (params (getf args :params))
           (docstring (getf args :docstring)))
      (unless file-path
        (error 'sibyl.conditions:tool-execution-error
               :tool-name "extract-function"
               :message "file parameter is required"
               :inner-error nil))
      (unless src-fn
        (error 'sibyl.conditions:tool-execution-error
               :tool-name "extract-function"
               :message "source-function is required"
               :inner-error nil))
      (unless new-fn
        (error 'sibyl.conditions:tool-execution-error
               :tool-name "extract-function"
               :message "new-function is required"
               :inner-error nil))
      (unless s-line
        (error 'sibyl.conditions:tool-execution-error
               :tool-name "extract-function"
               :message "start-line is required"
               :inner-error nil))
      (unless e-line
        (error 'sibyl.conditions:tool-execution-error
               :tool-name "extract-function"
               :message "end-line is required"
               :inner-error nil))
      (unless (probe-file file-path)
        (error 'sibyl.conditions:tool-execution-error
               :tool-name "extract-function"
               :message (format nil "File not found: ~a" file-path)
               :inner-error nil))
      (let* ((content (uiop:read-file-string file-path))
             (all-lines (%tools-split-lines content))
             (total-lines (length all-lines))
             (s-idx (1- s-line))
             (e-idx (1- e-line)))
        (unless (and (>= s-idx 0) (< e-idx total-lines) (<= s-idx e-idx))
          (error 'sibyl.conditions:tool-execution-error
                 :tool-name "extract-function"
                 :message (format nil "Line range ~a-~a is out of bounds (file has ~a lines)"
                                   s-line e-line total-lines)
                 :inner-error nil))
        (let* ((extracted-lines (coerce (subseq (coerce all-lines 'vector) s-idx (1+ e-idx)) 'list))
               (param-list (when (and params (not (string= params "")))
                             (uiop:split-string params :separator " ")))
               (new-fn-source (%extract-function-build-new-fn
                                extracted-lines new-fn param-list docstring))
               ;; Replace extracted lines with a call to the new function
               (call-line (if param-list
                              (format nil "  (~a ~{~a~^ ~})" new-fn param-list)
                              (format nil "  (~a)" new-fn)))
               (new-lines (append (coerce (subseq (coerce all-lines 'vector) 0 s-idx) 'list)
                                  (list call-line)
                                  (coerce (subseq (coerce all-lines 'vector) (1+ e-idx)) 'list)))
               (new-content (format nil "~{~a~^~%~}" new-lines))
               ;; Insert new function definition before source function
               (src-pattern (format nil "(defun ~a " src-fn))
               (insert-pos (search src-pattern new-content :test #'char-equal))
               (final-content (if insert-pos
                                  (concatenate 'string
                                               (subseq new-content 0 insert-pos)
                                               new-fn-source
                                               (string #\Newline)
                                               (string #\Newline)
                                               (subseq new-content insert-pos))
                                  (concatenate 'string new-content
                                               (string #\Newline)
                                               new-fn-source))))
          (with-open-file (out file-path :direction :output
                                         :if-exists :supersede
                                         :external-format :utf-8)
            (write-string final-content out))
          (format nil "Extracted lines ~a-~a from ~a into ~a~%New function inserted before ~a in ~a"
                  s-line e-line src-fn new-fn src-fn file-path))))))
