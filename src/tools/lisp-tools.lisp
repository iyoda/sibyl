;;;; lisp-tools.lisp — Lisp introspection tools

(in-package #:sibyl.tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors (require :sb-introspect))
  (ignore-errors (require :sb-cltl2)))

(defun %package-function (package-name symbol-name)
  (let ((package (find-package package-name)))
    (when package
      (multiple-value-bind (sym status) (find-symbol symbol-name package)
        (declare (ignore status))
        (when (and sym (fboundp sym))
          (symbol-function sym))))))

(defun %split-symbol-string (symbol-string)
  (let* ((trimmed (string-trim-whitespace symbol-string)))
    (cond
      ((or (null trimmed) (string= trimmed ""))
       (values nil nil))
      ((char= (char trimmed 0) #\:)
       (values "KEYWORD"
               (subseq trimmed (if (and (> (length trimmed) 1)
                                        (char= (char trimmed 1) #\:))
                                   2
                                   1))))
      ((position #\: trimmed)
       (let* ((pos (position #\: trimmed))
              (double (and (< (1+ pos) (length trimmed))
                           (char= (char trimmed (1+ pos)) #\:)))
              (package-name (subseq trimmed 0 pos))
              (symbol-name (subseq trimmed (if double (+ pos 2) (1+ pos)))))
         (values package-name symbol-name)))
      (t (values nil trimmed)))))

(defun %special-variable-p (symbol)
  (let ((variable-information (or (%package-function :sb-cltl2 "VARIABLE-INFORMATION")
                                  (%package-function "SB-CLTL2" "VARIABLE-INFORMATION"))))
    (when variable-information
      (handler-case
          (multiple-value-bind (binding-type declarations type)
              (funcall variable-information symbol)
            (declare (ignore declarations type))
            (eq binding-type :special))
        (error () nil)))))

(defun %safe-lambda-list (fn)
  (when fn
    (let ((lambda-list-fn (or (%package-function :sb-introspect "FUNCTION-LAMBDA-LIST")
                              (%package-function "SB-INTROSPECT" "FUNCTION-LAMBDA-LIST"))))
      (when lambda-list-fn
        (handler-case
            (funcall lambda-list-fn fn)
          (error () nil))))))

(defun %specializer-label (specializer)
  (cond
    ((typep specializer 'class)
     (or (class-name specializer) specializer))
    ((fboundp 'sb-mop:eql-specializer-object)
     (handler-case
         (sb-mop:eql-specializer-object specializer)
       (error () specializer)))
    (t specializer)))

(defun %generic-method-lines (generic-function)
  (when (and generic-function (typep generic-function 'generic-function))
    (let ((methods (sb-mop:generic-function-methods generic-function)))
      (cons (format nil "Methods: ~a" (length methods))
            (mapcar (lambda (method)
                      (let* ((qualifiers (sb-mop:method-qualifiers method))
                             (specializers (sb-mop:method-specializers method)))
                        (format nil "  - qualifiers: ~s; specializers: ~s"
                                qualifiers
                                (mapcar #'%specializer-label specializers))))
                    methods)))))

(defun %class-slot-line (class)
  (when class
    (handler-case
        (let* ((slots (sb-mop:class-slots class))
               (slot-names (mapcar #'sb-mop:slot-definition-name slots)))
          (when slots
            (format nil "Slots: ~{~a~^, ~}" slot-names)))
      (error () nil))))

(deftool "describe-symbol"
    (:description "Describe a Lisp symbol: bindings, functions, and types."
     :parameters ((:name "symbol" :type "string" :required t
                   :description "Symbol name, optionally with package prefix (e.g. \"pkg:name\")")
                  (:name "package" :type "string" :required nil
                   :description "Package name to resolve the symbol in")))
  (block describe-symbol
    (let* ((symbol-input (getf args :symbol))
           (package-input (getf args :package)))
      (multiple-value-bind (package-from-input symbol-name)
          (%split-symbol-string symbol-input)
        (let* ((requested-package (or package-input package-from-input))
               (package (and requested-package
                             (find-package (string-upcase requested-package))))
               (resolved-package (or package *package*))
               (normalized-symbol-name (and symbol-name (string-upcase symbol-name))))
          (when (or (null normalized-symbol-name)
                    (string= normalized-symbol-name ""))
            (return-from describe-symbol
              (string-join (string #\Newline)
                           (remove nil
                                   (list (format nil "Symbol: ~a" symbol-input)
                                         (when requested-package
                                           (format nil "Package: ~a" requested-package))
                                         "Status: not-found"
                                         "Reason: empty-symbol")))))
          (when (and requested-package (null package))
            (return-from describe-symbol
              (string-join (string #\Newline)
                           (remove nil
                                   (list (format nil "Symbol: ~a" symbol-input)
                                         (format nil "Package: ~a" requested-package)
                                         "Status: not-found"
                                         "Reason: package-not-found")))))
          (multiple-value-bind (symbol status)
              (find-symbol normalized-symbol-name resolved-package)
            (when (null status)
              (return-from describe-symbol
                (string-join (string #\Newline)
                             (remove nil
                                     (list (format nil "Symbol: ~a" symbol-input)
                                           (format nil "Package: ~a"
                                                   (package-name resolved-package))
                                           "Status: not-found")))))
            (let* ((is-bound (boundp symbol))
                   (is-fbound (not (null (fboundp symbol))))
                   (is-macro (and is-fbound (not (null (macro-function symbol)))))
                   (function (and is-fbound (symbol-function symbol)))
                   (lambda-list (%safe-lambda-list function))
                   (is-generic (and function (typep function 'generic-function)))
                   (class (ignore-errors (find-class symbol nil)))
                   (special-variable (%special-variable-p symbol))
                   (lines (list (format nil "Symbol: ~s" symbol)
                                (format nil "Package: ~a"
                                        (package-name (symbol-package symbol)))
                                (format nil "Status: found")
                                (format nil "Bound: ~a" is-bound)
                                (format nil "Fbound: ~a" is-fbound)
                                (format nil "Macro: ~a" is-macro)
                                (format nil "Special-variable: ~a" special-variable)
                                (format nil "Generic-function: ~a" is-generic)
                                (format nil "Class: ~a" (not (null class))))))
              (when lambda-list
                (setf lines (append lines
                                    (list (format nil "Lambda-list: ~s" lambda-list)))))
              (when is-generic
                (setf lines (append lines (%generic-method-lines function))))
              (let ((slot-line (%class-slot-line class)))
                (when slot-line
                  (setf lines (append lines (list slot-line)))))
              (return-from describe-symbol
                (string-join (string #\Newline) lines)))))))))

;;; ============================================================
;;; S-expression reader
;;; ============================================================

(defparameter *read-sexp-top-level-forms*
  '("defun" "defmethod" "defclass" "defvar" "defparameter" "deftool")
  "Top-level forms recognized by read-sexp.")

(defun %read-sexp-whitespace-char-p (ch)
  (or (char= ch #\Space)
      (char= ch #\Tab)
      (char= ch #\Newline)
      (char= ch #\Return)
      (char= ch #\Page)))

(defun %read-sexp-compute-line-starts (content)
  (let ((starts (list 0)))
    (loop for idx from 0 below (length content)
          when (char= (char content idx) #\Newline)
            do (push (1+ idx) starts))
    (coerce (nreverse starts) 'vector)))

(defun %read-sexp-line-number-at (line-starts index)
  (loop for i from (1- (length line-starts)) downto 0
        when (<= (aref line-starts i) index)
          do (return (1+ i))
        finally (return 1)))

(defun %read-sexp-skip-line-comment (content pos)
  (let ((len (length content))
        (i pos))
    (loop while (< i len)
          for ch = (char content i)
          do (when (char= ch #\Newline)
               (return (1+ i)))
             (incf i))
    i))

(defun %read-sexp-skip-block-comment (content pos)
  (let ((len (length content))
        (depth 1)
        (i pos))
    (loop while (< i len)
          for ch = (char content i)
          for next = (and (< (1+ i) len) (char content (1+ i)))
          do (cond
               ((and (char= ch #\#) (char= next #\|))
                (incf depth)
                (incf i 2))
               ((and (char= ch #\|) (char= next #\#))
                (decf depth)
                (incf i 2)
                (when (zerop depth)
                  (return i)))
               (t (incf i))))
    i))

(defun %read-sexp-skip-whitespace-and-comments (content pos)
  (let ((len (length content))
        (i pos)
        (eof (gensym "EOF")))
    (loop while (< i len)
          for ch = (char content i)
          for next = (and (< (1+ i) len) (char content (1+ i)))
          do (cond
               ((%read-sexp-whitespace-char-p ch)
                (incf i))
               ((char= ch #\;)
                (setf i (%read-sexp-skip-line-comment content (1+ i))))
               ((and (char= ch #\#) (char= next #\|))
                (setf i (%read-sexp-skip-block-comment content (+ i 2))))
               ((and (char= ch #\#) (char= next #\;))
                (multiple-value-bind (form end)
                    (read-from-string content nil eof :start (+ i 2))
                  (if (eq form eof)
                      (setf i len)
                      (setf i end))))
               (t (return i)))
          finally (return i))))

(defun %read-sexp-read-forms-from-file (path)
  (let ((*read-eval* nil))
    (with-open-file (stream path :direction :input)
      (let ((eof (gensym "EOF"))
            (forms nil))
        (loop for form = (read stream nil eof)
              until (eq form eof)
              do (push form forms))
        (nreverse forms)))))

(defun %read-sexp-read-forms-with-positions (content)
  (let ((*read-eval* nil))
    (let ((pos 0)
          (len (length content))
          (eof (gensym "EOF"))
          (entries nil))
      (loop
        (setf pos (%read-sexp-skip-whitespace-and-comments content pos))
        (when (>= pos len)
          (return (nreverse entries)))
        (multiple-value-bind (form next)
            (read-from-string content nil eof :start pos)
          (when (eq form eof)
            (return (nreverse entries)))
          (push (list :start pos :end next :form form) entries)
          (setf pos next))))))

(defun %read-sexp-form-type-string (form)
  (when (and (consp form) (symbolp (car form)))
    (string-downcase (symbol-name (car form)))))

(defun %read-sexp-form-name-string (form)
  (when (consp form)
    (let ((name (second form)))
      (cond
        ((stringp name) name)
        ((symbolp name) (string-downcase (symbol-name name)))
        (t (prin1-to-string name))))))

(defun %read-sexp-normalize-filter (value)
  (when value
    (string-downcase value)))

(defun %read-sexp-make-form-entry (form start end line-starts content-length)
  (let* ((safe-end (if (> end 0) (1- end) start))
         (last-index (max 0 (1- content-length)))
         (end-index (min safe-end last-index))
         (start-line (%read-sexp-line-number-at line-starts start))
         (end-line (%read-sexp-line-number-at line-starts end-index))
         (entry (make-hash-table :test 'equal)))
    (setf (gethash "type" entry) (%read-sexp-form-type-string form))
    (setf (gethash "name" entry) (%read-sexp-form-name-string form))
    (setf (gethash "start_line" entry) start-line)
    (setf (gethash "end_line" entry) end-line)
    (setf (gethash "form" entry) (prin1-to-string form))
    entry))

(deftool "read-sexp"
    (:description "Read Lisp source as top-level S-expressions with metadata."
     :parameters ((:name "path" :type "string" :required t
                   :description "Path to the Lisp source file")
                  (:name "name" :type "string" :required nil
                   :description "Optional definition name filter")
                  (:name "type" :type "string" :required nil
                   :description "Optional form type filter (defun, deftool, ...)")))
  (let* ((path (getf args :path))
         (name-filter (%read-sexp-normalize-filter (getf args :name)))
         (type-filter (%read-sexp-normalize-filter (getf args :type))))
    (unless (uiop:file-exists-p path)
      (error "File not found: ~a" path))
    (let* ((content (uiop:read-file-string path))
           (line-starts (%read-sexp-compute-line-starts content))
           (forms (%read-sexp-read-forms-from-file path))
           (positions (%read-sexp-read-forms-with-positions content)))
      (when (/= (length forms) (length positions))
        (error "Reader mismatch for ~a (forms: ~a, positions: ~a)"
               path (length forms) (length positions)))
      (let ((results nil))
        (loop for form in forms
              for position in positions
              for type = (%read-sexp-form-type-string form)
              for name = (%read-sexp-form-name-string form)
              for start = (getf position :start)
              for end = (getf position :end)
              for name-key = (%read-sexp-normalize-filter name)
              when (and type
                        (member type *read-sexp-top-level-forms*
                                :test #'string=)
                        (or (null type-filter)
                            (string= type type-filter))
                        (or (null name-filter)
                            (and name-key
                                 (string= name-key name-filter))))
                do (push (%read-sexp-make-form-entry form start end
                                                     line-starts
                                                     (length content))
                         results))
        (nreverse results)))))

;;; ============================================================
;;; Evaluation
;;; ============================================================

(defparameter *eval-form-blocked-symbol-names*
  '("QUIT"
    "EXIT"
    "DEFPACKAGE"
    "DELETE-PACKAGE"
    "RENAME-PACKAGE"
    "RUN-PROGRAM"
    "RUN-SHELL-COMMAND")
  "Symbol names that are disallowed in eval-form input.")

(defun %eval-form-normalize-timeout (timeout)
  "Normalize TIMEOUT to a positive integer, defaulting to 30 seconds."
  (if (and (integerp timeout) (> timeout 0))
      timeout
      30))

(defun %eval-form-resolve-package (package-name)
  "Resolve PACKAGE-NAME to a package object, erroring if missing."
  (or (find-package package-name)
      (find-package (string-upcase package-name))
      (error "Unknown package: ~a" package-name)))

(defun %eval-form-blocked-symbol-p (symbol)
  "Return true if SYMBOL is in the blocked list."
  (member (string-upcase (symbol-name symbol))
          *eval-form-blocked-symbol-names*
          :test #'string=))

(defun %eval-form-find-blocked-symbol (form)
  "Return the first blocked symbol found in FORM, or NIL."
  (labels ((walk (node)
             (cond
               ((symbolp node)
                (when (%eval-form-blocked-symbol-p node)
                  node))
               ((consp node)
                (or (walk (car node))
                    (walk (cdr node))))
               (t nil))))
    (walk form)))

(defun %eval-form-format-values (values)
  "Format VALUES (a list) into a single result string."
  (cond
    ((null values) "NIL")
    ((= (length values) 1) (prin1-to-string (first values)))
    (t (string-join " " (mapcar #'prin1-to-string values)))))

(defun %eval-form-format-result (values-string stdout stderr)
  "Merge output streams with VALUES-STRING for final tool output."
  (let ((stdout (and stdout (string/= stdout "") stdout))
        (stderr (and stderr (string/= stderr "") stderr)))
    (if (and (null stdout) (null stderr))
        values-string
        (format nil "~@[STDOUT: ~a~%~]~@[STDERR: ~a~%~]~a"
                stdout stderr values-string))))

(deftool "eval-form"
    (:description "Evaluate a Lisp form in the current image."
     :parameters ((:name "form" :type "string" :required t
                   :description "Lisp form to evaluate (string).")
                  (:name "package" :type "string" :required nil
                   :description "Package to bind *package* to (default: SIBYL).")
                  (:name "timeout" :type "integer" :required nil
                   :description "Timeout in seconds (default: 30).")))
  (block eval-form
    (let* ((form-string (getf args :form))
           (package-name (or (getf args :package) "SIBYL"))
           (timeout (%eval-form-normalize-timeout (getf args :timeout)))
           (package (%eval-form-resolve-package package-name))
           (stdout-stream (make-string-output-stream))
           (stderr-stream (make-string-output-stream)))
      (labels ((finish (values-string)
                 (let ((stdout (get-output-stream-string stdout-stream))
                       (stderr (get-output-stream-string stderr-stream)))
                   (return-from eval-form
                     (%eval-form-format-result values-string stdout stderr)))))
        (handler-case
            (let* ((*standard-output* stdout-stream)
                   (*error-output* stderr-stream)
                   (*package* package)
                   (*read-eval* nil))
              (multiple-value-bind (form read-position)
                  (read-from-string form-string)
                (declare (ignore read-position))
                (let ((blocked (%eval-form-find-blocked-symbol form)))
                  (when blocked
                    (finish (format nil "Blocked unsafe form: ~a"
                                    (symbol-name blocked)))))
                (let ((values (sb-ext:with-timeout timeout
                                (let ((values (multiple-value-list (eval form))))
                                  (let ((primary (first values)))
                                    (when (and (symbolp primary) (fboundp primary))
                                      (compile primary)))
                                  values))))
                  (finish (%eval-form-format-values values)))))
          (sb-ext:timeout ()
            (finish (format nil "Timeout after ~a seconds" timeout))))))))

;;; ============================================================
;;; Macro expansion
;;; ============================================================

(defun %macroexpand-form-normalize-full (full-param)
  "Normalize FULL-PARAM to a boolean. Defaults to T (full expansion)."
  (cond
    ((null full-param) t)
    ((stringp full-param)
     (let ((lower (string-downcase full-param)))
       (or (string= lower "true")
           (string= lower "t")
           (string= lower "yes")
           (string= lower "1"))))
    ((booleanp full-param) full-param)
    (t t)))

(defun %macroexpand-form-pretty-print (form)
  "Pretty-print a form to a string using pprint."
  (with-output-to-string (s)
    (pprint form s)))

(deftool "macroexpand-form"
    (:description "Expand Lisp macros using macroexpand or macroexpand-1, returning pretty-printed S-expressions."
     :parameters ((:name "form" :type "string" :required t
                   :description "Lisp form to expand (string).")
                  (:name "full" :type "string" :required nil
                   :description "Full expansion (true/false, default: true). true=macroexpand, false=macroexpand-1.")))
  (block macroexpand-form
    (let* ((form-string (getf args :form))
           (full-param (getf args :full))
           (full-expansion-p (%macroexpand-form-normalize-full full-param)))
      (handler-case
          (let* ((*read-eval* nil)
                 (form (read-from-string form-string))
                 (expanded (if full-expansion-p
                               (macroexpand form)
                               (macroexpand-1 form))))
            (%macroexpand-form-pretty-print expanded))
        (error (e)
          (error 'sibyl.conditions:tool-execution-error
                 :tool-name "macroexpand-form"
                 :message (format nil "Failed to expand form: ~a" e)
                 :inner-error e))))))

;;; ============================================================
;;; Package introspection
;;; ============================================================

(defun %package-symbols-determine-type (symbol)
  "Determine the type of a symbol: function, variable, class, or macro."
  (cond
    ((and (fboundp symbol) (macro-function symbol))
     "macro")
    ((fboundp symbol)
     "function")
    ((boundp symbol)
     "variable")
    ((ignore-errors (find-class symbol nil))
     "class")
    (t "unknown")))

(defun %package-symbols-collect (package-name external-only)
  "Collect symbols from a package with their types."
  (let ((package (find-package package-name)))
    (unless package
      (error "Package not found: ~a" package-name))
    (let ((symbols nil))
      (if external-only
          (do-external-symbols (sym package)
            (push (cons sym (%package-symbols-determine-type sym)) symbols))
          (do-symbols (sym package)
            (when (eq (symbol-package sym) package)
              (push (cons sym (%package-symbols-determine-type sym)) symbols))))
      ;; Sort alphabetically by symbol name
      (sort symbols (lambda (a b)
                      (string< (symbol-name (car a))
                               (symbol-name (car b))))))))

(defun %package-symbols-format-result (symbols)
  "Format collected symbols as JSON array."
  (let ((entries (mapcar (lambda (sym-type)
                           (let ((entry (make-hash-table :test 'equal)))
                             (setf (gethash "name" entry)
                                   (string-upcase (symbol-name (car sym-type))))
                             (setf (gethash "type" entry) (cdr sym-type))
                             entry))
                         symbols)))
    (with-output-to-string (s)
      (yason:encode entries s))))

(deftool "package-symbols"
    (:description "List symbols in a Lisp package with type annotations."
     :parameters ((:name "package" :type "string" :required t
                   :description "Package name (e.g., \"SIBYL.TOOLS\")")
                  (:name "external-only" :type "boolean" :required nil
                   :description "List only exported symbols (default: true)")))
  (let* ((package-name (string-upcase (getf args :package)))
         (external-only-param (getf args :external-only))
         (external-only (if (null external-only-param) t external-only-param)))
    (handler-case
        (let ((symbols (%package-symbols-collect package-name external-only)))
          (%package-symbols-format-result symbols))
      (error (e)
        (error 'sibyl.conditions:tool-execution-error
               :tool-name "package-symbols"
               :message (format nil "Failed to list package symbols: ~a" e)
               :inner-error e)))))

;;; ============================================================
;;; Call relationship analysis
;;; ============================================================

(defun %who-calls-resolve-symbol (function-string)
  "Resolve FUNCTION-STRING to a symbol, handling package prefixes."
  (multiple-value-bind (package-name symbol-name)
      (%split-symbol-string function-string)
    (let* ((package (if package-name
                        (find-package (string-upcase package-name))
                        *package*))
           (normalized-symbol-name (string-upcase symbol-name)))
      (when (null package)
        (return-from %who-calls-resolve-symbol
          (values nil (format nil "Package not found: ~a" package-name))))
      (multiple-value-bind (symbol status)
          (find-symbol normalized-symbol-name package)
        (cond
          ((null status)
           (values nil (format nil "Symbol not found: ~a in package ~a"
                               symbol-name (package-name package))))
          (t (values symbol nil)))))))

(defun %who-calls-get-callers (symbol)
  "Get list of functions that call SYMBOL using sb-introspect:who-calls."
  (let ((who-calls-fn (or (%package-function :sb-introspect "WHO-CALLS")
                          (%package-function "SB-INTROSPECT" "WHO-CALLS"))))
    (when who-calls-fn
      (handler-case
          (funcall who-calls-fn symbol)
        (error () nil)))))

(defun %who-calls-extract-caller-name (caller-entry)
  "Extract the caller name from a who-calls result entry.
   Handles both simple symbols and complex (method . source) pairs."
  (cond
    ;; Simple symbol
    ((symbolp caller-entry) caller-entry)
    ;; (method . source) pair - extract method name
    ((consp caller-entry)
     (let ((method-spec (car caller-entry)))
       (cond
         ;; Generic function method: (SB-PCL::FAST-METHOD name ...)
         ((and (consp method-spec)
               (symbolp (first method-spec))
               (or (search "METHOD" (symbol-name (first method-spec)))
                   (search "FAST-METHOD" (symbol-name (first method-spec)))))
          (second method-spec))
         ;; Other method spec
         ((consp method-spec) (first method-spec))
         ;; Direct symbol
         ((symbolp method-spec) method-spec)
         (t nil))))
    (t nil)))

(defun %who-calls-filter-sibyl-callers (callers)
  "Filter callers to only include Sibyl's own code (sibyl.* packages)."
  (let ((extracted-names
          (mapcar #'%who-calls-extract-caller-name callers)))
    (remove-if-not
     (lambda (caller)
       (when (symbolp caller)
         (let ((pkg (symbol-package caller)))
           (and pkg
                (let ((pkg-name (package-name pkg)))
                  (or (string= pkg-name "SIBYL")
                      (and (>= (length pkg-name) 6)
                           (string= (subseq pkg-name 0 6) "SIBYL."))))))))
     extracted-names)))

(defun %who-calls-format-callers (symbol callers)
  "Format callers list as human-readable output."
  (let ((filtered-callers (%who-calls-filter-sibyl-callers callers)))
    (if (null filtered-callers)
        (format nil "Function: ~s~%Callers: none found~%~%Note: Only compiled functions are tracked by sb-introspect.~%If this function was recently defined, ensure it has been compiled."
                symbol)
        (format nil "Function: ~s~%Callers (~a):~%~{  - ~s~%~}"
                symbol
                (length filtered-callers)
                filtered-callers))))

(deftool "who-calls"
    (:description "Analyze function call relationships using sb-introspect:who-calls. Reports which functions call a given function."
     :parameters ((:name "function" :type "string" :required t
                   :description "Function name with optional package prefix (e.g., \"sibyl.tools:execute-tool-call\")")
                  (:name "direction" :type "string" :required nil
                   :description "Direction: \"callers\" (default) or \"callees\" (not yet implemented)")))
  (block who-calls
    (let* ((function-string (getf args :function))
           (direction (or (getf args :direction) "callers")))
      
      ;; Validate direction
      (unless (string= (string-downcase direction) "callers")
        (return-from who-calls
          (format nil "Error: Only \"callers\" direction is currently supported.~%Requested: ~a" direction)))
      
      ;; Resolve symbol
      (multiple-value-bind (symbol error-msg)
          (%who-calls-resolve-symbol function-string)
        (when error-msg
          (return-from who-calls error-msg))
        
        ;; Check if it's a function
        (unless (fboundp symbol)
          (return-from who-calls
            (format nil "Symbol: ~s~%Status: not a function~%~@[Bound: ~a~%~]~@[Special-variable: ~a~]"
                    symbol
                    (boundp symbol)
                    (%special-variable-p symbol))))
        
        ;; Get callers
        (let ((callers (%who-calls-get-callers symbol)))
          (return-from who-calls
            (%who-calls-format-callers symbol callers)))))))
