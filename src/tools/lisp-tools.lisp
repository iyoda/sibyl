;;;; lisp-tools.lisp — Lisp introspection tools

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((this-file (or *load-pathname* *compile-file-pathname*))
         (root (and this-file (uiop:pathname-directory-pathname this-file)))
         (packages (and root (merge-pathnames "../packages.lisp" root)))
         (protocol (and root (merge-pathnames "protocol.lisp" root))))
    (unless (find-package :sibyl.tools)
      (when (and packages (probe-file packages))
        (load packages)))
    (ignore-errors (require :asdf))
    (when (and (find-package :asdf)
               (null (find-package :yason)))
      (ignore-errors (asdf:load-system :yason)))
    (when (and (find-package :asdf)
               (null (find-package :fiveam)))
      (ignore-errors (asdf:load-system :fiveam)))
    (let* ((tool-package (find-package :sibyl.tools))
           (deftool-symbol (and tool-package
                                (find-symbol "DEFTOOL" tool-package))))
      (unless (and deftool-symbol (fboundp deftool-symbol))
        (when (and protocol (probe-file protocol))
          (load protocol))))))

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
    (setf (gethash "form" entry)
          (let ((*print-case* :downcase))
            (prin1-to-string form)))
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
;;; Codebase map
;;; ============================================================

(defun %codebase-map-find-lisp-files (dir)
  "Find all .lisp files in DIR recursively."
  (let* ((dir-path (uiop:ensure-directory-pathname dir))
         (files (uiop:directory-files dir-path))
         (lisp-files (remove-if-not
                      (lambda (path)
                        (string= (string-downcase (or (pathname-type path) ""))
                                 "lisp"))
                      files))
         (subdirs (uiop:subdirectories dir-path)))
    (append lisp-files
            (mapcan #'%codebase-map-find-lisp-files subdirs))))

(defun %codebase-map-group-by-directory (files)
  "Group FILES by their module directory under src/."
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (file files)
      (let* ((dir (pathname-directory file))
             (src-index (and dir (position "src" dir :test #'string=)))
             (module (cond
                       ((and src-index (< (1+ src-index) (length dir)))
                        (string-downcase (nth (1+ src-index) dir)))
                       (t (string-downcase (or (pathname-name file) ""))))))
        (when (and module (string/= module ""))
          (push file (gethash module groups)))))
    (let ((result nil))
      (maphash (lambda (name files-list)
                 (push (cons name (nreverse files-list)) result))
               groups)
      (sort result #'string< :key #'car))))

(defun %codebase-map-analyze-file (filepath detail-level)
  "Analyze FILEPATH with read-sexp when DETAIL-LEVEL is full."
  (labels ((relative-path (path)
             (let* ((dir (pathname-directory path))
                    (components (remove-if (lambda (item) (eq item :absolute)) dir))
                    (src-pos (position "src" components :test #'string=))
                    (relative-components (if src-pos (subseq components src-pos) components))
                    (name (pathname-name path))
                    (type (pathname-type path)))
               (if relative-components
                   (format nil "~{~a~^/~}/~a~@[.~a~]" relative-components name type)
                   (format nil "~a~@[.~a~]" name type)))))
    (let* ((path (pathname filepath))
           (path-string (namestring path))
           (entry (make-hash-table :test 'equal)))
      (setf (gethash "path" entry) (relative-path path))
      (when (string= detail-level "full")
        (handler-case
            (let* ((result (execute-tool "read-sexp"
                                         (list (cons "path" path-string))))
                   (parsed (yason:parse result :object-as :hash-table))
                   (definitions (if (vectorp parsed)
                                    (coerce parsed 'list)
                                    parsed)))
              (setf (gethash "definitions" entry) definitions))
          (error (e)
            (setf (gethash "definitions" entry) nil)
            (setf (gethash "error" entry)
                  (format nil "Failed to read definitions: ~a" e)))))
      entry)))

(deftool "codebase-map"
    (:description "Map Sibyl's codebase modules, files, and definitions."
     :parameters ((:name "detail-level" :type "string" :required nil
                   :description "Detail level: summary (default) or full")))
  (block codebase-map
    (let* ((detail-input (getf args :detail-level))
           (detail-level (string-downcase (or detail-input "summary")))
           (src-dir (asdf:system-relative-pathname :sibyl "src/")))
      (unless (member detail-level '("summary" "full") :test #'string=)
        (return-from codebase-map
          (format nil "Error: detail-level must be \"summary\" or \"full\". Got: ~a"
                  detail-input)))
      (let* ((files (%codebase-map-find-lisp-files src-dir))
             (groups (%codebase-map-group-by-directory files))
             (modules (mapcar (lambda (group)
                                (let* ((module-name (car group))
                                       (module-files (cdr group))
                                       (module (make-hash-table :test 'equal)))
                                  (setf (gethash "name" module) module-name)
                                  (setf (gethash "files" module)
                                        (mapcar (lambda (file)
                                                  (%codebase-map-analyze-file file detail-level))
                                                module-files))
                                  module))
                              groups))
             (result (make-hash-table :test 'equal)))
        (setf (gethash "detail_level" result) detail-level)
        (setf (gethash "modules" result) modules)
        result))))

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
;;; Safe redefinition
;;; ============================================================

(defun %safe-redefine-sibyl-package-p (symbol)
  "Return true if SYMBOL belongs to a Sibyl package."
  (let ((package (symbol-package symbol)))
    (when package
      (let ((pkg-name (string-upcase (package-name package))))
        (or (string= pkg-name "SIBYL")
            (and (>= (length pkg-name) 6)
                 (string= (subseq pkg-name 0 6) "SIBYL.")))))))

(defun %safe-redefine-resolve-symbol (name-string)
  "Resolve NAME-STRING to a symbol and package, or return an error message."
  (multiple-value-bind (package-name symbol-name)
      (%split-symbol-string name-string)
    (let* ((package (if package-name
                        (find-package (string-upcase package-name))
                        *package*))
           (normalized-symbol-name (and symbol-name
                                        (string-upcase symbol-name))))
      (cond
        ((or (null normalized-symbol-name)
             (string= normalized-symbol-name ""))
         (values nil nil (format nil "Symbol not specified: ~a" name-string)))
        ((null package)
         (values nil nil (format nil "Package not found: ~a" package-name)))
        (t
         (multiple-value-bind (symbol status)
             (find-symbol normalized-symbol-name package)
           (if (null status)
               (values nil package
                       (format nil "Symbol not found: ~a in package ~a"
                               symbol-name
                               (package-name package)))
               (values symbol package nil))))))))

(defun %safe-redefine-normalize-force (force)
  "Normalize FORCE to a boolean. Defaults to NIL."
  (cond
    ((null force) nil)
    ((stringp force)
     (let ((lower (string-downcase force)))
       (or (string= lower "true")
           (string= lower "t")
           (string= lower "yes")
           (string= lower "1"))))
    (t t)))

(defun %safe-redefine-eval-definition (definition package)
  "Evaluate DEFINITION in PACKAGE using eval-form."
  (let* ((result (execute-tool "eval-form"
                               (list (cons "form" definition)
                                     (cons "package" (package-name package)))))
         (lower (string-downcase result)))
    (when (or (search "blocked unsafe form" lower)
              (search "timeout after" lower))
      (error "Failed to evaluate new definition: ~a" result))
    result))

(defun %safe-redefine-closure-warning (name-string force)
  "Return a warning string about closure capture when FORCE is NIL."
  (unless force
    (when (find-tool "who-calls")
      (handler-case
          (let* ((result (execute-tool "who-calls"
                                       (list (cons "function" name-string))))
                 (lower (string-downcase result)))
            (when (and (search "callers" lower)
                       (not (search "none found" lower)))
              "Warning: compiled callers may retain old function objects; consider recompiling dependents."))
        (error () nil)))))

(deftool "safe-redefine"
    (:description "Safely redefine a function with rollback restart support."
     :parameters ((:name "name" :type "string" :required t
                   :description "Function name with package prefix (e.g., \"sibyl.tools:find-tool\")")
                  (:name "new-definition" :type "string" :required t
                   :description "New definition as an S-expression string")
                  (:name "force" :type "boolean" :required nil
                   :description "Skip caller warnings when true")))
  (block safe-redefine
    (let* ((name-string (getf args :name))
           (definition (getf args :new-definition))
           (force (%safe-redefine-normalize-force (getf args :force))))
      (multiple-value-bind (symbol package error-msg)
          (%safe-redefine-resolve-symbol name-string)
        (when error-msg
          (error error-msg))
        (unless (fboundp symbol)
          (error "Symbol not fbound: ~s" symbol))
        (unless (%safe-redefine-sibyl-package-p symbol)
          (error "safe-redefine can only modify Sibyl packages: ~a"
                 (package-name (symbol-package symbol))))
        ;; Save function before attempting redefinition
        (let ((saved-function (fdefinition symbol)))
          ;; Establish restart that covers ALL redefinition logic
          (restart-case
              (progn
                ;; This will signal an error for invalid code
                (%safe-redefine-eval-definition definition package)
                ;; Compile the newly defined function
                (compile symbol)
                ;; Verify compilation succeeded
                (let ((fn (symbol-function symbol)))
                  (unless (compiled-function-p fn)
                    (error "Redefinition did not compile for ~a" name-string)))
                ;; Check for closure warnings
                (let ((warning (%safe-redefine-closure-warning name-string force)))
                  (if warning
                      (format nil "Success: ~a redefined.~%~a" name-string warning)
                      (format nil "Success: ~a redefined." name-string))))
            ;; Restart to restore original function
            (restore-definition ()
              :report "Restore the original function definition"
              (setf (fdefinition symbol) saved-function)
              (format nil "Rolled back: ~a restored." name-string))))))))

;;; ============================================================
;;; Sync in-memory definitions to file
;;; ============================================================

(defun %sync-to-file-parse-read-sexp-result (json)
  (let ((parsed (yason:parse json :object-as :hash-table)))
    (cond
      ((null parsed) nil)
      ((vectorp parsed) (coerce parsed 'list))
      ((hash-table-p parsed) (list parsed))
      (t parsed))))

(defun %sync-to-file-entry-line (entry key)
  (let ((value (gethash key entry)))
    (when (integerp value)
      value)))

(defun %sync-to-file-split-lines (content)
  (let ((lines nil)
        (start 0)
        (len (length content)))
    (loop for idx from 0 below len
          for ch = (char content idx)
          do (when (char= ch #\Newline)
               (push (subseq content start idx) lines)
               (setf start (1+ idx))))
    (push (subseq content start len) lines)
    (nreverse lines)))

(defun %sync-to-file-join-lines (lines)
  (string-join (string #\Newline) lines))

(defun %sync-to-file-trim-trailing-empty (lines)
  (if (and lines (string= (car (last lines)) ""))
      (butlast lines)
      lines))

(defun %sync-to-file-write-content (path content)
  (with-open-file (stream path :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string content stream)))

(defun %sync-to-file-validate-source (new-source)
  (let ((*read-eval* nil))
    (handler-case
        (progn
          (read-from-string new-source)
          t)
      (error (e)
        (error "Invalid new-source: ~a" e)))))

(deftool "sync-to-file"
    (:description "Sync an in-memory definition back to its source file."
     :parameters ((:name "name" :type "string" :required t
                   :description "Definition name to replace")
                  (:name "file" :type "string" :required t
                   :description "Path to the source file")
                  (:name "new-source" :type "string" :required t
                   :description "New definition source as a string")))
  (block sync-to-file
    (let* ((name-input (getf args :name))
           (file (getf args :file))
           (new-source (getf args :new-source)))
      (unless (and name-input (stringp name-input)
                   (string/= name-input ""))
        (error "Definition name not specified: ~a" name-input))
      (unless (and file (stringp file) (string/= file ""))
        (error "File path not specified"))
      (unless (uiop:file-exists-p file)
        (error "File not found: ~a" file))
      (unless (and new-source (stringp new-source)
                   (string/= new-source ""))
        (error "New source not specified"))
      (%sync-to-file-validate-source new-source)
      (multiple-value-bind (package-name symbol-name)
          (%split-symbol-string name-input)
        (declare (ignore package-name))
        (let* ((name (or symbol-name name-input))
               (result (execute-tool "read-sexp"
                                     (list (cons "path" file)
                                           (cons "name" name))))
               (entries (%sync-to-file-parse-read-sexp-result result)))
          (unless entries
            (error "Definition ~a not found in ~a" name file))
          (when (> (length entries) 1)
            (error "Multiple definitions found for ~a in ~a" name file))
          (let* ((entry (first entries))
                 (start-line (%sync-to-file-entry-line entry "start_line"))
                 (end-line (%sync-to-file-entry-line entry "end_line")))
            (unless (and start-line end-line)
              (error "Definition ~a not found in ~a" name file))
            (let ((original-content (uiop:read-file-string file)))
              (restart-case
                  (let* ((lines (%sync-to-file-split-lines original-content))
                         (line-count (length lines))
                         (start-index (1- start-line))
                         (end-index end-line))
                    (when (or (< start-line 1)
                              (< end-line start-line)
                              (> end-line line-count))
                      (error "Invalid definition range for ~a in ~a: ~a-~a"
                             name file start-line end-line))
                    (let* ((before (subseq lines 0 start-index))
                           (after (subseq lines end-index))
                           (new-lines (%sync-to-file-trim-trailing-empty
                                       (%sync-to-file-split-lines new-source)))
                           (updated (append before new-lines after))
                           (new-content (%sync-to-file-join-lines updated)))
                      (%sync-to-file-write-content file new-content)
                      (format nil "Success: ~a synced to ~a" name file)))
                (restore-file ()
                  :report "Restore original file content"
                  (%sync-to-file-write-content file original-content)
                  (format nil "Rolled back: ~a restored." file))))))))))

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
     ((typep full-param 'boolean) full-param)
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
                 (*package* (or (find-package :sibyl.tools) *package*))
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
      (setf symbols (remove-if (lambda (entry)
                                 (string= (cdr entry) "unknown"))
                               symbols))
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

;;; ============================================================
;;; Programmatic test execution
;;; ============================================================

(defun %run-tests-resolve-target (suite-name test-name)
  "Resolve suite or test name to a symbol in SIBYL.TESTS package."
  (let ((tests-package (find-package "SIBYL.TESTS")))
    (unless tests-package
      (error "SIBYL.TESTS package not found. Load tests first."))
    (cond
      (test-name
       (let ((sym (find-symbol (string-upcase test-name) tests-package)))
         (unless sym
           (error "Test not found: ~a" test-name))
         sym))
      (suite-name
       (let ((sym (find-symbol (string-upcase suite-name) tests-package)))
         (unless sym
           (error "Suite not found: ~a" suite-name))
         sym))
      (t
       (let ((default-suite (find-symbol "SIBYL-TESTS" tests-package)))
         (unless default-suite
           (error "Default test suite SIBYL-TESTS not found"))
         default-suite)))))

(defun %run-tests-count-results (results)
  "Count total, passed, and failed tests from FiveAM results."
  (let ((total 0)
        (passed 0)
        (failed 0)
        (failures nil)
        (fiveam-pkg (find-package "FIVEAM")))
    (unless fiveam-pkg
      (error "FiveAM package not found"))
    (let ((test-result-class (find-symbol "TEST-RESULT" fiveam-pkg))
          (test-passed-class (find-symbol "TEST-PASSED" fiveam-pkg))
          (test-failure-class (find-symbol "TEST-FAILURE" fiveam-pkg))
          (test-suite-result-class (find-symbol "TEST-SUITE-RESULT" fiveam-pkg))
          (test-case-slot (find-symbol "TEST-CASE" fiveam-pkg))
          (name-slot (find-symbol "NAME" fiveam-pkg))
          (reason-accessor (find-symbol "REASON" fiveam-pkg))
          (results-accessor (find-symbol "RESULTS" fiveam-pkg)))
      (labels ((get-test-name (result)
                 (let ((test-case (slot-value result test-case-slot)))
                   (when test-case
                     (slot-value test-case name-slot))))
               (walk-result (result)
                 (cond
                   ;; Test result
                   ((typep result test-result-class)
                    (incf total)
                    (if (typep result test-passed-class)
                        (incf passed)
                        (progn
                          (incf failed)
                          (push (make-hash-table :test 'equal) failures)
                          (let ((failure (car failures))
                                (test-name (get-test-name result)))
                            (setf (gethash "test" failure)
                                  (if test-name
                                      (string-downcase (symbol-name test-name))
                                      "unknown"))
                            (when (typep result test-failure-class)
                              (setf (gethash "reason" failure)
                                    (format nil "~a" (funcall reason-accessor result))))))))
                   ;; Suite result - walk children
                   ((typep result test-suite-result-class)
                    (dolist (child (funcall results-accessor result))
                      (walk-result child)))
                   ;; List of results
                   ((listp result)
                    (dolist (r result)
                      (walk-result r))))))
        (walk-result results))
      (values total passed failed (nreverse failures)))))

(deftool "run-tests"
    (:description "Run FiveAM tests programmatically and return structured results."
     :parameters ((:name "suite" :type "string" :required nil
                   :description "Suite name (e.g., \"sibyl-tests\"), defaults to all tests")
                  (:name "test" :type "string" :required nil
                   :description "Specific test name (e.g., \"read-sexp-basic\")")))
  (block run-tests
    (let* ((suite-name (getf args :suite))
           (test-name (getf args :test))
           (target (%run-tests-resolve-target suite-name test-name))
           (fiveam-pkg (find-package "FIVEAM")))
      
      (unless fiveam-pkg
        (error "FiveAM package not found. Load FiveAM first."))
      
      (let ((run-fn (find-symbol "RUN" fiveam-pkg)))
        (unless run-fn
          (error "FiveAM:RUN function not found"))
        
        ;; Suppress output during test execution
        (let* ((*standard-output* (make-string-output-stream))
               (*error-output* (make-string-output-stream))
               (results (funcall run-fn target)))
          
          ;; Build result structure
          (multiple-value-bind (total passed failed failures)
              (%run-tests-count-results results)
            (let ((result (make-hash-table :test 'equal)))
              (setf (gethash "total" result) total)
              (setf (gethash "passed" result) passed)
              (setf (gethash "failed" result) failed)
              (setf (gethash "failures" result) failures)
              result)))))))

;;; ============================================================
;;; Programmatic test generation
;;; ============================================================

(defun %write-test-validate-name (name)
  "Validate test NAME is a non-empty string."
  (unless (and (stringp name) (string/= name ""))
    (error "Test name must be a non-empty string: ~a" name))
  name)

(defun %write-test-validate-body (body)
  "Validate test BODY is a non-empty string and valid Lisp."
  (unless (and (stringp body) (string/= body ""))
    (error "Test body must be a non-empty string: ~a" body))
  ;; Validate it's valid Lisp
  (let ((*read-eval* nil))
    (handler-case
        (read-from-string body)
      (error (e)
        (error "Invalid test body syntax: ~a" e))))
  body)

(defun %write-test-check-duplicate (name package)
  "Check if a test with NAME already exists in PACKAGE."
  (let* ((test-symbol (intern (string-upcase name) package))
         (fiveam-pkg (find-package "FIVEAM"))
         (get-test-fn (and fiveam-pkg (find-symbol "GET-TEST" fiveam-pkg))))
    (when (and get-test-fn (funcall get-test-fn test-symbol))
      (error "Test ~a already exists" name))))

(defun %write-test-generate-code (name body)
  "Generate test definition code."
  (format nil "(test ~a~%  \"Auto-generated test\"~%  ~a)"
          name body))

(defun %write-test-register-in-memory (test-code package-name)
  "Register test in memory using eval-form."
  (let ((result (execute-tool "eval-form"
                              (list (cons "form" test-code)
                                    (cons "package" package-name)))))
    (when (search "blocked unsafe form" (string-downcase result))
      (error "Failed to register test: ~a" result))
    (when (search "timeout" (string-downcase result))
      (error "Failed to register test: ~a" result))
    result))

(defun %write-test-append-to-file (file test-code)
  "Append test code to FILE."
  (unless (uiop:file-exists-p file)
    (error "Test file not found: ~a" file))
  (with-open-file (stream file :direction :output
                          :if-exists :append
                          :if-does-not-exist :error)
    (terpri stream)
    (write-string test-code stream)
    (terpri stream)))

(deftool "write-test"
    (:description "Generate and register a FiveAM test programmatically."
     :parameters ((:name "name" :type "string" :required t
                   :description "Test name (e.g., \"my-new-test\")")
                  (:name "suite" :type "string" :required nil
                   :description "Suite name (default: \"sibyl-tests\")")
                  (:name "body" :type "string" :required t
                   :description "Test body as S-expression string (e.g., \"(is (equal 1 1))\")")
                  (:name "file" :type "string" :required nil
                   :description "Target file (default: \"tests/sexp-tools-test.lisp\")")))
  (block write-test
    (let* ((name (%write-test-validate-name (getf args :name)))
           (suite (or (getf args :suite) "sibyl-tests"))
           (body (%write-test-validate-body (getf args :body)))
           (file (or (getf args :file)
                     (namestring
                      (asdf:system-relative-pathname :sibyl
                                                     "tests/sexp-tools-test.lisp"))))
           (package-name "SIBYL.TESTS")
           (package (find-package package-name)))
      
      (unless package
        (error "Package not found: ~a" package-name))
      
      ;; Check for duplicate
      (%write-test-check-duplicate name package)
      
      ;; Generate test code
      (let ((test-code (%write-test-generate-code name body)))
        
        ;; Register in-memory
        (%write-test-register-in-memory test-code package-name)
        
        ;; Persist to file
        (%write-test-append-to-file file test-code)
        
        (format nil "Success: Test ~a created and registered in ~a"
                name (file-namestring file))))))

#|
;;; ============================================================
;;; Codebase architecture map
;;; ============================================================

(defun %codebase-map-read-forms (path)
  (let ((*read-eval* nil))
    (with-open-file (stream path :direction :input)
      (let ((eof (gensym "EOF"))
            (forms nil))
        (loop for form = (read stream nil eof)
              until (eq form eof)
              do (push form forms))
        (nreverse forms)))))

(defun %codebase-map-normalize-name (value)
  (cond
    ((stringp value) value)
    ((symbolp value) (string-downcase (symbol-name value)))
    (t (prin1-to-string value))))

(defun %codebase-map-find-system-form (forms system-name)
  (let ((target (string-downcase system-name)))
    (find-if (lambda (form)
               (and (consp form)
                    (member (first form) '(asdf:defsystem defsystem) :test #'equal)
                    (let ((name (%codebase-map-normalize-name (second form))))
                      (and name (string= (string-downcase name) target)))))
             forms)))

(defun %codebase-map-find-src-components (components)
  (let ((src-component
          (find-if (lambda (component)
                     (and (consp component)
                          (eq (first component) :module)
                          (string= (string-downcase
                                    (%codebase-map-normalize-name (second component)))
                                   "src")))
                   components)))
    (and src-component (getf (cddr src-component) :components))))

(defun %codebase-map-extract-src-components (asdf-path)
  (let* ((forms (%codebase-map-read-forms asdf-path))
         (system-form (%codebase-map-find-system-form forms "sibyl")))
    (unless system-form
      (error "System definition not found in ~a" asdf-path))
    (let* ((components (getf (cddr system-form) :components))
           (src-components (%codebase-map-find-src-components components)))
      (unless src-components
        (error "No src module found in ~a" asdf-path))
      src-components)))

(defun %codebase-map-path-for (segments)
  (format nil "~{~a~^/~}.lisp" segments))

(defun %codebase-map-collect-file-paths (components parent)
  (let ((paths nil))
    (dolist (component components (nreverse paths))
      (when (consp component)
        (let ((kind (first component))
              (name (second component)))
          (cond
            ((eq kind :file)
             (push (%codebase-map-path-for
                    (append parent (list (%codebase-map-normalize-name name))))
                   paths))
            ((eq kind :module)
             (let* ((module-name (%codebase-map-normalize-name name))
                    (module-components (getf (cddr component) :components))
                    (nested (%codebase-map-collect-file-paths
                             module-components
                             (append parent (list module-name)))))
               (setf paths (nconc paths nested))))))))))

(defun %codebase-map-collect-module-specs (components parent)
  (let ((modules nil))
    (dolist (component components (nreverse modules))
      (when (consp component)
        (let ((kind (first component))
              (name (second component)))
          (cond
            ((eq kind :module)
             (let* ((module-name (%codebase-map-normalize-name name))
                    (module-components (getf (cddr component) :components))
                    (paths (%codebase-map-collect-file-paths
                            module-components
                            (append parent (list module-name)))))
               (push (list :name module-name :paths paths) modules)))
            ((eq kind :file)
             (let* ((file-name (%codebase-map-normalize-name name))
                    (path (%codebase-map-path-for (append parent (list file-name)))))
               (push (list :name file-name :paths (list path)) modules)))))))))

(defun %codebase-map-parse-read-sexp-result (json)
  (let ((parsed (yason:parse json :object-as :hash-table)))
    (if (vectorp parsed)
        (coerce parsed 'list)
        parsed)))

(defun %codebase-map-safe-read-form (form-string)
  (let ((*read-eval* nil))
    (handler-case
        (multiple-value-bind (form) (read-from-string form-string)
          form)
      (error () nil))))

(defun %codebase-map-format-args (args)
  (when args
    (prin1-to-string args)))

(defun %codebase-map-defun-details (form)
  (let* ((tail (cddr form))
         (lambda-list (first tail))
         (body (rest tail))
         (docstring (and body (stringp (first body)) (first body))))
    (values (%codebase-map-format-args lambda-list) docstring)))

(defun %codebase-map-defmethod-details (form)
  (let ((tail (cddr form)))
    (loop while (and tail (not (listp (first tail))))
          do (setf tail (rest tail)))
    (let ((lambda-list (first tail))
          (body (rest tail)))
      (values (%codebase-map-format-args lambda-list)
              (and body (stringp (first body)) (first body))))))

(defun %codebase-map-defclass-docstring (options)
  (when (listp options)
    (dolist (option options)
      (when (and (consp option) (eq (first option) :documentation))
        (let ((value (second option)))
          (when (stringp value)
            (return value)))))))

(defun %codebase-map-defclass-details (form)
  (let* ((superclasses (third form))
         (slots (fourth form))
         (options (cddddr form))
         (docstring (%codebase-map-defclass-docstring options)))
    (values (%codebase-map-format-args (list superclasses slots)) docstring)))

(defun %codebase-map-deftool-details (form)
  (let* ((options (third form))
         (description (and (listp options) (getf options :description)))
         (parameters (and (listp options) (getf options :parameters))))
    (values (%codebase-map-format-args parameters)
            (and (stringp description) description))))

(defun %codebase-map-defvar-details (form)
  (let* ((init (third form))
         (doc (fourth form)))
    (cond
      ((stringp init) (values nil init))
      ((stringp doc) (values (%codebase-map-format-args init) doc))
      (t (values (%codebase-map-format-args init) nil)))))

(defun %codebase-map-extract-definition-details (type form-string)
  (let* ((type-key (and type (string-downcase type)))
         (form (%codebase-map-safe-read-form form-string)))
    (cond
      ((and form (string= type-key "defun"))
       (%codebase-map-defun-details form))
      ((and form (string= type-key "defmethod"))
       (%codebase-map-defmethod-details form))
      ((and form (string= type-key "defclass"))
       (%codebase-map-defclass-details form))
      ((and form (string= type-key "deftool"))
       (%codebase-map-deftool-details form))
      ((and form (or (string= type-key "defvar")
                     (string= type-key "defparameter")))
       (%codebase-map-defvar-details form))
      (t (values nil nil)))))

(defun %codebase-map-definition-entry (entry)
  (let* ((definition (make-hash-table :test 'equal))
         (type (gethash "type" entry))
         (name (gethash "name" entry))
         (form-string (gethash "form" entry))
         (start-line (gethash "start_line" entry))
         (end-line (gethash "end_line" entry)))
    (setf (gethash "name" definition) name)
    (setf (gethash "type" definition) type)
    (setf (gethash "args" definition) nil)
    (setf (gethash "docstring" definition) nil)
    (when start-line
      (setf (gethash "start_line" definition) start-line))
    (when end-line
      (setf (gethash "end_line" definition) end-line))
    (multiple-value-bind (args docstring)
        (%codebase-map-extract-definition-details type form-string)
      (setf (gethash "args" definition) args)
      (setf (gethash "docstring" definition) docstring))
    definition))

(defun %codebase-map-read-definitions (path)
  (let* ((result (execute-tool "read-sexp" (list (cons "path" path))))
         (entries (%codebase-map-parse-read-sexp-result result)))
    (mapcar #'%codebase-map-definition-entry entries)))

(defun %codebase-map-file-entry (path detail-level)
  (let ((entry (make-hash-table :test 'equal)))
    (setf (gethash "path" entry) path)
    (when (string= detail-level "full")
      (handler-case
          (setf (gethash "definitions" entry)
                (%codebase-map-read-definitions path))
        (error (e)
          (setf (gethash "definitions" entry) nil)
          (setf (gethash "error" entry)
                (format nil "Failed to read definitions: ~a" e)))))
    entry))

(defun %codebase-map-module-entry (name paths detail-level)
  (let ((module (make-hash-table :test 'equal)))
    (setf (gethash "name" module) name)
    (setf (gethash "files" module)
          (mapcar (lambda (path)
                    (%codebase-map-file-entry path detail-level))
                  paths))
    module))

(deftool "codebase-map"
    (:description "Map Sibyl's codebase modules, files, and definitions."
     :parameters ((:name "detail-level" :type "string" :required nil
                   :description "Detail level: summary (default) or full")
                  (:name "asdf-path" :type "string" :required nil
                   :description "Override path to sibyl.asd for diagnostics")))
  (block codebase-map
    (let* ((detail-input (getf args :detail-level))
           (detail-level (string-downcase (or detail-input "summary")))
           (asdf-path (or (getf args :asdf-path)
                          (asdf:system-relative-pathname :sibyl "sibyl.asd"))))
      (unless (member detail-level '("summary" "full") :test #'string=)
        (return-from codebase-map
          (format nil "Error: detail-level must be \"summary\" or \"full\". Got: ~a"
                  detail-input)))
      (unless (uiop:file-exists-p asdf-path)
        (return-from codebase-map
          (format nil "Error: ASDF file not found: ~a" asdf-path)))
      (handler-case
          (let* ((src-components (%codebase-map-extract-src-components asdf-path))
                 (module-specs (%codebase-map-collect-module-specs
                                src-components
                                (list "src")))
                 (modules (mapcar (lambda (spec)
                                    (%codebase-map-module-entry
                                     (getf spec :name)
                                     (getf spec :paths)
                                     detail-level))
                                  module-specs))
                 (result (make-hash-table :test 'equal)))
            (setf (gethash "detail_level" result) detail-level)
            (setf (gethash "modules" result) modules)
            result)
        (error (e)
          (format nil "Error: ~a" e))))))
|#
