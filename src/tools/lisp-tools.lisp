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
;;; Self-analysis: suggest-improvements
;;; ============================================================

(defun %suggest-improvements-normalize-scope (scope)
  "Normalize SCOPE string, defaulting to \"all\"."
  (let ((value (string-downcase (string-trim-whitespace (or scope "")))))
    (if (string= value "") "all" value)))

(defun %suggest-improvements-ensure-list (value)
  (cond
    ((vectorp value) (coerce value 'list))
    ((null value) nil)
    (t value)))

(defun %suggest-improvements-parse-json (json)
  (yason:parse json :object-as :hash-table))

(defun %suggest-improvements-parse-sexp-result (json)
  (let ((parsed (%suggest-improvements-parse-json json)))
    (cond
      ((vectorp parsed) (coerce parsed 'list))
      ((hash-table-p parsed) (list parsed))
      (t parsed))))

(defun %suggest-improvements-map-modules (map)
  (%suggest-improvements-ensure-list (gethash "modules" map)))

(defun %suggest-improvements-module-name (module)
  (gethash "name" module))

(defun %suggest-improvements-module-files (module)
  (%suggest-improvements-ensure-list (gethash "files" module)))

(defun %suggest-improvements-file-path (file)
  (gethash "path" file))

(defun %suggest-improvements-resolve-path (path)
  (let ((pathname (pathname path)))
    (if (uiop:absolute-pathname-p pathname)
        (namestring pathname)
        (namestring (asdf:system-relative-pathname :sibyl path)))))

(defun %suggest-improvements-file-match-p (scope path)
  (let* ((scope-lower (string-downcase scope))
         (path-lower (string-downcase path))
         (absolute (ignore-errors (%suggest-improvements-resolve-path path)))
         (absolute-lower (and absolute (string-downcase absolute)))
         (file-name (string-downcase (file-namestring (pathname path)))))
    (or (string= scope-lower path-lower)
        (and absolute-lower (string= scope-lower absolute-lower))
        (string= scope-lower file-name)
        (and (> (length path-lower) (length scope-lower))
             (string= scope-lower
                      (subseq path-lower (- (length path-lower)
                                            (length scope-lower)))))
        (and absolute-lower
             (> (length absolute-lower) (length scope-lower))
             (string= scope-lower
                      (subseq absolute-lower (- (length absolute-lower)
                                                (length scope-lower))))))))

(defun %suggest-improvements-collect-scope-files (map scope)
  "Collect file paths from MAP based on SCOPE."
  (let* ((normalized (%suggest-improvements-normalize-scope scope))
         (modules (%suggest-improvements-map-modules map)))
    (cond
      ((string= normalized "all")
       (let ((paths nil))
         (dolist (module modules (nreverse paths))
           (dolist (file (%suggest-improvements-module-files module))
             (let ((path (%suggest-improvements-file-path file)))
               (when path
                 (push path paths)))))))
      (t
       (let* ((module (find normalized modules
                            :key (lambda (m)
                                   (string-downcase (or (%suggest-improvements-module-name m) "")))
                            :test #'string=)))
         (cond
           (module
            (let ((paths nil))
              (dolist (file (%suggest-improvements-module-files module) (nreverse paths))
                (let ((path (%suggest-improvements-file-path file)))
                  (when path
                    (push path paths))))))
           (t
            (let ((matched-path nil))
              (dolist (module modules)
                (dolist (file (%suggest-improvements-module-files module))
                  (let ((path (%suggest-improvements-file-path file)))
                    (when (and path
                               (%suggest-improvements-file-match-p normalized path))
                      (setf matched-path path)))))
              (if matched-path
                  (list matched-path)
                  (error "Unknown scope: ~a" scope))))))))))

(defun %suggest-improvements-read-sexp (path)
  (let* ((result (execute-tool "read-sexp"
                               (list (cons "path" path))))
         (entries (%suggest-improvements-parse-sexp-result result)))
    (%suggest-improvements-ensure-list entries)))

(defun %suggest-improvements-collect-definitions (files)
  "Collect definition metadata from FILES using read-sexp."
  (let ((definitions nil))
    (dolist (path files (nreverse definitions))
      (handler-case
          (let* ((resolved (%suggest-improvements-resolve-path path))
                 (entries (%suggest-improvements-read-sexp resolved)))
            (dolist (entry entries)
              (push (list :file path
                          :type (gethash "type" entry)
                          :name (gethash "name" entry)
                          :start-line (gethash "start_line" entry)
                          :form (gethash "form" entry))
                    definitions)))
        (error () nil)))))

(defun %suggest-improvements-public-name-p (name)
  (and (stringp name)
       (string/= name "")
       (not (char= (char name 0) #\%))))

(defun %suggest-improvements-parse-form (form-string)
  (let ((*read-eval* nil))
    (handler-case
        (read-from-string form-string)
      (error () nil))))

(defun %suggest-improvements-docstring-present-p (form)
  (when (consp form)
    (let ((head (string-downcase (symbol-name (first form)))))
      (when (member head '("defun" "defmethod") :test #'string=)
        (let* ((tail (rest form))
               (lambda-pos (position-if #'listp tail)))
          (when lambda-pos
            (let ((candidate (nth (1+ lambda-pos) tail)))
              (stringp candidate))))))))

(defun %suggest-improvements-form-contains-symbol-p (form names)
  (let ((targets (mapcar #'string-downcase names)))
    (labels ((walk (node)
               (cond
                 ((symbolp node)
                  (member (string-downcase (symbol-name node))
                          targets
                          :test #'string=))
                 ((consp node)
                  (or (walk (car node))
                      (walk (cdr node))))
                 (t nil))))
      (walk form))))

(defun %suggest-improvements-form-has-error-handling-p (form)
  (%suggest-improvements-form-contains-symbol-p
   form
   '("handler-case" "ignore-errors" "handler-bind" "restart-case")))

(defun %suggest-improvements-form-has-risky-ops-p (form)
  (%suggest-improvements-form-contains-symbol-p
   form
   '("with-open-file" "read-file-string" "write-file-string"
     "run-program" "execute-tool")))

(defun %suggest-improvements-package-for-file (file-path)
  (let ((lower (string-downcase file-path)))
    (cond
      ((search "src/tools/" lower) "SIBYL.TOOLS")
      ((search "src/agent/" lower) "SIBYL.AGENT")
      ((search "src/llm/" lower) "SIBYL.LLM")
      ((search "src/repl/" lower) "SIBYL.REPL")
      ((search "src/util/" lower) "SIBYL.UTIL")
      ((search "src/conditions/" lower) "SIBYL.CONDITIONS")
      ((search "src/config/" lower) "SIBYL.CONFIG")
      ((search "src/system/" lower) "SIBYL.SYSTEM")
      (t "SIBYL"))))

(defun %suggest-improvements-callers-count (function-string)
  "Return caller count from who-calls output, or NIL."
  (when (find-tool "who-calls")
    (handler-case
        (let* ((result (execute-tool "who-calls"
                                     (list (cons "function" function-string))))
               (lower (string-downcase result))
               (marker "callers ("))
          (cond
            ((search marker lower)
             (let* ((start (search marker lower))
                    (from (+ start (length marker)))
                    (end (position #\) lower :start from)))
               (when end
                 (parse-integer (subseq lower from end)
                                :junk-allowed t))))
            ((search "none found" lower) 0)
            ((search "not found" lower) 0)
            (t nil)))
      (error () nil))))

(defun %suggest-improvements-make-suggestion (category priority description
                                              file line rationale)
  (let ((entry (make-hash-table :test 'equal)))
    (setf (gethash "priority" entry) priority)
    (setf (gethash "category" entry) category)
    (setf (gethash "description" entry) description)
    (setf (gethash "file" entry) file)
    (setf (gethash "line" entry) (or line 0))
    (setf (gethash "rationale" entry) rationale)
    entry))

(defun %suggest-improvements-suggestion-plist (suggestion)
  "Convert suggestion hash table to plist for store-suggestions."
  (list :id (gethash "id" suggestion)
        :description (gethash "description" suggestion)
        :rationale (gethash "rationale" suggestion)
        :priority (gethash "priority" suggestion)
        :category (gethash "category" suggestion)
        :file (gethash "file" suggestion)
        :line (gethash "line" suggestion)))

(defun %suggest-improvements-split-lines (content)
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

(defun %suggest-improvements-read-tests-content ()
  (let* ((tests-dir (asdf:system-relative-pathname :sibyl "tests/"))
         (files (and (probe-file tests-dir)
                     (%codebase-map-find-lisp-files tests-dir))))
    (string-downcase
     (string-join (string #\Newline)
                  (mapcar (lambda (path)
                            (uiop:read-file-string path))
                          files)))))

(defun %suggest-improvements-missing-docstrings (definitions)
  (let ((suggestions nil)
        (count 0))
    (dolist (definition definitions (nreverse suggestions))
      (let* ((type (getf definition :type))
             (name (getf definition :name))
             (form-string (getf definition :form))
             (form (%suggest-improvements-parse-form form-string)))
        (when (and (< count 3)
                   (member type '("defun" "defmethod") :test #'string=)
                   (%suggest-improvements-public-name-p name)
                   form
                   (not (%suggest-improvements-docstring-present-p form)))
          (incf count)
          (push (%suggest-improvements-make-suggestion
                 "missing-docstrings"
                 "medium"
                 (format nil "Function \"~a\" lacks a docstring." name)
                 (getf definition :file)
                 (getf definition :start-line)
                 "Public functions should document intent for maintainability and self-analysis.")
                 suggestions))))))

(defun %suggest-improvements-missing-tests (definitions tests-content baseline-callers)
  (let ((suggestions nil)
        (seen (make-hash-table :test 'equal))
        (tool-count 0)
        (function-count 0))
    (dolist (definition definitions (nreverse suggestions))
      (let* ((type (getf definition :type))
             (name (getf definition :name)))
        (when (and name (stringp name) (string/= name ""))
          (let ((key (string-downcase name)))
            (unless (gethash key seen)
              (setf (gethash key seen) t)
              (when (not (search key tests-content))
                (cond
                  ((and (string= type "deftool") (< tool-count 3))
                   (incf tool-count)
                   (push (%suggest-improvements-make-suggestion
                          "test-coverage"
                          (if (and baseline-callers (> baseline-callers 0))
                              "high"
                              "medium")
                          (format nil "Tool \"~a\" lacks explicit test coverage." name)
                          (getf definition :file)
                          (getf definition :start-line)
                          "User-facing tools should have regression tests to avoid behavioral drift.")
                         suggestions))
                  ((and (member type '("defun" "defmethod") :test #'string=)
                        (%suggest-improvements-public-name-p name)
                        (< function-count 2))
                   (incf function-count)
                   (let* ((package (%suggest-improvements-package-for-file
                                    (getf definition :file)))
                          (qualified (format nil "~a::~a"
                                             (string-downcase package)
                                             name))
                          (callers (%suggest-improvements-callers-count qualified))
                          (priority (if (and callers (> callers 0))
                                        "high"
                                        "medium"))
                          (rationale (if (and callers (> callers 0))
                                         (format nil "Function has ~a internal callers; missing tests increase regression risk."
                                                 callers)
                                         "Public functions benefit from explicit tests to lock in expectations.")))
                     (push (%suggest-improvements-make-suggestion
                            "test-coverage"
                            priority
                            (format nil "Function \"~a\" appears untested." name)
                            (getf definition :file)
                            (getf definition :start-line)
                            rationale)
                           suggestions))))))))))))

(defun %suggest-improvements-todo-comments (files)
  (let ((suggestions nil)
        (count 0))
    (dolist (path files (nreverse suggestions))
      (when (< count 3)
        (handler-case
            (let* ((resolved (%suggest-improvements-resolve-path path))
                   (content (uiop:read-file-string resolved))
                   (lines (%suggest-improvements-split-lines content))
                   (line-number 0))
              (dolist (line lines)
                (incf line-number)
                (when (and (< count 3)
                           (let ((lower (string-downcase line)))
                             (or (search "todo" lower)
                                 (search "fixme" lower))))
                  (incf count)
                  (push (%suggest-improvements-make-suggestion
                         "todo"
                         "medium"
                         (format nil "TODO comment: ~a"
                                 (string-trim-whitespace line))
                         path
                         line-number
                         "Outstanding TODOs should be triaged or resolved to reduce uncertainty.")
                         suggestions))))
          (error () nil))))))

(defun %suggest-improvements-error-handling (definitions)
  (let ((suggestions nil)
        (count 0))
    (dolist (definition definitions (nreverse suggestions))
      (let* ((type (getf definition :type))
             (name (getf definition :name))
             (form-string (getf definition :form))
             (form (%suggest-improvements-parse-form form-string)))
        (when (and (< count 2)
                   (string= type "defun")
                   (%suggest-improvements-public-name-p name)
                   form
                   (%suggest-improvements-form-has-risky-ops-p form)
                   (not (%suggest-improvements-form-has-error-handling-p form)))
          (incf count)
          (let* ((package (%suggest-improvements-package-for-file
                           (getf definition :file)))
                 (qualified (format nil "~a::~a"
                                    (string-downcase package)
                                    name))
                 (callers (%suggest-improvements-callers-count qualified))
                 (priority (if (and callers (> callers 0))
                               "high"
                               "medium"))
                 (rationale (if (and callers (> callers 0))
                                (format nil "Function has ~a internal callers; missing error handling risks cascading failures."
                                        callers)
                                "I/O and tool calls should handle failures to avoid cascading errors.")))
            (push (%suggest-improvements-make-suggestion
                   "error-handling"
                   priority
                   (format nil "Function \"~a\" performs risky operations without error handling." name)
                   (getf definition :file)
                   (getf definition :start-line)
                   rationale)
                   suggestions)))))))

(defun %suggest-improvements-duplications (definitions)
  (let ((table (make-hash-table :test 'equal))
        (suggestions nil)
        (count 0))
    (dolist (definition definitions)
      (let ((form (getf definition :form)))
        (when (and form (stringp form))
          (let ((key (string-downcase (string-trim-whitespace form))))
            (push definition (gethash key table))))))
    (maphash
     (lambda (key defs)
       (declare (ignore key))
       (when (< count 2)
         (let* ((paths (remove-duplicates
                        (mapcar (lambda (def) (getf def :file)) defs)
                        :test #'string=))
                (name (getf (first defs) :name)))
           (when (> (length paths) 1)
             (incf count)
             (push (%suggest-improvements-make-suggestion
                    "code-duplication"
                    "low"
                    (format nil "Identical definition~@[ for \"~a\"~] appears in: ~{~a~^, ~}."
                            name paths)
                    (getf (first defs) :file)
                    (getf (first defs) :start-line)
                    "Consolidating duplicates reduces drift and simplifies maintenance.")
                   suggestions)))))
     table)
    (nreverse suggestions)))

(defun %suggest-improvements-priority-rank (priority)
  (cond
    ((string= priority "high") 0)
    ((string= priority "medium") 1)
    (t 2)))

(defun %suggest-improvements-sort (suggestions)
  (sort (copy-list suggestions)
        (lambda (a b)
          (let ((rank-a (%suggest-improvements-priority-rank
                         (gethash "priority" a)))
                (rank-b (%suggest-improvements-priority-rank
                         (gethash "priority" b))))
            (if (/= rank-a rank-b)
                (< rank-a rank-b)
                (string< (gethash "category" a)
                         (gethash "category" b)))))))

(defun %suggest-improvements-dedupe (suggestions)
  (let ((seen (make-hash-table :test 'equal))
        (result nil))
    (dolist (suggestion suggestions (nreverse result))
      (let ((key (format nil "~a|~a|~a"
                         (gethash "category" suggestion)
                         (gethash "file" suggestion)
                         (gethash "line" suggestion))))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push suggestion result))))))

(defun %suggest-improvements-limit (suggestions max-count)
  (let ((count (length suggestions)))
    (if (and max-count (> count max-count))
        (subseq suggestions 0 max-count)
        suggestions)))

(defun %suggest-improvements-assign-ids (suggestions)
  (loop for suggestion in suggestions
        for idx from 1
        do (setf (gethash "id" suggestion) idx))
  suggestions)

(defun %suggest-improvements-build-note (scope suggestions)
  (with-output-to-string (stream)
    (format stream "## [~a] suggest-improvements~%~%" (timestamp-now))
    (format stream "Scope: ~a~%~%" scope)
    (if (null suggestions)
        (progn
          (write-string "Suggestions: none" stream)
          (terpri stream)
          (terpri stream))
        (progn
          (write-string "Suggestions:" stream)
          (terpri stream)
          (dolist (suggestion suggestions)
            (let ((line (gethash "line" suggestion)))
              (format stream "- [~a][~a] ~a (~a:~a)~%"
                      (gethash "priority" suggestion)
                      (gethash "category" suggestion)
                      (gethash "description" suggestion)
                      (gethash "file" suggestion)
                      (if (integerp line) line 0)))
            (let ((rationale (gethash "rationale" suggestion)))
              (when rationale
                (format stream "  - Rationale: ~a~%" rationale))))
          (terpri stream)))))

(defun %suggest-improvements-append-learnings (scope suggestions)
  (let* ((path (asdf:system-relative-pathname
                :sibyl
                ".sisyphus/notepads/self-development-roadmap/learnings.md"))
         (note (%suggest-improvements-build-note scope suggestions)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (write-string note stream))))

(deftool "suggest-improvements"
    (:description "Analyze the codebase and suggest improvements."
     :parameters ((:name "scope" :type "string" :required nil
                   :description "Scope: all, module-name, or file-path")))
  (block suggest-improvements
    (let* ((scope (getf args :scope))
           (normalized-scope (%suggest-improvements-normalize-scope scope))
           (map-result (execute-tool "codebase-map"
                                     '(("detail-level" . "summary"))))
           (map (%suggest-improvements-parse-json map-result))
           (files (%suggest-improvements-collect-scope-files map normalized-scope))
           (definitions (%suggest-improvements-collect-definitions files))
           (tests-content (%suggest-improvements-read-tests-content))
           (baseline-callers (%suggest-improvements-callers-count
                              "sibyl.tools:execute-tool-call"))
           (suggestions nil))
      (setf suggestions
            (append (%suggest-improvements-missing-tests
                     definitions tests-content baseline-callers)
                    (%suggest-improvements-missing-docstrings definitions)
                    (%suggest-improvements-todo-comments files)
                    (%suggest-improvements-error-handling definitions)
                    (%suggest-improvements-duplications definitions)))
      (setf suggestions (%suggest-improvements-dedupe suggestions))
      (setf suggestions (%suggest-improvements-sort suggestions))
      (setf suggestions (%suggest-improvements-limit suggestions 10))
      (setf suggestions (%suggest-improvements-assign-ids suggestions))
      (%suggest-improvements-append-learnings normalized-scope suggestions)
      (let ((result (make-hash-table :test 'equal)))
        (setf (gethash "suggestions" result) suggestions)
        result))))

;;; ============================================================
;;; Self-analysis: self-assess
;;; ============================================================

(defvar *self-assess-running* nil
  "Guard against recursive test runs inside self-assess.")

(defvar *self-assess-last-test-results* nil
  "Cache of the most recent run-tests result (parsed).")

(defparameter *self-assess-cyclomatic-branches*
  '("if" "when" "unless" "cond" "case" "typecase" "ecase" "ccase"
    "handler-case" "restart-case" "and" "or")
  "Symbols counted as cyclomatic decision points.")

(defparameter *self-assess-tool-categories*
  '(("file_ops" "read-file" "write-file" "list-directory" "grep" "file-info" "shell")
    ("lisp_introspection" "read-sexp" "describe-symbol" "macroexpand-form"
                           "package-symbols" "who-calls" "codebase-map")
    ("testing" "run-tests" "write-test")
    ("self_modification" "safe-redefine" "sync-to-file")
    ("analysis" "suggest-improvements" "self-assess")
    ("evaluation" "eval-form"))
  "Category map for registered tool names.")

(defun %self-assess-ensure-list (value)
  (cond
    ((vectorp value) (coerce value 'list))
    ((null value) nil)
    (t value)))

(defun %self-assess-parse-json (json)
  (yason:parse json :object-as :hash-table))

(defun %self-assess-collect-files-from-map (map)
  (let* ((modules (%self-assess-ensure-list (gethash "modules" map)))
         (paths nil))
    (dolist (module modules (nreverse paths))
      (dolist (file (%self-assess-ensure-list (gethash "files" module)))
        (let ((path (gethash "path" file)))
          (when path
            (push path paths)))))))

(defun %self-assess-count-lines (content)
  (cond
    ((null content) 0)
    ((string= content "") 0)
    (t (1+ (count #\Newline content)))))

(defun %self-assess-total-lines (paths)
  (let ((total 0))
    (dolist (path paths total)
      (handler-case
          (let* ((resolved (%suggest-improvements-resolve-path path))
                 (content (uiop:read-file-string resolved)))
            (incf total (%self-assess-count-lines content)))
        (error () nil)))))

(defun %self-assess-definition-function-p (definition)
  (let ((type (getf definition :type)))
    (member type '("defun" "defmethod") :test #'string=)))

(defun %self-assess-collect-function-names (definitions)
  (let ((names nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (definition definitions (nreverse names))
      (let ((name (getf definition :name)))
        (when (and name
                   (%self-assess-definition-function-p definition)
                   (%suggest-improvements-public-name-p name))
          (let ((key (string-downcase name)))
            (unless (gethash key seen)
              (setf (gethash key seen) t)
              (push name names))))))))

(defun %self-assess-parse-form (form-string)
  (let ((*read-eval* nil))
    (handler-case
        (multiple-value-bind (form read-position)
            (read-from-string form-string)
          (declare (ignore read-position))
          form)
      (error () nil))))

(defun %self-assess-cyclomatic-count (form)
  (let ((count 0))
    (labels ((walk (node)
               (when (consp node)
                 (let ((head (car node)))
                   (when (and (symbolp head)
                              (member (string-downcase (symbol-name head))
                                      *self-assess-cyclomatic-branches*
                                      :test #'string=))
                     (incf count)))
                 (walk (car node))
                 (walk (cdr node)))))
      (walk form))
    (1+ count)))

(defun %self-assess-total-cyclomatic (definitions)
  (let ((total 0)
        (count 0))
    (dolist (definition definitions (values total count))
      (when (%self-assess-definition-function-p definition)
        (let* ((form-string (getf definition :form))
               (form (and form-string (%self-assess-parse-form form-string))))
          (when form
            (incf count)
            (incf total (%self-assess-cyclomatic-count form))))))))

(defun %self-assess-test-files ()
  (let* ((tests-dir (asdf:system-relative-pathname :sibyl "tests/"))
         (files (and (probe-file tests-dir)
                     (%codebase-map-find-lisp-files tests-dir))))
    (%self-assess-ensure-list files)))

(defun %self-assess-count-tests-in-content (content)
  (let ((count 0)
        (*read-eval* nil)
        (*package* (or (find-package "SIBYL.TESTS") *package*)))
    (handler-case
        (with-input-from-string (stream content)
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (when (and (consp form)
                              (symbolp (car form))
                              (string= (string-downcase (symbol-name (car form)))
                                       "test"))
                     (incf count))))
      (error () nil))
    count))

(defun %self-assess-count-tests (files)
  (let ((total 0))
    (dolist (file files total)
      (handler-case
          (let ((content (uiop:read-file-string file)))
            (incf total (%self-assess-count-tests-in-content content)))
        (error () nil)))))

(defun %self-assess-untested-functions (function-names tests-content)
  (let ((untested nil))
    (dolist (name function-names (nreverse untested))
      (let ((lower (string-downcase name)))
        (unless (search lower tests-content)
          (push name untested))))))

(defun %self-assess-coverage-estimate (total-functions untested-count)
  (if (and (integerp total-functions) (> total-functions 0))
      (/ (float (- total-functions untested-count))
         total-functions)
      0.0))

(defun %self-assess-tool-category (tool-name)
  (let ((lower (string-downcase tool-name)))
    (dolist (entry *self-assess-tool-categories* "other")
      (let ((category (first entry))
            (names (rest entry)))
        (when (find lower names :test #'string=)
          (return category))))))

(defun %self-assess-categories-counts (tools)
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (tool tools counts)
      (let* ((name (tool-name tool))
             (category (%self-assess-tool-category name))
             (current (gethash category counts 0)))
        (setf (gethash category counts) (1+ current))))))

(defun %self-assess-tool-entries (tools)
  (mapcar (lambda (tool)
            (let ((entry (make-hash-table :test 'equal)))
              (setf (gethash "name" entry) (tool-name tool))
              (setf (gethash "description" entry) (tool-description tool))
              (setf (gethash "category" entry)
                    (%self-assess-tool-category (tool-name tool)))
              entry))
          tools))

(defun %self-assess-tool-name-set (tools)
  (let ((set (make-hash-table :test 'equal)))
    (dolist (tool tools set)
      (setf (gethash (string-downcase (tool-name tool)) set) t))))

(defun %self-assess-capabilities (tool-set execute-tool-description)
  (let ((caps nil))
    (when (and (gethash "read-file" tool-set)
               (gethash "write-file" tool-set))
      (push "Can read and write files" caps))
    (when (or (gethash "list-directory" tool-set)
              (gethash "grep" tool-set))
      (push "Can search and list files/directories" caps))
    (when (gethash "eval-form" tool-set)
      (push "Can evaluate Lisp forms safely" caps))
    (when (and (gethash "read-sexp" tool-set)
               (gethash "describe-symbol" tool-set)
               (gethash "codebase-map" tool-set))
      (push "Can introspect Lisp symbols and codebase structure" caps))
    (when (and (gethash "run-tests" tool-set)
               (gethash "write-test" tool-set))
      (push "Can generate and run tests programmatically" caps))
    (when (and (gethash "safe-redefine" tool-set)
               (gethash "sync-to-file" tool-set))
      (push "Can modify functions safely and sync to disk" caps))
    (when (gethash "suggest-improvements" tool-set)
      (push "Can analyze codebase and propose improvements" caps))
    (when (and execute-tool-description
               (search "status: found" (string-downcase execute-tool-description)))
      (push "Can execute registered tools with error handling" caps))
    (nreverse caps)))

(defun %self-assess-toolset-limitations (tool-set)
  (let ((limitations nil))
    (unless (gethash "safe-redefine" tool-set)
      (push "Cannot safely redefine functions in-memory" limitations))
    (unless (gethash "sync-to-file" tool-set)
      (push "Cannot persist self-modifications to disk" limitations))
    (unless (gethash "run-tests" tool-set)
      (push "Cannot execute tests programmatically" limitations))
    (push "Cannot modify external dependencies or Quicklisp libraries" limitations)
    (push "Cannot modify system prompt or LLM client" limitations)
    (nreverse limitations)))

(defun %self-assess-known-limitations ()
  (list
   "Cannot modify system prompt or LLM client"
   "Requires human approval for self-modifications"
   "No persistent task execution logging yet"
   "Self-modification limited to Sibyl packages"
   "ASDF reload protection required for in-memory modifications"
   "Cannot bypass approval flow or safety guardrails"
   "Requires SBCL (sb-introspect) for full introspection"
   "Tooling is Lisp-only; no multi-language parsing"
   "External dependency changes require manual updates"))

(defun %self-assess-describe-symbol (symbol-string)
  (handler-case
      (execute-tool "describe-symbol" (list (cons "symbol" symbol-string)))
    (error () nil)))

(defun %self-assess-run-tests ()
  (when (find-tool "run-tests")
    (let ((*self-assess-running* t))
      (handler-case
          (let* ((result (execute-tool "run-tests" nil))
                 (parsed (%self-assess-parse-json result)))
            (setf *self-assess-last-test-results* parsed)
            (values parsed nil))
        (error (e)
          (values nil (format nil "~a" e)))))))

(deftool "self-assess"
    (:description "Evaluate Sibyl's current capabilities and limitations."
     :parameters ())
  (block self-assess
    (let* ((map-json (execute-tool "codebase-map"
                                   '(("detail-level" . "summary"))))
           (map (%self-assess-parse-json map-json))
           (modules (%self-assess-ensure-list (gethash "modules" map)))
           (module-count (length modules))
           (src-files (%self-assess-collect-files-from-map map))
           (total-lines (%self-assess-total-lines src-files))
           (definitions (%suggest-improvements-collect-definitions src-files))
           (function-names (%self-assess-collect-function-names definitions))
           (total-functions (length function-names))
           (cyclomatic-total 0)
           (cyclomatic-count 0)
           (package-symbols-json (execute-tool "package-symbols"
                                               '(("package" . "SIBYL.TOOLS"))))
           (package-symbols-parsed (%self-assess-parse-json package-symbols-json))
           (package-symbols (%self-assess-ensure-list package-symbols-parsed))
           (package-symbols-count (length package-symbols))
           (tools (list-tools))
           (tool-set (%self-assess-tool-name-set tools))
           (tool-entries (%self-assess-tool-entries tools))
           (categories (%self-assess-categories-counts tools))
           (execute-tool-description
             (%self-assess-describe-symbol "sibyl.tools:execute-tool"))
           (capabilities (%self-assess-capabilities tool-set execute-tool-description))
           (toolset-limitations (%self-assess-toolset-limitations tool-set))
           (tests-files (%self-assess-test-files))
           (test-files-count (length tests-files))
           (tests-content (%suggest-improvements-read-tests-content))
           (untested-functions (%self-assess-untested-functions function-names tests-content))
           (untested-count (length untested-functions))
           (coverage-estimate (%self-assess-coverage-estimate total-functions untested-count))
           (test-count (%self-assess-count-tests tests-files))
           (test-results nil)
           (test-results-error nil))
      (multiple-value-setq (cyclomatic-total cyclomatic-count)
        (%self-assess-total-cyclomatic definitions))
      (cond
        ((not *self-assess-running*)
         (multiple-value-setq (test-results test-results-error)
           (%self-assess-run-tests)))
        (t
         (setf test-results *self-assess-last-test-results*)))
      (let* ((tests-total (or (and test-results (gethash "total" test-results))
                              test-count))
             (tests-passed (and test-results (gethash "passed" test-results)))
             (tests-failed (and test-results (gethash "failed" test-results)))
             (pass-rate (if (and tests-total (integerp tests-total) (> tests-total 0)
                                 (integerp tests-passed))
                            (/ (float tests-passed) tests-total)
                            nil))
             (codebase (make-hash-table :test 'equal))
             (complexity (make-hash-table :test 'equal))
             (toolset (make-hash-table :test 'equal))
             (test-coverage (make-hash-table :test 'equal))
             (result (make-hash-table :test 'equal)))
        (setf (gethash "total" complexity) cyclomatic-total)
        (setf (gethash "average" complexity)
              (if (> cyclomatic-count 0)
                  (/ (float cyclomatic-total) cyclomatic-count)
                  0.0))
        (setf (gethash "function_count" complexity) cyclomatic-count)
        (setf (gethash "total_lines" codebase) total-lines)
        (setf (gethash "total_functions" codebase) total-functions)
        (setf (gethash "total_modules" codebase) module-count)
        (setf (gethash "test_files" codebase) test-files-count)
        (setf (gethash "cyclomatic_complexity" codebase) complexity)
        (setf (gethash "total_tools" toolset) (hash-table-count *tool-registry*))
        (setf (gethash "package_symbols" toolset) package-symbols-count)
        (setf (gethash "categories" toolset) categories)
        (setf (gethash "capabilities" toolset) capabilities)
        (setf (gethash "limitations" toolset) toolset-limitations)
        (setf (gethash "tools" toolset) tool-entries)
        (setf (gethash "total_tests" test-coverage) tests-total)
        (setf (gethash "passed" test-coverage) tests-passed)
        (setf (gethash "failed" test-coverage) tests-failed)
        (setf (gethash "pass_rate" test-coverage) pass-rate)
        (setf (gethash "untested_functions" test-coverage) untested-count)
        (setf (gethash "coverage_estimate" test-coverage) coverage-estimate)
        (setf (gethash "untested" test-coverage) untested-functions)
        (when test-results-error
          (setf (gethash "test_run_error" test-coverage) test-results-error))
        (setf (gethash "toolset" result) toolset)
        (setf (gethash "codebase" result) codebase)
        (setf (gethash "test_coverage" result) test-coverage)
        (setf (gethash "limitations" result) (%self-assess-known-limitations))
        result))))

;;; ============================================================
;;; Self-analysis: improvement-plan
;;; ============================================================

(defun %improvement-plan-ensure-list (value)
  (cond
    ((vectorp value) (coerce value 'list))
    ((null value) nil)
    (t value)))

(defun %improvement-plan-parse-json (json)
  (yason:parse json :object-as :hash-table))

(defun %improvement-plan-string-contains-p (text keywords)
  (when text
    (let ((lower (string-downcase text)))
      (some (lambda (keyword)
              (search keyword lower))
            keywords))))

(defun %improvement-plan-limitations-match-p (limitations keywords)
  (let ((items (%improvement-plan-ensure-list limitations)))
    (some (lambda (entry)
            (%improvement-plan-string-contains-p entry keywords))
          items)))

(defun %improvement-plan-toolset-has-keyword-p (tools keywords)
  (let ((entries (%improvement-plan-ensure-list tools)))
    (some (lambda (tool)
            (let ((name (gethash "name" tool))
                  (desc (gethash "description" tool)))
              (or (%improvement-plan-string-contains-p name keywords)
                  (%improvement-plan-string-contains-p desc keywords))))
          entries)))

(defun %improvement-plan-make-entry (title description category priority
                                    risk effect timeframe rationale estimated-effort)
  (let ((entry (make-hash-table :test 'equal)))
    (setf (gethash "title" entry) title)
    (setf (gethash "description" entry) description)
    (setf (gethash "category" entry) category)
    (setf (gethash "priority" entry) priority)
    (setf (gethash "risk" entry) risk)
    (setf (gethash "effect" entry) effect)
    (setf (gethash "timeframe" entry) timeframe)
    (setf (gethash "rationale" entry) rationale)
    (setf (gethash "estimated_effort" entry) estimated-effort)
    entry))

(defun %improvement-plan-assign-ids (improvements)
  (loop for improvement in improvements
        for idx from 1
        do (setf (gethash "id" improvement) idx))
  improvements)

(defun %improvement-plan-priority-rank (priority)
  (cond
    ((string= priority "high") 0)
    ((string= priority "medium") 1)
    (t 2)))

(defun %improvement-plan-sort (improvements)
  (sort (copy-list improvements)
        (lambda (a b)
          (let ((rank-a (%improvement-plan-priority-rank
                         (gethash "priority" a)))
                (rank-b (%improvement-plan-priority-rank
                         (gethash "priority" b))))
            (if (/= rank-a rank-b)
                (< rank-a rank-b)
                (string< (gethash "category" a)
                         (gethash "category" b)))))))

(defun %improvement-plan-summary (improvements)
  (let ((summary (make-hash-table :test 'equal)))
    (setf (gethash "total_improvements" summary) (length improvements))
    (setf (gethash "high_priority" summary)
          (count-if (lambda (improvement)
                      (string= (gethash "priority" improvement) "high"))
                    improvements))
    (setf (gethash "medium_priority" summary)
          (count-if (lambda (improvement)
                      (string= (gethash "priority" improvement) "medium"))
                    improvements))
    (setf (gethash "low_priority" summary)
          (count-if (lambda (improvement)
                      (string= (gethash "priority" improvement) "low"))
                    improvements))
    (setf (gethash "short_term" summary)
          (count-if (lambda (improvement)
                      (string= (gethash "timeframe" improvement) "short"))
                    improvements))
    (setf (gethash "medium_term" summary)
          (count-if (lambda (improvement)
                      (string= (gethash "timeframe" improvement) "medium"))
                    improvements))
    (setf (gethash "long_term" summary)
          (count-if (lambda (improvement)
                      (string= (gethash "timeframe" improvement) "long"))
                    improvements))
    summary))

(defun %improvement-plan-estimate-test-effort (untested-count)
  (cond
    ((>= untested-count 40) "6-10 hours")
    ((>= untested-count 20) "4-6 hours")
    ((>= untested-count 10) "3-5 hours")
    (t "2-4 hours")))

(defun %improvement-plan-definition-table (definitions)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (definition definitions table)
      (let ((name (getf definition :name))
            (type (getf definition :type)))
        (when (and name
                   (member type '("defun" "defmethod") :test #'string=))
          (push definition (gethash (string-downcase name) table)))))))

(defun %improvement-plan-callers-for-definition (definition)
  (let* ((name (getf definition :name))
         (file (getf definition :file))
         (package (%suggest-improvements-package-for-file file))
         (qualified (format nil "~a::~a" (string-downcase package) name))
         (callers (%suggest-improvements-callers-count qualified)))
    (or callers 0)))

(defun %improvement-plan-collect-untested-callers (untested definitions)
  (let ((table (%improvement-plan-definition-table definitions))
        (best (make-hash-table :test 'equal)))
    (dolist (name untested)
      (let* ((key (string-downcase name))
             (defs (gethash key table)))
        (dolist (definition defs)
          (let* ((callers (%improvement-plan-callers-for-definition definition))
                 (current (gethash key best)))
            (when (or (null current)
                      (> callers (getf current :callers)))
              (setf (gethash key best)
                    (list :name name
                          :callers callers
                          :file (getf definition :file))))))))
    (let ((entries nil))
      (maphash (lambda (key value)
                 (declare (ignore key))
                 (push value entries))
               best)
      entries)))

(defun %improvement-plan-sort-untested (entries)
  (sort (copy-list entries)
        #'>
        :key (lambda (entry) (or (getf entry :callers) 0))))

(defun %improvement-plan-format-untested-sample (entries)
  (let* ((nonzero (remove-if (lambda (entry)
                               (<= (getf entry :callers) 0))
                             entries))
         (sample (subseq (or nonzero entries)
                         0
                         (min 3 (length (or nonzero entries))))))
    (when sample
      (if nonzero
          (string-join ", "
                       (mapcar (lambda (entry)
                                 (format nil "~a (~a callers)"
                                         (getf entry :name)
                                         (getf entry :callers)))
                               sample))
          (string-join ", "
                       (mapcar (lambda (entry)
                                 (getf entry :name))
                               sample))))))

(defun %improvement-plan-store-suggestions (improvements)
  (let* ((repl-package (find-package "SIBYL.REPL"))
         (store (and repl-package (find-symbol "STORE-SUGGESTIONS" repl-package))))
    (when (and store (fboundp store))
      (funcall store
               (mapcar (lambda (improvement)
                         (list :id (gethash "id" improvement)
                               :description (gethash "title" improvement)
                               :rationale (gethash "rationale" improvement)
                               :priority (gethash "priority" improvement)))
                       improvements)))))

(deftool "improvement-plan"
    (:description "Generate a prioritized improvement plan from self-assessment."
     :parameters ())
  (block improvement-plan
    (let* ((assessment-json (execute-tool "self-assess" nil))
           (assessment (%improvement-plan-parse-json assessment-json))
           (toolset (gethash "toolset" assessment))
           (codebase (gethash "codebase" assessment))
           (test-coverage (gethash "test_coverage" assessment))
           (limitations (%improvement-plan-ensure-list
                         (gethash "limitations" assessment)))
           (tools (%improvement-plan-ensure-list
                   (and toolset (gethash "tools" toolset))))
           (timestamp (timestamp-now))
           (improvements nil))
      ;; Test coverage improvements
      (let* ((untested (%improvement-plan-ensure-list
                        (and test-coverage (gethash "untested" test-coverage))))
             (untested-count (length untested)))
        (when (> untested-count 0)
          (let* ((map-json (execute-tool "codebase-map"
                                         '(("detail-level" . "summary"))))
                 (map (%improvement-plan-parse-json map-json))
                 (files (%self-assess-collect-files-from-map map))
                 (definitions (%suggest-improvements-collect-definitions files))
                 (untested-info (%improvement-plan-collect-untested-callers
                                 untested definitions))
                 (sorted (%improvement-plan-sort-untested untested-info))
                 (high-callers (remove-if (lambda (entry)
                                            (< (getf entry :callers) 3))
                                          sorted))
                 (priority (if high-callers "high"
                               (if (> untested-count 10) "medium" "low")))
                 (sample (%improvement-plan-format-untested-sample sorted))
                 (title (if high-callers
                            "Add tests for high-traffic untested functions"
                            "Improve test coverage for untested functions"))
                 (description (if sample
                                  (format nil "~a functions lack tests; prioritize ~a."
                                          untested-count sample)
                                  (format nil "~a functions lack tests; prioritize critical paths."
                                          untested-count)))
                 (rationale (if high-callers
                                "Functions with 3+ callers need tests to prevent regressions."
                                "Untested public functions increase regression risk."))
                 (estimated-effort (%improvement-plan-estimate-test-effort
                                    untested-count)))
            (push (%improvement-plan-make-entry
                   title description "test-coverage" priority
                   "low" "high" "short" rationale estimated-effort)
                  improvements))))
      ;; Infrastructure gaps from limitations
      (when (%improvement-plan-limitations-match-p
             limitations '("logging" "log"))
        (push (%improvement-plan-make-entry
               "Implement task execution logging"
               "Add persistent logging for task success/failure tracking."
               "infrastructure" "medium" "medium" "high" "medium"
               "Self-assessment notes missing persistent logging for task outcomes."
               "5-8 hours")
              improvements))
      ;; Tooling gaps: debugging/tracing
      (when (not (%improvement-plan-toolset-has-keyword-p
                  tools '("debug" "trace" "profil" "diagnos")))
        (push (%improvement-plan-make-entry
               "Add debugging and tracing tools"
               "Provide lightweight tracing or debug inspection for tool runs and agent steps."
               "tooling" "medium" "medium" "high" "medium"
               "Current toolset lacks debugging/tracing capabilities for diagnosing failures."
               "4-6 hours")
              improvements))
      ;; Codebase complexity or size driven refactoring
      (let* ((total-lines (gethash "total_lines" codebase))
             (total-functions (gethash "total_functions" codebase))
             (complexity (and codebase (gethash "cyclomatic_complexity" codebase)))
             (avg-complexity (and complexity (gethash "average" complexity))))
        (when (or (and total-lines (> total-lines 3000))
                  (and total-functions (> total-functions 80))
                  (and avg-complexity (> avg-complexity 3.0)))
          (push (%improvement-plan-make-entry
                 "Refactor large modules and hotspots"
                 (format nil "Codebase has ~a lines and ~a functions with avg complexity ~,2f; refactor hotspots for maintainability."
                         (or total-lines 0)
                         (or total-functions 0)
                         (or avg-complexity 0.0))
                 "refactoring" "medium" "medium" "medium" "medium"
                 "Targeted refactors keep complexity low and reduce future change risk."
                 "6-10 hours")
                improvements)))
      ;; Tooling gaps: visualization/graphing
      (when (not (%improvement-plan-toolset-has-keyword-p
                  tools '("graph" "visual" "diagram" "chart")))
        (push (%improvement-plan-make-entry
               "Add visualization tools for codebase graphs"
               "Provide graph/diagram views for module and call relationships."
               "features" "low" "low" "medium" "long"
               "Visualization accelerates architectural understanding and review workflows."
               "8-12 hours")
              improvements))
      (setf improvements (%improvement-plan-sort improvements))
      (setf improvements (%improvement-plan-assign-ids improvements))
      (%improvement-plan-store-suggestions improvements)
      (let ((result (make-hash-table :test 'equal)))
        (setf (gethash "plan_id" result)
              (format nil "plan-~a" timestamp))
        (setf (gethash "based_on_assessment" result)
              (format nil "self-assess-~a" timestamp))
        (setf (gethash "improvements" result) improvements)
        (setf (gethash "summary" result)
              (%improvement-plan-summary improvements))
        result))))

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
