;;;; self-tools.lisp — Self-modification tools: add-export, create-module,
;;;;                   register-in-asdf, register-command

(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package #:sibyl.tools))

;;; ============================================================
;;; Package export management
;;; ============================================================

(defun %add-export-parse-symbols (symbols-input)
  "Parse symbols input - can be a single symbol or comma-separated list."
  (if (stringp symbols-input)
      (let ((trimmed (string-trim-whitespace symbols-input)))
        (if (position #\, trimmed)
            ;; Multiple symbols separated by commas
            (mapcar #'string-trim-whitespace
                    (cl-ppcre:split "," trimmed))
            ;; Single symbol
            (list trimmed)))
      (list (format nil "~a" symbols-input))))

(defun %add-export-find-defpackage-bounds (content package-name)
  "Find the start and end positions of a defpackage form in content.
   Returns (values start-pos end-pos) or (values nil nil) if not found."
  (let* ((search-string (format nil "(defpackage #:~a" 
                                (string-downcase package-name)))
         (start-pos (search search-string content :test #'char-equal)))
    (if start-pos
        (let ((end-pos (position #\) content :start start-pos :from-end nil)))
          ;; Find the matching closing paren
          (let ((depth 0)
                (pos start-pos))
            (loop while (< pos (length content))
                  do (let ((ch (char content pos)))
                       (cond
                         ((char= ch #\() (incf depth))
                         ((char= ch #\)) (decf depth)))
                       (when (zerop depth)
                         (return-from %add-export-find-defpackage-bounds
                           (values start-pos (1+ pos))))
                       (incf pos)))
            (values nil nil)))
        (values nil nil))))

(defun %add-export-find-export-section (defpackage-text)
  "Find the :export section within a defpackage form.
   Returns (values start-offset end-offset) relative to defpackage-text."
  (let ((export-pos (search "(:export" defpackage-text :test #'char-equal)))
    (if export-pos
        (let ((depth 0)
              (pos export-pos))
          (loop while (< pos (length defpackage-text))
                do (let ((ch (char defpackage-text pos)))
                     (cond
                       ((char= ch #\() (incf depth))
                       ((char= ch #\)) (decf depth)))
                     (when (zerop depth)
                       (return-from %add-export-find-export-section
                         (values export-pos (1+ pos))))
                     (incf pos)))
          (values nil nil))
        (values nil nil))))

(defun %add-export-insert-symbols (export-section symbols)
  "Insert new symbols into an :export section, preserving formatting.
   Returns the updated export section text."
  (let* ((lines (cl-ppcre:split "\\n" export-section))
         (last-line-idx (1- (length lines)))
         (last-line (nth last-line-idx lines)))
    ;; Find indentation from existing symbols
    (let ((indent "    "))
      (dolist (line lines)
        (when (search "#:" line)
          (let ((hash-pos (search "#:" line)))
            (when hash-pos
              (setf indent (make-string hash-pos :initial-element #\Space))
              (return)))))
      ;; Insert symbols before the closing paren
      (let ((new-symbols (mapcar (lambda (sym)
                                   (format nil "~a#:~a"
                                           indent
                                           (string-downcase sym)))
                                 symbols)))
        ;; Reconstruct the export section
        (with-output-to-string (s)
          (loop for line in (butlast lines)
                do (write-line line s))
          (dolist (new-sym new-symbols)
            (write-line new-sym s))
          (write-string last-line s))))))

(defun %add-export-update-packages-file (packages-path package-name symbols)
  "Update packages.lisp to add symbols to the package's :export section."
  (let* ((content (uiop:read-file-string packages-path)))
    (multiple-value-bind (defpkg-start defpkg-end)
        (%add-export-find-defpackage-bounds content package-name)
      (unless defpkg-start
        (error "Package ~a not found in ~a" package-name packages-path))
      (let ((defpackage-text (subseq content defpkg-start defpkg-end)))
        (multiple-value-bind (export-start export-end)
            (%add-export-find-export-section defpackage-text)
          (unless export-start
            (error "No :export section found in package ~a" package-name))
          (let* ((export-section (subseq defpackage-text export-start export-end))
                 (updated-export (%add-export-insert-symbols export-section symbols))
                 (updated-defpackage (concatenate 'string
                                                   (subseq defpackage-text 0 export-start)
                                                   updated-export
                                                   (subseq defpackage-text export-end)))
                 (updated-content (concatenate 'string
                                               (subseq content 0 defpkg-start)
                                               updated-defpackage
                                               (subseq content defpkg-end))))
            (with-open-file (stream packages-path
                                    :direction :output
                                    :if-exists :supersede)
              (write-string updated-content stream))
            updated-content))))))

(defun %add-export-runtime-export (package-name symbols)
  "Export symbols at runtime using CL's export function."
  (let ((pkg (find-package package-name)))
    (unless pkg
      (error "Package not found: ~a" package-name))
    (dolist (sym-name symbols)
      (let ((sym (intern (string-upcase sym-name) pkg)))
        ;; Check if already exported
        (multiple-value-bind (found-sym status)
            (find-symbol (string sym) pkg)
          (declare (ignore found-sym))
          (unless (eq status :external)
            (export sym pkg)))))))

(deftool "add-export"
    (:description "Add new symbols to a package's export list in packages.lisp and export them at runtime."
     :parameters ((:name "package" :type "string" :required t
                   :description "Package name (e.g., \"sibyl.tools\")")
                  (:name "symbols" :type "string" :required t
                   :description "Symbol name or comma-separated list of symbols to export")))
  (block add-export
    (let* ((package-input (getf args :package))
           (symbols-input (getf args :symbols))
           (packages-path (asdf:system-relative-pathname :sibyl "src/packages.lisp")))
      
      ;; Validate inputs
      (unless (and package-input (stringp package-input) (string/= package-input ""))
        (error "Package name not specified"))
      (unless (and symbols-input (stringp symbols-input) (string/= symbols-input ""))
        (error "Symbols not specified"))
      
      ;; Parse package name
      (let* ((package-name-input (string-trim-whitespace package-input))
             (package-name (string-upcase package-name-input))
             (symbols (%add-export-parse-symbols symbols-input))
             (original-content (uiop:read-file-string packages-path)))
        
        ;; Verify package exists
        (unless (find-package package-name)
          (error "Package not found: ~a" package-name-input))
        
        ;; Verify it's a sibyl.* package
        (unless (or (string-equal package-name "sibyl")
                    (cl-ppcre:scan "^sibyl\\." (string-downcase package-name)))
          (error "Can only add exports to sibyl.* packages, got: ~a" package-name))
        
        (restart-case
            (progn
              ;; Update packages.lisp
              (%add-export-update-packages-file packages-path package-name symbols)
              
              ;; Export at runtime
              (%add-export-runtime-export package-name symbols)
              
              (format nil "Success: Added ~{~a~^, ~} to ~a exports"
                      symbols package-name))
          (undo-export ()
            :report "Restore original packages.lisp content"
            (with-open-file (stream packages-path
                                    :direction :output
                                    :if-exists :supersede)
              (write-string original-content stream))
            (format nil "Export undone: packages.lisp restored")))))))

;;; ============================================================
;;; create-module — Create new Lisp source files
;;; ============================================================

(defun %create-module-sibyl-package-p (package-name)
  "Return T if PACKAGE-NAME is a valid Sibyl package name (sibyl or sibyl.*)."
  (%add-definition-sibyl-package-name-p package-name))

(defun %create-module-validate-path (path)
  "Signal an error if PATH does not start with src/."
  (unless (and (stringp path)
               (>= (length path) 4)
               (string= (subseq path 0 4) "src/"))
    (error "Path must be within src/ directory: ~a" path)))

(defun %create-module-split-definitions (defs-string)
  "Return a list of individual S-expression strings parsed from DEFS-STRING."
  (when (and defs-string (stringp defs-string))
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) defs-string)))
      (when (string/= trimmed "")
        (let ((results nil)
              (pos 0)
              (len (length defs-string))
              (*read-eval* nil))
          (loop
            ;; Skip whitespace
            (loop while (and (< pos len)
                             (member (char defs-string pos)
                                     '(#\Space #\Tab #\Newline #\Return)))
                  do (incf pos))
            (when (>= pos len) (return))
            (let ((start pos))
              (multiple-value-bind (form new-pos)
                  (handler-case
                      (read-from-string defs-string t nil :start pos)
                    (error (e)
                      (error "Invalid S-expression in initial-definitions: ~a" e)))
                (declare (ignore form))
                (push (subseq defs-string start new-pos) results)
                (setf pos new-pos))))
          (nreverse results))))))

(defun %create-module-generate-content (package-name header-comment def-strings)
  "Generate Lisp source file content with PACKAGE-NAME, optional HEADER-COMMENT, and DEF-STRINGS."
  (with-output-to-string (stream)
    (when (and header-comment
               (stringp header-comment)
               (string/= (string-trim '(#\Space #\Tab) header-comment) ""))
      (format stream ";;;; ~a~%~%" header-comment))
    (format stream "(in-package #:~a)~%" (string-downcase package-name))
    (when def-strings
      (dolist (def def-strings)
        (when (and def (string/= (string-trim '(#\Space #\Tab #\Newline #\Return) def) ""))
          (format stream "~%~a~%" def))))))

(defun %create-module-eval-definitions (def-strings package-name)
  "Compile and evaluate each definition string in DEF-STRINGS within PACKAGE-NAME."
  (dolist (def def-strings)
    (when (and def (stringp def)
               (string/= (string-trim '(#\Space #\Tab #\Newline #\Return) def) ""))
      (let* ((result (execute-tool "eval-form"
                                   (list (cons "form" def)
                                         (cons "package" package-name))))
             (lower (string-downcase result)))
        (when (or (search "blocked unsafe form" lower)
                  (search "timeout after" lower))
          (error 'sibyl.conditions:tool-execution-error
                 :tool-name "create-module"
                 :message (format nil "Failed to evaluate definition: ~a" result)
                 :inner-error nil))))))

(deftool "create-module"
    (:description "Create a new Lisp source file within src/ with a package declaration and optional initial definitions."
     :parameters ((:name "path" :type "string" :required t
                   :description "File path within src/ directory, e.g. \"src/tools/new-tool.lisp\"")
                  (:name "package" :type "string" :required t
                   :description "Package name in sibyl.* namespace, e.g. \"sibyl.tools\"")
                  (:name "header-comment" :type "string" :required nil
                   :description "Optional comment for file header")
                  (:name "initial-definitions" :type "string" :required nil
                   :description "Optional S-expressions to include as initial definitions")))
  (block create-module
    (let* ((path (getf args :path))
           (package (getf args :package))
           (header-comment (getf args :header-comment))
           (initial-defs-raw (getf args :initial-definitions)))

      ;; Validate path
      (unless (and path (stringp path) (string/= path ""))
        (error "File path not specified"))
      (%create-module-validate-path path)

      ;; Validate package
      (unless (and package (stringp package) (string/= package ""))
        (error "Package name not specified"))
      (let ((package-name (string-upcase package)))
        (unless (%create-module-sibyl-package-p package-name)
          (error "Package must be in sibyl.* namespace: ~a" package))

        ;; Resolve path relative to ASDF system root
        (let ((full-path (asdf:system-relative-pathname :sibyl path)))

          ;; Check file does not already exist
          (when (uiop:file-exists-p full-path)
            (error "File already exists: ~a" path))

          ;; Parse initial definitions
          (let* ((def-strings (when initial-defs-raw
                                (%create-module-split-definitions initial-defs-raw)))
                 (content (%create-module-generate-content
                            package-name header-comment def-strings)))

            (restart-case
                (progn
                  ;; Write file
                  (ensure-directories-exist full-path)
                  (%sync-to-file-write-content full-path content)
                  ;; Compile initial definitions
                  (when def-strings
                    (%create-module-eval-definitions def-strings package-name))
                  (format nil "Success: created ~a" path))
              (remove-module ()
                :report "Delete the created module file"
                (when (probe-file full-path)
                  (delete-file full-path))
                (format nil "Module deleted: ~a" path)))))))))

;;; ============================================================
;;; register-in-asdf — Register new files in sibyl.asd
;;; ============================================================

(defun %register-in-asdf-find-form-end (text start-pos)
  "Return end position (exclusive) of form starting at START-POS, or NIL."
  (when (and start-pos (< start-pos (length text))
             (char= (char text start-pos) #\())
    (let ((depth 0)
          (pos start-pos)
          (len (length text)))
      (loop while (< pos len)
            do (let ((ch (char text pos)))
                 (cond
                   ((char= ch #\() (incf depth))
                   ((char= ch #\)) (decf depth)))
                 (when (zerop depth)
                   (return-from %register-in-asdf-find-form-end (1+ pos)))
                 (incf pos)))
      nil)))

(defun %register-in-asdf-find-module-bounds (content module-name)
  "Find the start and end positions of MODULE-NAME in CONTENT.
   Returns (values start-pos end-pos) or (values nil nil)."
  (let* ((search-string (format nil "(:module \"~a\"" module-name))
         (start-pos (search search-string content :test #'char-equal)))
    (if start-pos
        (let ((end-pos (%register-in-asdf-find-form-end content start-pos)))
          (values start-pos end-pos))
        (values nil nil))))

(defun %register-in-asdf-find-components-bounds (module-text)
  "Find the start and end positions of the :components list in MODULE-TEXT.
   Returns (values start-pos end-pos) or (values nil nil)."
  (let ((components-pos (search ":components" module-text :test #'char-equal)))
    (when components-pos
      (let ((list-start (position #\( module-text :start components-pos)))
        (when list-start
          (values list-start
                  (%register-in-asdf-find-form-end module-text list-start)))))))

(defun %register-in-asdf-parse-entry (entry-text)
  "Parse ENTRY-TEXT and return (values type name) if possible."
  (handler-case
      (let ((*read-eval* nil))
        (multiple-value-bind (form pos)
            (read-from-string entry-text)
          (declare (ignore pos))
          (when (and (consp form)
                     (keywordp (first form))
                     (stringp (second form)))
            (values (string-downcase (symbol-name (first form)))
                    (second form)))))
    (error () (values nil nil))))

(defun %register-in-asdf-collect-component-entries (components-text)
  "Return a list of top-level component entries in COMPONENTS-TEXT.
   Each entry is a plist with :start, :end, :type, and :name."
  (let ((entries nil)
        (pos 0)
        (len (length components-text))
        (depth 0))
    (loop while (< pos len)
          do (let ((ch (char components-text pos)))
               (cond
                 ((char= ch #\()
                  (incf depth)
                  (when (= depth 2)
                    (let ((entry-start pos)
                          (entry-end (%register-in-asdf-find-form-end components-text pos)))
                      (unless entry-end
                        (error "Malformed :components list"))
                      (multiple-value-bind (entry-type entry-name)
                          (%register-in-asdf-parse-entry
                           (subseq components-text entry-start entry-end))
                        (push (list :start entry-start
                                    :end entry-end
                                    :type entry-type
                                    :name entry-name)
                              entries))
                      (setf pos (1- entry-end))
                      (setf depth 1))))
                 ((char= ch #\))
                  (decf depth))))
             (incf pos))
    (nreverse entries)))

(defun %register-in-asdf-detect-indent (components-text entries)
  "Detect indentation for new entries within COMPONENTS-TEXT."
  (let ((entry (find-if (lambda (item) (getf item :start)) entries)))
    (if entry
        (let* ((pos (getf entry :start))
               (line-start (position #\Newline components-text :end pos :from-end t))
               (start (if line-start (1+ line-start) 0))
               (width (max 0 (- pos start))))
          (make-string width :initial-element #\Space))
        "  ")))

(defun %register-in-asdf-insert-entry (components-text file-name &optional after-name)
  "Insert FILE-NAME into COMPONENTS-TEXT after AFTER-NAME (if provided)."
  (let* ((entries (%register-in-asdf-collect-component-entries components-text))
         (entry-text (format nil "(:file \"~a\")" file-name))
         (duplicate (find-if (lambda (item)
                               (and (string-equal (getf item :type) "file")
                                    (getf item :name)
                                    (string-equal (getf item :name) file-name)))
                             entries)))
    (when duplicate
      (error "File already registered: ~a" file-name))
    (let* ((indent (%register-in-asdf-detect-indent components-text entries))
           (after-entry (when after-name
                          (find-if (lambda (item)
                                     (and (getf item :name)
                                          (string-equal (getf item :name) after-name)))
                                   entries)))
           (insert-pos (if after-name
                           (if after-entry
                               (getf after-entry :end)
                               (error "Component not found: ~a" after-name))
                           (1- (length components-text)))))
      (concatenate 'string
                   (subseq components-text 0 insert-pos)
                   (format nil "~%~a~a" indent entry-text)
                   (subseq components-text insert-pos)))))

(defun %register-in-asdf-update-content (content module-name file-name after-name)
  "Return updated CONTENT with FILE-NAME added to MODULE-NAME components."
  (multiple-value-bind (module-start module-end)
      (%register-in-asdf-find-module-bounds content module-name)
    (unless module-start
      (error "Module not found: ~a" module-name))
    (unless module-end
      (error "Malformed module definition: ~a" module-name))
    (let ((module-text (subseq content module-start module-end)))
      (multiple-value-bind (components-start components-end)
          (%register-in-asdf-find-components-bounds module-text)
        (unless components-start
          (error "No :components list found for module: ~a" module-name))
        (unless components-end
          (error "Malformed :components list for module: ~a" module-name))
        (let* ((components-text (subseq module-text components-start components-end))
               (updated-components (%register-in-asdf-insert-entry
                                    components-text file-name after-name))
               (updated-module (concatenate 'string
                                            (subseq module-text 0 components-start)
                                            updated-components
                                            (subseq module-text components-end)))
               (updated-content (concatenate 'string
                                             (subseq content 0 module-start)
                                             updated-module
                                             (subseq content module-end))))
          updated-content)))))

(deftool "register-in-asdf"
    (:description "Register a new file component in sibyl.asd under a module."
     :parameters ((:name "file" :type "string" :required t
                   :description "File name without extension (e.g., new-tool).")
                  (:name "module" :type "string" :required t
                   :description "Module name (e.g., tools or agent).")
                  (:name "after" :type "string" :required nil
                   :description "Optional component name to insert after.")))
  (block register-in-asdf
    (let* ((file-input (getf args :file))
           (module-input (getf args :module))
           (after-input (getf args :after))
           (asdf-path (asdf:system-relative-pathname :sibyl "sibyl.asd")))
      ;; Validate inputs
      (unless (and file-input (stringp file-input)
                   (string/= (string-trim-whitespace file-input) ""))
        (error "File name not specified"))
      (unless (and module-input (stringp module-input)
                   (string/= (string-trim-whitespace module-input) ""))
        (error "Module name not specified"))
      (let* ((file-name (string-downcase (string-trim-whitespace file-input)))
             (module-name (string-downcase (string-trim-whitespace module-input)))
             (after-name (when (and after-input (stringp after-input)
                                    (string/= (string-trim-whitespace after-input) ""))
                           (string-downcase (string-trim-whitespace after-input))))
             (original-content (uiop:read-file-string asdf-path)))
        (restart-case
            (progn
              (let ((updated-content (%register-in-asdf-update-content
                                      original-content module-name file-name after-name)))
                (with-open-file (stream asdf-path
                                        :direction :output
                                        :if-exists :supersede)
                  (write-string updated-content stream))
                (asdf:clear-system :sibyl)
                (asdf:find-system :sibyl t)
                (let ((component (or (asdf:find-component :sibyl (list module-name file-name))
                                     (asdf:find-component :sibyl (list "src" module-name file-name)))))
                  (unless component
                    (error "ASDF registration failed for ~a/~a" module-name file-name)))
                (format nil "Success: registered ~a in module ~a" file-name module-name)))
          (undo-registration ()
            :report "Restore original sibyl.asd content"
            (with-open-file (stream asdf-path
                                    :direction :output
                                    :if-exists :supersede)
              (write-string original-content stream))
            (asdf:clear-system :sibyl)
            (asdf:find-system :sibyl t)
            (format nil "Registration undone: sibyl.asd restored")))))))

;;; ============================================================
;;; Dynamic REPL command registration
;;; ============================================================

(defun %register-command-validate-name (name)
  "Validate that NAME is a non-empty string. Signals tool-execution-error if invalid."
  (unless (and (stringp name) (string/= (string-trim-whitespace name) ""))
    (error 'sibyl.conditions:tool-execution-error
           :tool-name "register-command"
           :message "Command name must be a non-empty string"
           :inner-error nil)))

(defun %register-command-parse-handler (handler-body-string)
  "Parse and validate HANDLER-BODY-STRING as a lambda expression.
   Returns the compiled function, or signals tool-execution-error."
  (let ((form
          (handler-case
              (let ((*read-eval* nil))
                (read-from-string handler-body-string))
            (error (e)
              (error 'sibyl.conditions:tool-execution-error
                     :tool-name "register-command"
                     :message (format nil "Invalid handler-body S-expression: ~a" e)
                     :inner-error e)))))
    ;; Validate it's a lambda form
    (unless (and (consp form)
                 (symbolp (car form))
                 (string-equal (symbol-name (car form)) "LAMBDA"))
      (error 'sibyl.conditions:tool-execution-error
             :tool-name "register-command"
             :message (format nil "handler-body must be a lambda expression, got: ~a"
                               (if (consp form)
                                   (string-downcase (symbol-name (car form)))
                                   (type-of form)))
             :inner-error nil))
    ;; Evaluate the lambda to create a function
    (handler-case
        (let ((*read-eval* t))
          (eval form))
      (error (e)
        (error 'sibyl.conditions:tool-execution-error
               :tool-name "register-command"
               :message (format nil "Failed to evaluate handler-body: ~a" e)
               :inner-error e)))))

