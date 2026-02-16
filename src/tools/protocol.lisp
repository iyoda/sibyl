;;;; protocol.lisp — Tool definition, registry, and execution
;;;; The heart of Sibyl's Lisp-idiomatic tool system.
;;;; Tools are defined with the DEFTOOL macro, registered globally,
;;;; and dispatched via the condition system for error recovery.

(in-package #:sibyl.tools)

;;; ============================================================
;;; Tool struct
;;; ============================================================

(defstruct tool
  "A tool that the agent can invoke."
  (name        "" :type string)
  (description "" :type string)
  (parameters  nil :type list)    ; list of parameter specs
  (handler     nil :type (or function null)))

;;; Parameter spec: (:name "param" :type "string" :required t :description "...")

;;; ============================================================
;;; Global registry
;;; ============================================================

(defvar *tool-registry* (make-hash-table :test 'equal)
  "Global registry mapping tool names to tool structs.")

(defun register-tool (tool)
  "Register a TOOL in the global registry. Overwrites if exists."
  (setf (gethash (tool-name tool) *tool-registry*) tool))

(defun find-tool (name)
  "Find a tool by NAME. Returns the tool struct or NIL."
  (gethash name *tool-registry*))

(defun list-tools ()
  "Return a list of all registered tool structs."
  (let ((tools nil))
    (maphash (lambda (k v) (declare (ignore k)) (push v tools))
             *tool-registry*)
    (sort tools #'string< :key #'tool-name)))

(defun unregister-tool (name)
  "Remove a tool from the registry."
  (remhash name *tool-registry*))

;;; ============================================================
;;; DEFTOOL macro — Lisp-idiomatic tool definition
;;; ============================================================

(defmacro deftool (name (&key description parameters) &body handler-body)
  "Define and register a tool.

   Usage:
     (deftool \"read-file\"
       (:description \"Read contents of a file\"
        :parameters ((:name \"path\" :type \"string\" :required t
                      :description \"Absolute file path\")))
       (let ((path (getf args :path)))
         (uiop:read-file-string path)))

   Within HANDLER-BODY, the variable ARGS is bound to a plist of
   the tool's arguments (keyword keys, e.g. :path, :content).
   TOOL-NAME is bound to the tool's name string."
  (let ((tool-var (gensym "TOOL"))
        (args-var (intern "ARGS" *package*))
        (tname-var (intern "TOOL-NAME" *package*)))
    `(let ((,tool-var
             (make-tool
              :name ,name
              :description ,description
              :parameters ',parameters
              :handler (lambda (,args-var)
                         (declare (ignorable ,args-var))
                         (let ((,tname-var ,name))
                           (declare (ignorable ,tname-var))
                           ,@handler-body)))))
       (register-tool ,tool-var)
       ,tool-var)))

;;; ============================================================
;;; Tool schema generation (for LLM API)
;;; ============================================================

(defun parameter-to-json-schema (param-spec)
  "Convert a parameter spec plist to JSON Schema property."
  (let ((name (getf param-spec :name))
        (type-str (or (getf param-spec :type) "string"))
        (desc (or (getf param-spec :description) "")))
    (cons name
          `(("type" . ,type-str)
            ("description" . ,desc)))))

(defun tool-to-schema (tool)
  "Convert a tool struct to an LLM-compatible schema plist."
  (let* ((params (tool-parameters tool))
         (properties (mapcar #'parameter-to-json-schema params))
         (required (mapcar (lambda (p) (getf p :name))
                           (remove-if-not (lambda (p) (getf p :required))
                                          params))))
    (list :name (tool-name tool)
          :description (tool-description tool)
          :parameters `(("type" . "object")
                        ("properties" . ,properties)
                        ("required" . ,required)))))

(defun tools-as-schema ()
  "Return all registered tools as a list of schema plists."
  (mapcar #'tool-to-schema (list-tools)))

;;; ============================================================
;;; Tool execution with condition-based error handling
;;; ============================================================

(defun validate-tool-args (tool args)
  "Validate ARGS against TOOL's parameter specifications.
   Signals TOOL-VALIDATION-ERROR on failure."
  (dolist (param (tool-parameters tool))
    (let ((name (getf param :name))
          (required-p (getf param :required)))
      (when (and required-p
                 (not (getf args (intern (string-upcase name) :keyword))))
        (error 'tool-validation-error
               :tool-name (tool-name tool)
               :parameter name
               :message (format nil "Required parameter ~a missing" name))))))

(defun normalize-args (args)
  "Normalize tool arguments: ensure keys are keywords.
   Accepts alist ((\"key\" . val) ...) or plist (:key val ...)."
  (cond
    ;; Already a plist with keywords
    ((and args (keywordp (first args)))
     args)
    ;; Alist with string keys
    ((and args (consp (first args)))
     (loop for (k . v) in args
           collect (intern (string-upcase k) :keyword)
           collect v))
    (t args)))

(defun execute-tool (name args)
  "Execute tool NAME with ARGS. Handles errors via the condition system.
   Provides restarts: RETRY-TOOL, SKIP-TOOL, USE-VALUE."
  (let ((tool (find-tool name)))
    (unless tool
      (error 'tool-not-found-error
             :tool-name name
             :message (format nil "No tool registered with name ~a" name)))
    (let ((normalized-args (normalize-args args)))
      (validate-tool-args tool normalized-args)
      (restart-case
          (handler-case
              (let ((result (funcall (tool-handler tool) normalized-args)))
                (if (stringp result)
                    result
                    (with-output-to-string (s)
                      (yason:encode result s))))
            (error (e)
              (error 'tool-execution-error
                     :tool-name name
                     :message (format nil "~a" e)
                     :inner-error e)))
        (retry-tool ()
          :report "Retry the tool execution"
          (execute-tool name args))
        (skip-tool ()
          :report "Skip this tool and return an error message"
          (format nil "Tool ~a was skipped due to an error" name))
        (use-value (value)
          :report "Use a specific value instead of tool result"
          :interactive (lambda ()
                         (format *query-io* "Value to use: ")
                         (list (read-line *query-io*)))
          value)))))

(defun execute-tool-call (tool-call-struct)
  "Execute a tool-call struct (from LLM response).
   Returns a string result."
  (execute-tool (sibyl.llm:tool-call-name tool-call-struct)
                (sibyl.llm:tool-call-arguments tool-call-struct)))
