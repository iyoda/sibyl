;;;; evolution-tools.lisp — Dynamic command registration

(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package #:sibyl.tools))

;;; ============================================================
;;; Dynamic command registration
;;; ============================================================

(defun %register-command-make-keyword (name)
  "Convert NAME string to a keyword symbol."
  (intern (string-upcase (string-trim-whitespace name)) :keyword))

(deftool "register-command"
 (:description "Dynamically register a new REPL command in *command-handlers*."
  :parameters
  ((:name "name" :type "string" :required t :description
    "Command name (e.g. \"custom\"). Used as keyword key in *command-handlers*.")
   (:name "description" :type "string" :required t :description
    "Help text describing what the command does.")
   (:name "handler-body" :type "string" :required t :description
    "Lambda expression as S-expression string: (lambda (agent input) ...)")))
 (block register-command
   (let* ((name (getf args :name))
          (description (getf args :description))
          (handler-body (getf args :handler-body)))
     (%register-command-validate-name name)
     (unless
         (and (stringp description)
              (string/= (string-trim-whitespace description) ""))
       (error 'sibyl.conditions:tool-execution-error :tool-name
              "register-command" :message
              "Command description must be a non-empty string" :inner-error
              nil))
     (let* ((handler-fn (%register-command-parse-handler handler-body))
            (keyword (%register-command-make-keyword name))
            (repl-package (find-package "SIBYL.REPL"))
            (handlers-sym
             (and repl-package
                  (find-symbol "*COMMAND-HANDLERS*" repl-package))))
       (unless (and handlers-sym (boundp handlers-sym))
         (error 'sibyl.conditions:tool-execution-error :tool-name
                "register-command" :message
                "sibyl.repl::*command-handlers* not found or unbound"
                :inner-error nil))
       (push (cons keyword
                   (list :handler handler-fn
                         :description description
                         :hidden nil))
             (symbol-value handlers-sym))
       (format nil "Success: command ~a registered (~a)"
               (string-downcase (symbol-name keyword)) description)))))
