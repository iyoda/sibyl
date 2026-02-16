;;;; builtin.lisp — Built-in tools for file operations, shell, and search
;;;; These are the core tools a coding agent needs.

(in-package #:sibyl.tools)

;;; ============================================================
;;; File operations
;;; ============================================================

(deftool "read-file"
    (:description "Read the contents of a file at the given path."
     :parameters ((:name "path" :type "string" :required t
                   :description "Absolute or relative file path to read")))
  (let ((path (getf args :path)))
    (unless (uiop:file-exists-p path)
      (error "File not found: ~a" path))
    (uiop:read-file-string path)))

(deftool "write-file"
    (:description "Write content to a file, creating or overwriting it."
     :parameters ((:name "path" :type "string" :required t
                   :description "File path to write to")
                  (:name "content" :type "string" :required t
                   :description "Content to write")))
  (let ((path (getf args :path))
        (content (getf args :content)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
      (write-string content stream))
    (format nil "Written ~a bytes to ~a" (length content) path)))

(deftool "list-directory"
    (:description "List files and subdirectories in a directory."
     :parameters ((:name "path" :type "string" :required t
                   :description "Directory path to list")))
  (let* ((path (getf args :path))
         (dir (uiop:directory-files path))
         (subdirs (uiop:subdirectories path))
         (entries (append
                   (mapcar (lambda (d)
                             (format nil "~a/" (car (last (pathname-directory d)))))
                           subdirs)
                   (mapcar #'file-namestring dir))))
    (string-join (string #\Newline) (sort entries #'string<))))

;;; ============================================================
;;; Shell execution
;;; ============================================================

(deftool "shell"
    (:description "Execute a shell command and return its output."
     :parameters ((:name "command" :type "string" :required t
                   :description "Shell command to execute")
                  (:name "timeout" :type "integer" :required nil
                   :description "Timeout in seconds (default: 30)")))
  (let* ((command (getf args :command))
         ;; timeout arg is available but not yet enforced in this skeleton
         (result (multiple-value-list
                  (uiop:run-program command
                                    :output :string
                                    :error-output :string
                                    :ignore-error-status t))))
    (destructuring-bind (stdout stderr exit-code) result
      (format nil "Exit code: ~a~%~a~@[~%STDERR: ~a~]"
              exit-code stdout
              (when (and stderr (string/= stderr ""))
                stderr)))))

;;; ============================================================
;;; Search
;;; ============================================================

(deftool "grep"
    (:description "Search for a pattern in files using regular expressions."
     :parameters ((:name "pattern" :type "string" :required t
                   :description "Regular expression pattern to search for")
                  (:name "path" :type "string" :required t
                   :description "Directory or file path to search in")
                  (:name "include" :type "string" :required nil
                   :description "File glob pattern to include (e.g. \"*.lisp\")")))
  (let* ((pattern (getf args :pattern))
         (path (getf args :path))
         (include (getf args :include))
         (cmd (format nil "grep -rn ~a~@[ --include=~a~] ~s ~s"
                      (if include "" "") include
                      pattern path)))
    (uiop:run-program cmd
                      :output :string
                      :error-output nil
                      :ignore-error-status t)))

;;; ============================================================
;;; Code intelligence
;;; ============================================================

(deftool "file-info"
    (:description "Get metadata about a file: size, type, modification time."
     :parameters ((:name "path" :type "string" :required t
                   :description "Path to the file")))
  (let ((path (getf args :path)))
    (unless (uiop:file-exists-p path)
      (error "File not found: ~a" path))
    (let ((stat (uiop:safe-file-write-date path)))
      (format nil "Path: ~a~%Size: ~a bytes~%Modified: ~a"
              path
              (with-open-file (s path) (file-length s))
              (or stat "unknown")))))
