;;;; asdf-protection.lisp — ASDF reload protection for in-memory modifications
;;;;
;;;; Prevents ASDF from reloading files that have been modified in-memory
;;;; via safe-redefine, until those changes are synced to disk.
;;;;
;;;; NOTE: Protection works for normal system loads (ql:quickload, asdf:load-system).
;;;; For forced reloads (:force t), ASDF's state tracking requires the operation
;;;; to complete, so we skip the actual compilation/loading but let ASDF continue.
;;;; This means :force t will still "process" protected files, but won't actually
;;;; recompile or reload them.

(in-package #:sibyl.system)

(defvar *modified-files* (make-hash-table :test 'equal)
  "Hash table of file paths that should not be reloaded by ASDF.
   Keys are absolute pathnames as strings.")

(defvar *modified-files-lock* (bt:make-recursive-lock "modified-files-lock")
  "Recursive lock protecting *modified-files*.
   Lock order: tool-registry (1st) < modified-files (2nd) < command-handlers (3rd)")

(defun protect-file (path)
  "Mark PATH as modified, preventing ASDF reload.
   
   PATH can be a pathname or string. The file must exist to be protected
   (we use TRUENAME to get the canonical path)."
  (bt:with-recursive-lock-held (*modified-files-lock*)
    (let ((truename (handler-case (truename path)
                      (file-error ()
                        ;; If file doesn't exist yet, use the path as-is
                        ;; This handles the case where we're protecting a file
                        ;; before it's created
                        (if (pathnamep path)
                            path
                            (pathname path))))))
      (setf (gethash (namestring truename) *modified-files*) t)
      truename)))

(defun unprotect-file (path)
  "Remove PATH from protection, allowing ASDF reload.
   
   PATH can be a pathname or string."
  (bt:with-recursive-lock-held (*modified-files-lock*)
    (let ((truename (handler-case (truename path)
                      (file-error ()
                        (if (pathnamep path)
                            path
                            (pathname path))))))
      (remhash (namestring truename) *modified-files*))))

(defun file-protected-p (path)
  "Check if PATH is protected from ASDF reload.
   
   PATH can be a pathname or string. Returns T if protected, NIL otherwise."
  (bt:with-recursive-lock-held (*modified-files-lock*)
    (let ((truename (handler-case (truename path)
                      (file-error ()
                        (if (pathnamep path)
                            path
                            (pathname path))))))
      (gethash (namestring truename) *modified-files*))))

(defun clear-all-protections ()
  "Remove all file protections."
  (bt:with-recursive-lock-held (*modified-files-lock*)
    (clrhash *modified-files*)))

(defmethod asdf:perform :around ((op asdf:compile-op) (c asdf:cl-source-file))
  "Skip compilation of protected files.
   
   For protected files, we print a message and return without compiling.
   This prevents in-memory modifications from being overwritten."
  (let ((source-file (asdf:component-pathname c)))
    (if (file-protected-p source-file)
        (format t "~&; Skipping protected file (compile): ~a~%" source-file)
        (call-next-method))))

(defmethod asdf:perform :around ((op asdf:load-op) (c asdf:cl-source-file))
  "Skip loading of protected files.
   
   For protected files, we print a message and return without loading.
   This prevents in-memory modifications from being overwritten."
  (let ((source-file (asdf:component-pathname c)))
    (if (file-protected-p source-file)
        (format t "~&; Skipping protected file (load): ~a~%" source-file)
        (call-next-method))))
