;;;; asdf-protection-test.lisp — Tests for ASDF reload protection

(in-package #:sibyl.tests)

(def-suite asdf-protection-tests
  :description "Tests for ASDF reload protection mechanism"
  :in sibyl-tests)

(in-suite asdf-protection-tests)

(test protect-file-basic
  "Test basic file protection mechanism."
  ;; Clear any existing protections
  (sibyl.system:clear-all-protections)
  
  ;; Initially, no files should be protected
  (is (null (sibyl.system:file-protected-p #p"/tmp/test.lisp")))
  
  ;; Protect a file
  (sibyl.system:protect-file #p"/tmp/test.lisp")
  (is (sibyl.system:file-protected-p #p"/tmp/test.lisp"))
  
  ;; Unprotect the file
  (sibyl.system:unprotect-file #p"/tmp/test.lisp")
  (is (null (sibyl.system:file-protected-p #p"/tmp/test.lisp"))))

(test protect-multiple-files
  "Test protecting multiple files."
  (sibyl.system:clear-all-protections)
  
  ;; Protect multiple files
  (sibyl.system:protect-file #p"/tmp/file1.lisp")
  (sibyl.system:protect-file #p"/tmp/file2.lisp")
  (sibyl.system:protect-file #p"/tmp/file3.lisp")
  
  ;; All should be protected
  (is (sibyl.system:file-protected-p #p"/tmp/file1.lisp"))
  (is (sibyl.system:file-protected-p #p"/tmp/file2.lisp"))
  (is (sibyl.system:file-protected-p #p"/tmp/file3.lisp"))
  
  ;; Unprotect one
  (sibyl.system:unprotect-file #p"/tmp/file2.lisp")
  (is (sibyl.system:file-protected-p #p"/tmp/file1.lisp"))
  (is (null (sibyl.system:file-protected-p #p"/tmp/file2.lisp")))
  (is (sibyl.system:file-protected-p #p"/tmp/file3.lisp")))

(test clear-all-protections
  "Test clearing all protections."
  (sibyl.system:clear-all-protections)
  
  ;; Protect several files
  (sibyl.system:protect-file #p"/tmp/file1.lisp")
  (sibyl.system:protect-file #p"/tmp/file2.lisp")
  (sibyl.system:protect-file #p"/tmp/file3.lisp")
  
  ;; Clear all
  (sibyl.system:clear-all-protections)
  
  ;; None should be protected
  (is (null (sibyl.system:file-protected-p #p"/tmp/file1.lisp")))
  (is (null (sibyl.system:file-protected-p #p"/tmp/file2.lisp")))
  (is (null (sibyl.system:file-protected-p #p"/tmp/file3.lisp"))))

(test protect-file-with-string-path
  "Test that string paths work correctly."
  (sibyl.system:clear-all-protections)
  
  ;; Create a temporary file to get a real truename
  (let ((temp-file (uiop:with-temporary-file (:pathname p :keep t)
                     p)))
    (unwind-protect
         (progn
           ;; Protect using string path
           (sibyl.system:protect-file (namestring temp-file))
           (is (sibyl.system:file-protected-p temp-file))
           
           ;; Unprotect using pathname
           (sibyl.system:unprotect-file temp-file)
           (is (null (sibyl.system:file-protected-p temp-file))))
      ;; Cleanup
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(test modified-files-hash-table
  "Test that *modified-files* is a hash table."
  (is (hash-table-p sibyl.system:*modified-files*))
  (is (eq 'equal (hash-table-test sibyl.system:*modified-files*))))
