;;;; util.lisp — General utilities for Sibyl

(in-package #:sibyl.util)

(defun string-join (separator strings)
  "Join a list of STRINGS with SEPARATOR."
  (format nil (concatenate 'string "~{~a~^" separator "~}") strings))

(defun string-trim-whitespace (string)
  "Trim leading and trailing whitespace from STRING."
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun alist-to-hash (alist &key (test 'equal))
  "Convert an association list to a hash table."
  (let ((ht (make-hash-table :test test)))
    (dolist (pair alist ht)
      (setf (gethash (car pair) ht) (cdr pair)))))

(defun hash-to-alist (hash-table)
  "Convert a hash table to an association list."
  (let ((result nil))
    (maphash (lambda (k v) (push (cons k v) result)) hash-table)
    (nreverse result)))

(defun getf* (plist key &optional default)
  "Like GETF but works with string keys via EQUAL comparison."
  (loop for (k v) on plist by #'cddr
        when (equal k key) return v
        finally (return default)))

(defmacro with-gensyms (syms &body body)
  "Bind each symbol in SYMS to a fresh gensym."
  `(let ,(mapcar (lambda (s)
                   `(,s (gensym ,(string s))))
                 syms)
     ,@body))

(defun timestamp-now ()
  "Return current timestamp as an ISO 8601 string."
  (local-time:format-timestring
   nil (local-time:now)
   :format '((:year 4) #\- (:month 2) #\- (:day 2)
             #\T (:hour 2) #\: (:min 2) #\: (:sec 2) #\Z)))

(defun truncate-string (string max-length &key (suffix "..."))
  "Truncate STRING to MAX-LENGTH, appending SUFFIX if truncated."
  (if (<= (length string) max-length)
      string
      (concatenate 'string
                   (subseq string 0 (- max-length (length suffix)))
                   suffix)))
