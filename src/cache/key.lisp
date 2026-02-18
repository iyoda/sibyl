;;;; key.lisp — Deterministic cache key generation via SHA-256
;;;; Normalizes alists and hash-tables into a canonical string, then hashes.

(in-package #:sibyl.cache)

;;; ─────────────────────────────────────────────
;;;  Value normalization for canonical form
;;; ─────────────────────────────────────────────

(defun alist-like-p (v)
  "Return T if V looks like an alist with string keys (JSON object)."
  (and (consp v)
       (consp (car v))
       (stringp (caar v))))

(defun sort-alist-entries (alist)
  "Recursively sort an alist by key (string<) for canonical ordering."
  (let ((normalized (mapcar (lambda (pair)
                              (cons (car pair)
                                    (normalize-cache-value (cdr pair))))
                            alist)))
    (sort normalized #'string< :key #'car)))

(defun normalize-cache-value (v)
  "Recursively normalize a value for canonical form.
Handles hash-tables (from yason:parse), alists (from sibyl request
construction), arrays (CL lists), and scalars."
  (cond
    ((null v) :null)
    ((eq v t) t)
    ((eq v :null) :null)
    ((hash-table-p v)
     ;; yason-parsed JSON object → sort as alist
     (sort-alist-entries (alexandria:hash-table-alist v)))
    ((alist-like-p v)
     ;; sibyl-constructed alist (string-keyed)
     (sort-alist-entries v))
    ((listp v)
     ;; JSON array
     (mapcar #'normalize-cache-value v))
    (t v)))

;;; ─────────────────────────────────────────────
;;;  Canonical string + SHA-256
;;; ─────────────────────────────────────────────

(defun request-to-canonical-string (request)
  "Produce a deterministic string from a request alist or hash-table.
The output is a CL-readable S-expression of sorted alist entries."
  (let ((sorted (cond
                  ((hash-table-p request)
                   (sort-alist-entries (alexandria:hash-table-alist request)))
                  ((alist-like-p request)
                   (sort-alist-entries request))
                  (t
                   (error "Cannot canonicalize request of type ~a"
                          (type-of request))))))
    (with-output-to-string (s)
      (write sorted :stream s :readably t))))

(defun sha256-hex (string)
  "Return the SHA-256 hex digest of STRING (UTF-8 encoded)."
  (let* ((bytes (sb-ext:string-to-octets string :external-format :utf-8))
         (digest (ironclad:digest-sequence :sha256 bytes)))
    (ironclad:byte-array-to-hex-string digest)))

(defun make-cache-key (request)
  "Compute a stable SHA-256 cache key from a request (alist or hash-table).
Returns a 64-character lowercase hex string."
  (sha256-hex (request-to-canonical-string request)))
