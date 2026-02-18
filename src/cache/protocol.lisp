;;;; protocol.lisp — Generic cache store protocol
;;;; All backends must implement these generic functions.

(in-package #:sibyl.cache)

(defgeneric cache-get (store key)
  (:documentation
   "Look up KEY in STORE.
Returns (values entry found-p) where ENTRY is a plist:
  (:response <string> :created-at <universal-time> :ttl <seconds> :hit-count <integer>)
or NIL if not found / expired."))

(defgeneric cache-put (store key response &key ttl)
  (:documentation
   "Store RESPONSE (string) under KEY in STORE.
TTL is in seconds (default: *cache-ttl-seconds*)."))

(defgeneric cache-evict-expired (store)
  (:documentation
   "Remove all TTL-expired entries from STORE.
Returns the number of entries evicted."))

(defgeneric cache-flush (store)
  (:documentation "Remove ALL entries from STORE."))

(defgeneric cache-stats (store)
  (:documentation
   "Return a plist of store-level statistics:
  (:size <integer> :max-size <integer> :oldest <universal-time-or-nil>)"))
