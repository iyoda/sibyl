;;;; lru.lisp — Thread-safe LRU cache with TTL support
;;;; Doubly-linked list + hash-table for O(1) get/put/evict.

(in-package #:sibyl.cache)

;;; ─────────────────────────────────────────────
;;;  Doubly-linked list node
;;; ─────────────────────────────────────────────

(defstruct lru-node
  key
  response
  (created-at (get-universal-time) :type integer)
  (ttl *cache-ttl-seconds* :type integer)
  (hit-count 0 :type integer)
  prev
  next)

;;; ─────────────────────────────────────────────
;;;  LRU cache class
;;; ─────────────────────────────────────────────

(defclass lru-cache ()
  ((table    :initform (make-hash-table :test #'equal)
             :accessor lru-table
             :documentation "key → lru-node")
   (head     :initform nil
             :accessor lru-head
             :documentation "Most-recently-used sentinel (dummy)")
   (tail     :initform nil
             :accessor lru-tail
             :documentation "Least-recently-used sentinel (dummy)")
   (size     :initform 0
             :accessor lru-size
             :type integer)
   (max-size :initarg :max-size
             :initform 512
             :accessor lru-max-size
             :type integer)
   (lock     :initform (bt:make-lock "lru-cache-lock")
             :accessor lru-lock)))

(defun make-lru-cache (&key (max-size *cache-max-entries*))
  "Create a new thread-safe LRU cache with MAX-SIZE capacity."
  (let ((cache (make-instance 'lru-cache :max-size max-size))
        (head  (make-lru-node :key :head))
        (tail  (make-lru-node :key :tail)))
    (setf (lru-node-next head) tail
          (lru-node-prev tail) head
          (lru-head cache) head
          (lru-tail cache) tail)
    cache))

;;; ─────────────────────────────────────────────
;;;  Internal doubly-linked list helpers
;;; ─────────────────────────────────────────────

(defun %remove-node (node)
  "Unlink NODE from the doubly-linked list."
  (let ((prev (lru-node-prev node))
        (next (lru-node-next node)))
    (setf (lru-node-next prev) next
          (lru-node-prev next) prev)))

(defun %insert-after-head (cache node)
  "Insert NODE right after the head sentinel (MRU position)."
  (let ((head (lru-head cache))
        (second (lru-node-next (lru-head cache))))
    (setf (lru-node-next head)   node
          (lru-node-prev node)   head
          (lru-node-next node)   second
          (lru-node-prev second) node)))

(defun %move-to-front (cache node)
  "Move NODE to the MRU position."
  (%remove-node node)
  (%insert-after-head cache node))

(defun %expired-p (node)
  "Return T if NODE's TTL has elapsed."
  (let ((age (- (get-universal-time) (lru-node-created-at node))))
    (>= age (lru-node-ttl node))))

;;; ─────────────────────────────────────────────
;;;  Protocol implementation
;;; ─────────────────────────────────────────────

(defmethod cache-get ((store lru-cache) key)
  (bt:with-lock-held ((lru-lock store))
    (let ((node (gethash key (lru-table store))))
      (cond
        ((null node)
         (values nil nil))
        ((%expired-p node)
         ;; Lazy eviction on access
         (%remove-node node)
         (remhash key (lru-table store))
         (decf (lru-size store))
         (values nil nil))
        (t
         (incf (lru-node-hit-count node))
         (%move-to-front store node)
         (values (list :response   (lru-node-response node)
                       :created-at (lru-node-created-at node)
                       :ttl        (lru-node-ttl node)
                       :hit-count  (lru-node-hit-count node))
                 t))))))

(defmethod cache-put ((store lru-cache) key response
                      &key (ttl *cache-ttl-seconds*))
  (bt:with-lock-held ((lru-lock store))
    (let ((existing (gethash key (lru-table store))))
      (if existing
          ;; Update in place, move to front
          (progn
            (setf (lru-node-response   existing) response
                  (lru-node-created-at existing) (get-universal-time)
                  (lru-node-ttl        existing) ttl)
            (%move-to-front store existing))
          ;; New entry
          (progn
            (when (>= (lru-size store) (lru-max-size store))
              ;; Evict LRU (node before tail)
              (let ((lru-node (lru-node-prev (lru-tail store))))
                (%remove-node lru-node)
                (remhash (lru-node-key lru-node) (lru-table store))
                (decf (lru-size store))))
            (let ((node (make-lru-node :key key :response response :ttl ttl)))
              (%insert-after-head store node)
              (setf (gethash key (lru-table store)) node)
              (incf (lru-size store))))))
    (values)))

(defmethod cache-evict-expired ((store lru-cache))
  "Walk the list from LRU end and remove expired nodes."
  (bt:with-lock-held ((lru-lock store))
    (let ((node (lru-node-prev (lru-tail store)))
          (evicted 0))
      (loop while (not (eq (lru-node-key node) :head))
            for prev = (lru-node-prev node)
            do (when (%expired-p node)
                 (%remove-node node)
                 (remhash (lru-node-key node) (lru-table store))
                 (decf (lru-size store))
                 (incf evicted))
               (setf node prev))
      evicted)))

(defmethod cache-flush ((store lru-cache))
  (bt:with-lock-held ((lru-lock store))
    (clrhash (lru-table store))
    ;; Re-link head ↔ tail
    (setf (lru-node-next (lru-head store)) (lru-tail store)
          (lru-node-prev (lru-tail store)) (lru-head store)
          (lru-size store) 0))
  (values))

(defmethod cache-stats ((store lru-cache))
  (bt:with-lock-held ((lru-lock store))
    (let ((oldest-node (lru-node-prev (lru-tail store))))
      (list :size     (lru-size store)
            :max-size (lru-max-size store)
            :oldest   (unless (eq (lru-node-key oldest-node) :head)
                        (lru-node-created-at oldest-node))))))
