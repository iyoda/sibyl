;;;; config.lisp — Cache configuration
;;;; Dynamic variables with sensible defaults; overridable via sibyl.config.

(in-package #:sibyl.cache)

(defvar *cache-enabled* t
  "Enable/disable the response cache globally.")

(defvar *cache-max-entries* 512
  "Maximum number of entries in the LRU cache.")

(defvar *cache-ttl-seconds* 86400
  "Default TTL in seconds (24 hours).")
