;;;; config.lisp — Configuration management for Sibyl
;;;; Loads from environment variables and optional config files.

(in-package #:sibyl.config)

;;; ============================================================
;;; Configuration store
;;; ============================================================

(defvar *config* (make-hash-table :test 'equal)
  "Global configuration hash table.")

(defun config-value (key &optional default)
  "Retrieve a configuration value by KEY. Returns DEFAULT if not found.
   KEY can be a dotted string like \"llm.api-key\"."
  (multiple-value-bind (val found-p)
      (gethash key *config*)
    (if found-p val default)))

(defun (setf config-value) (value key)
  "Set a configuration value."
  (setf (gethash key *config*) value))

;;; ============================================================
;;; Environment variable loading
;;; ============================================================

(defparameter *env-mappings*
  '(("CLAUDE_CODE_OAUTH_TOKEN" . "llm.anthropic.api-key")
    ("ANTHROPIC_API_KEY"  . "llm.anthropic.api-key")
    ("OPENAI_API_KEY"     . "llm.openai.api-key")
    ("SIBYL_MODEL"        . "llm.model")
    ("SIBYL_MAX_TOKENS"   . "llm.max-tokens")
    ("SIBYL_TEMPERATURE"  . "llm.temperature")
    ("SIBYL_LOG_LEVEL"    . "log.level"))
  "Mapping from environment variable names to config keys.")

(defun load-env-config ()
  "Load configuration from environment variables."
  (dolist (mapping *env-mappings*)
    (let ((value (uiop:getenv (car mapping))))
      (when value
        (setf (config-value (cdr mapping)) value)))))

;;; ============================================================
;;; File-based configuration (simple S-expression format)
;;; ============================================================

(defun load-config-file (path)
  "Load configuration from an S-expression config file.
   File format: ((:key \"value\") (:key2 \"value2\") ...)"
  (when (uiop:file-exists-p path)
    (let ((entries (uiop:safe-read-file-form path)))
      (dolist (entry entries)
        (destructuring-bind (key value) entry
          (setf (config-value (string-downcase (string key))) value))))))

;;; ============================================================
;;; Default values
;;; ============================================================

(defun set-defaults ()
  "Set default configuration values."
  (let ((defaults '(("llm.model"         . "claude-sonnet-4-20250514")
                    ("llm.max-tokens"    . 4096)
                    ("llm.temperature"   . 0.0)
                    ("agent.max-steps"   . 50)
                    ("agent.name"        . "Sibyl")
                    ("log.level"         . "info"))))
    (dolist (pair defaults)
      (unless (config-value (car pair))
        (setf (config-value (car pair)) (cdr pair))))))

;;; ============================================================
;;; Public API
;;; ============================================================

(defun load-config (&key (config-file nil))
  "Load all configuration: defaults, then config file, then env vars.
   Later sources override earlier ones."
  (clrhash *config*)
  (set-defaults)
  (when config-file
    (load-config-file config-file))
  (load-env-config)
  *config*)

(defmacro with-config ((&key config-file) &body body)
  "Execute BODY with a fresh configuration context."
  `(let ((*config* (make-hash-table :test 'equal)))
     (load-config :config-file ,config-file)
     ,@body))
