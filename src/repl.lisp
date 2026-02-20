;;;; repl.lisp — Interactive REPL interface for Sibyl
;;;; Provides a user-facing loop for conversing with the agent.

(in-package #:sibyl.repl)

;;; ============================================================
;;; REPL commands
;;; ============================================================

(defparameter *repl-commands*
  '(("/quit"          . :quit)
    ("/exit"          . :quit)
    ("/reset"         . :reset)
    ("/new"           . :reset)
    ("/tools"         . :list-tools)
    ("/help"          . :help)
    ("/history"       . :history)
    ("/mcp"           . :mcp)
    ("/plan"          . :plan)
    ("/improve"       . :improve)
    ("/review"        . :review)
    ("/tokens"        . :tokens)
    ("/model"         . :model)
    ("/cost-report"   . :cost-report)
    ("/sessions"      . :sessions)
    ("/save"          . :save-session)
    ("/load"          . :load-session))
  "Mapping of REPL commands to actions.")

;;; ============================================================
;;; Suggestion state management
;;; ============================================================

(defvar *pending-suggestions* nil
  "List of pending improvement suggestions from suggest-improvements.
   Each suggestion is a plist with:
   - :id (integer)
   - :description (string) — what to improve
   - :rationale (string) — why it should be improved
   - :priority (string) — 'high', 'medium', or 'low'
   - :status (string) — 'pending', 'approved', 'rejected', 'modified'")

(defvar *next-suggestion-id* 1
  "Counter for generating unique suggestion IDs.")

(defvar *command-handlers-lock* (bt:make-recursive-lock "command-handlers-lock")
   "Recursive lock protecting *command-handlers* and *pending-suggestions*.
    Lock order: tool-registry (1st) < modified-files (2nd) < command-handlers (3rd)")

(defvar *use-colors* t
  "Whether to use colored output in the REPL. Defaults to t (colors enabled).")

(defvar *stream-enabled* t
  "When T, LLM responses are streamed token-by-token to the terminal.")

(defvar *ignore-ctrl-j* nil
  "When T, strip Ctrl+J (linefeed) characters from REPL input.")

(defvar *command-count* 0
  "Counter tracking the number of commands executed in the current REPL session.")

(defvar *current-session-id* nil
  "The session ID for the current REPL session.")

;;; ============================================================
;;; Output post-processing
;;; ============================================================

(defparameter *function-block-start* "<function"
  "Start tag prefix for tool-call style blocks emitted by some models.")

(defparameter *function-block-end* "</function>"
  "End tag for tool-call style blocks emitted by some models.")

(defun strip-function-blocks (text)
  "Strip <function ...>...</function> blocks from TEXT.
Returns TEXT unchanged when it is not a string."
  (if (not (stringp text))
      text
      (let ((pos 0)
            (out (make-string-output-stream)))
        (loop
          (let ((start (search *function-block-start* text :start2 pos)))
            (if start
                (progn
                  (write-string text out :start pos :end start)
                  (let ((end (search *function-block-end* text :start2 start)))
                    (if end
                        (setf pos (+ end (length *function-block-end*)))
                        (return))))
                (progn
                  (write-string text out :start pos)
                  (return)))))
        (get-output-stream-string out))))

(defun make-function-block-stripper ()
  "Return a stateful function that strips <function> blocks from streamed chunks."
  (let ((in-block nil)
        (carry ""))
    (lambda (chunk)
      (let* ((data (if (string= carry "")
                       chunk
                       (concatenate 'string carry chunk)))
             (pos 0)
             (out (make-string-output-stream)))
        (setf carry "")
        (loop
          (cond
            (in-block
             (let ((end (search *function-block-end* data :start2 pos)))
               (if end
                   (setf in-block nil
                         pos (+ end (length *function-block-end*)))
                   (progn
                     (setf carry (subseq data pos))
                     (return)))))
            (t
             (let ((start (search *function-block-start* data :start2 pos)))
               (if start
                   (progn
                     (write-string data out :start pos :end start)
                     (let ((end (search *function-block-end* data :start2 start)))
                       (if end
                           (setf pos (+ end (length *function-block-end*)))
                           (progn
                             (setf in-block t
                                   carry (subseq data start))
                             (return)))))
                   (progn
                     (write-string data out :start pos)
                     (return)))))))
        (get-output-stream-string out)))))

(defvar *command-history* nil
  "List of command strings entered in the current REPL session, in reverse chronological order.")

(defvar *cancel-requested* nil
  "When non-NIL, the current LLM call should be cancelled.
   Set to T by the Ctrl+C handler (single press); reset to NIL before each agent-run call.")

(defvar *current-spinner* nil
  "Currently active spinner for LLM calls, or NIL when idle.")


(defvar *thinking-output-active* nil
  "Set to T by the thinking display callback after writing chunks.
   Checked and reset by the text streaming callback to emit a newline
   separator between thinking output and response text.")
(defvar *spinner-output-lock* (bt:make-lock "spinner-output")
  "Lock serializing spinner stop/start and terminal output in the tool-call hook.
   Prevents garbled escape sequences when parallel tool calls each fire the hook
   from separate worker threads.")

(defun %spinner-stream-supported-p (&optional (stream *standard-output*))
  "Return T when STREAM is suitable for background spinner updates.
Spinner output assumes an interactive terminal stream; writing from a
background thread to non-interactive streams (e.g., string streams used by
tests) can trigger SBCL stream corruption warnings."
  (and (streamp stream)
       (ignore-errors (interactive-stream-p stream))))

(defvar *last-interrupt-time* 0
  "Internal real time (from GET-INTERNAL-REAL-TIME) of the most recent Ctrl+C press.
   Used to detect double-press within 2 seconds for REPL exit.")

(defun store-suggestion (description rationale priority)
  "Store a new improvement suggestion. Returns the created suggestion."
  (bt:with-recursive-lock-held (*command-handlers-lock*)
    (let ((suggestion (list :id *next-suggestion-id*
                            :description description
                            :rationale rationale
                            :priority priority
                            :status "pending")))
      (push suggestion *pending-suggestions*)
      (incf *next-suggestion-id*)
      suggestion)))

(defun store-suggestions (suggestions-list)
  "Store multiple suggestions from a list of plists.
   Each plist should have :description, :rationale, and :priority."
  (dolist (suggestion suggestions-list)
    (store-suggestion (getf suggestion :description)
                      (getf suggestion :rationale)
                      (getf suggestion :priority))))

(defun get-suggestion-by-id (id)
  "Retrieve a suggestion by its ID. Returns NIL if not found."
  (bt:with-recursive-lock-held (*command-handlers-lock*)
    (find id *pending-suggestions* :key (lambda (s) (getf s :id)))))

(defun update-suggestion-status (id new-status)
  "Update the status of a suggestion."
  (bt:with-recursive-lock-held (*command-handlers-lock*)
    (let ((suggestion (get-suggestion-by-id id)))
      (when suggestion
        (setf (getf suggestion :status) new-status)
        t))))

(defun clear-suggestions ()
  "Clear all pending suggestions."
  (bt:with-recursive-lock-held (*command-handlers-lock*)
    (setf *pending-suggestions* nil)
    (setf *next-suggestion-id* 1)))

(defun format-colored-text (text color &optional (stream *standard-output*))
  "Format text with ANSI color codes. COLOR can be :red, :green, :blue, :yellow, :cyan, :magenta, :white, :black"
  (let ((color-code (case color
                      (:black 30)
                      (:red 31)
                      (:green 32)
                      (:yellow 33)
                      (:blue 34)
                      (:magenta 35)
                      (:cyan 36)
                      (:white 37)
                      (t 37)))) ; default to white
    (format stream "~C[~Am~A~C[0m" #\Escape color-code text #\Escape)))

#+sbcl
(defun %ensure-utf8-locale ()
  "Ensure the C locale is inherited from the user's environment.
   SBCL starts its process with LC_ALL=\"C\", which prevents GNU readline's
   internal mbrtowc(3) from decoding multibyte UTF-8 byte sequences.
   In the C locale, readline cannot determine character boundaries or display
   widths for CJK / full-width characters, causing the cursor to drift after
   editing (insert, delete, etc.).
   Calling setlocale(LC_ALL, \"\") inherits LANG / LC_* from the environment
   (typically en_US.UTF-8 or ja_JP.UTF-8) so that mbrtowc and wcwidth work
   correctly.  Must be called once, before any readline interaction."
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "setlocale"
                          (function sb-alien:c-string
                                    sb-alien:int sb-alien:c-string))
   ;; LC_ALL: Darwin=0, Linux=6
   #+darwin 0 #+linux 6 #-(or darwin linux) 0
   ""))

#-sbcl
(defun %ensure-utf8-locale ()
  "No-op on non-SBCL implementations which typically inherit locale."
  (values))

(defun %ensure-readline ()
  "Attempt to load cl-readline for line-editing support (history, Emacs
   keybindings, CJK-aware cursor movement).  If cl-readline is already
   loaded or Quicklisp is not available, this is a no-op.  Any load
   error is logged and silently ignored so the REPL falls back to the
   basic read-line path."
  (unless (find-package :cl-readline)
    (handler-case
        (progn
          (uiop:symbol-call '#:ql '#:quickload :cl-readline :silent t)
          (log-info "repl" "cl-readline loaded — line-editing enabled"))
      (error (e)
        (log-debug "repl" "cl-readline not available: ~a" e)))))

(defun %rl-escape (ansi-string)
  "Wrap ANSI escape sequences in STRING with readline's invisible markers.
   \\001 (RL_PROMPT_START_IGNORE) and \\002 (RL_PROMPT_END_IGNORE) tell
   readline which characters are non-printable, so it correctly calculates
   visible prompt width for cursor positioning.  Without these markers,
   readline counts escape bytes as visible columns, causing cursor drift
   (especially noticeable when editing CJK/full-width characters)."
  (with-output-to-string (out)
    (loop with i = 0
          with len = (length ansi-string)
          while (< i len)
          for ch = (char ansi-string i)
          do (cond
               ;; ESC starts an ANSI sequence — wrap it in \001...\002
               ((char= ch #\Escape)
                (write-char (code-char 1) out)   ; RL_PROMPT_START_IGNORE
                (write-char ch out)
                (incf i)
                ;; Copy through the terminating letter (e.g. 'm')
                (loop while (and (< i len)
                                 (not (alpha-char-p (char ansi-string i))))
                      do (write-char (char ansi-string i) out)
                         (incf i))
                (when (< i len)                  ; the letter itself
                  (write-char (char ansi-string i) out)
                  (incf i))
                (write-char (code-char 2) out))  ; RL_PROMPT_END_IGNORE
               (t
                (write-char ch out)
                (incf i))))))

(defun format-enhanced-prompt (name &optional (command-count 0))
  "Format an enhanced prompt with colors and information.
   Returns the raw ANSI-colored string.  When passing to readline,
   callers should wrap with %rl-escape to mark invisible sequences."
  (with-output-to-string (s)
    (format-colored-text "┌─[" :cyan s)
    (format-colored-text name :green s)
    (when (> command-count 0)
      (format s " ")
      (format-colored-text (format nil "#~A" command-count) :yellow s))
    (format-colored-text "]" :cyan s)
    (format s "~%")
    (format-colored-text "└─> " :cyan s)))

(defun complete-command (partial)
  "Complete a partial command string. Returns list of matching commands."
  (let ((commands (mapcar #'car *repl-commands*)))
    (remove-if-not (lambda (cmd) 
                     (and (>= (length cmd) (length partial))
                          (string= partial cmd :end2 (length partial))))
                   commands)))

(defun handle-colors-command (agent input)
  "Handler for :colors command to toggle color output."
  (declare (ignore agent))
  (let* ((args (string-trim '(#\  #\Tab) 
                           (if (search "/colors" input :test #'string-equal)
                               (subseq input (length "/colors"))
                               input)))
         (action (string-downcase args)))
    (cond
      ((or (string= action "on") (string= action "enable") (string= action "true"))
       (setf *use-colors* t)
       (format-colored-text "Colors enabled!" :green)
       (format t "~%"))
      ((or (string= action "off") (string= action "disable") (string= action "false"))
       (setf *use-colors* nil)
       (format t "Colors disabled.~%"))
      ((string= action "")
       (if *use-colors*
           (progn
             (format-colored-text "Colors are currently " :white)
             (format-colored-text "enabled" :green)
             (format t ".~%"))
           (format t "Colors are currently disabled.~%")))
      (t
       (format t "Usage: /colors [on|off]~%"))))
  nil)

(defun repl-command-p (input)
  "Check if INPUT is a REPL command. Returns the command keyword or NIL.
   Handles commands with arguments by extracting the first word."
  (let* ((trimmed (string-trim '(#\Space #\Tab) input))
         ;; Extract first word (command part)
         (space-pos (position #\Space trimmed))
         (command-part (if space-pos
                          (subseq trimmed 0 space-pos)
                          trimmed)))
    (cdr (assoc command-part *repl-commands* :test #'string-equal))))

;;; ============================================================
;;; Command handler functions
;;; ============================================================

(defun handle-quit-command (agent input)
  "Handler for :quit command."
  (declare (ignore agent input))
  (format t "~%Goodbye.~%")
  :quit)

(defun handle-reset-command (agent input)
  "Handler for :reset command."
  (declare (ignore input))
  ;; Auto-save current session before reset.
  (when *current-session-id*
    (handler-case
        (let* ((mem (sibyl.agent:agent-memory agent))
               (messages (sibyl.llm:conversation-to-list
                          (sibyl.agent:memory-conversation mem)))
               (summary (sibyl.agent:memory-summary mem)))
          (save-session *current-session-id* messages summary
                        :command-count *command-count*)
          (format t "~%Session saved before reset: ~a~%" *current-session-id*))
      (error (e)
        (format *error-output*
                "~&Warning: failed to save session before reset: ~a~%" e))))
  (sibyl.agent:agent-reset agent)
  (format t "~%[Conversation reset]~%")
  nil)

(defun handle-tools-command (agent input)
  "Handler for :list-tools command."
  (declare (ignore input))
  (let ((tools (sibyl.tools:list-tools)))
    (format t "~%Registered tools (~a):~%" (length tools))
    (dolist (tool tools)
      (format t "  ~a — ~a~%"
              (sibyl.tools:tool-name tool)
              (sibyl.tools:tool-description tool))))
  nil)


(defun handle-help-command (agent input)
  "Handler for :help command with enhanced formatting."
  (declare (ignore agent input))
  (if *use-colors*
      (progn
        (format t "~%")
        (format-colored-text "Sibyl REPL Commands:" :cyan)
        (format t "~%~%")
        (format-colored-text "  /help" :green)
        (format t "          — Show this help~%")
        (format-colored-text "  /tools" :green)
        (format t "         — List registered tools~%")
        (format-colored-text "  /reset" :green)
        (format t "         — Reset conversation~%")
        (format-colored-text "  /history" :green)
        (format t "       — Show conversation history~%")
        (format-colored-text "  /mcp" :green)
        (format t "           — Show MCP server status and tools~%")
        (format-colored-text "  /plan" :green)
        (format t "          — Manage development plans~%")
        (format-colored-text "  /improve" :green)
        (format t "       — Request self-improvement (TDD cycle)~%")
        (format-colored-text "  /review" :green)
        (format t "        — Review improvement suggestions~%")
        (format-colored-text "  /tokens" :green)
        (format t "         — Show cumulative token usage statistics~%")
        (format-colored-text "  /model" :green)
        (format t "         — Show current model information~%")
        (format-colored-text "  /sessions" :green)
        (format t "      — List saved sessions~%")
        (format-colored-text "  /save" :green)
        (format t "          — Save current session~%")
        (format-colored-text "  /load <id>" :green)
        (format t "     — Load a saved session~%")
        (format-colored-text "  /colors" :green)
        (format t "        — Toggle color output (on/off)~%")
        (format-colored-text "  /quit" :green)
        (format t "          — Exit REPL~%~%")
        (format-colored-text "Type anything else to chat with the agent." :yellow)
        (format t "~%"))
      (progn
        (format t "~%Sibyl REPL commands:~%")
        (format t "  /help            — Show this help~%")
        (format t "  /tools           — List registered tools~%")
        (format t "  /reset           — Reset conversation~%")
        (format t "  /history         — Show conversation history~%")
        (format t "  /mcp             — Show MCP server status and tools~%")
        (format t "  /plan            — Manage development plans~%")
        (format t "  /improve         — Request self-improvement (TDD cycle)~%")
        (format t "  /review          — Review improvement suggestions~%")
        (format t "  /tokens          — Show cumulative token usage statistics~%")
        (format t "  /model           — Show current model information~%")
        (format t "  /sessions        — List saved sessions~%")
        (format t "  /save            — Save current session~%")
        (format t "  /load <id>       — Load a saved session~%")
        (format t "  /colors          — Toggle color output (on/off)~%")
        (format t "  /quit            — Exit REPL~%")
        (format t "~%Type anything else to chat with the agent.~%")))
  nil)

(defun handle-mcp-command (agent input)
  "Handler for :mcp command. Shows MCP server status and tools."
  (declare (ignore agent input))
  (let ((servers (sibyl.mcp:list-mcp-servers)))
    (if servers
        (progn
          (format t "~%MCP Servers (~a):~%" (length servers))
          (dolist (entry servers)
            (let* ((name (car entry))
                   (client (cdr entry))
                   (status (sibyl.mcp:mcp-client-status client))
                   (info (sibyl.mcp:mcp-client-server-info client))
                   (server-name (when (and info (hash-table-p info))
                                  (gethash "name" info))))
              (if *use-colors*
                  (progn
                    (format-colored-text (format nil "  ~a" name)
                                         (if (eq status :connected) :green :red))
                    (format t " (~a)~@[ — ~a~]~%"
                            status
                            server-name)
                    (format t "    URL: ~a~%" (sibyl.mcp:mcp-client-url client)))
                  (format t "  ~a (~a)~@[ — ~a~]~%    URL: ~a~%"
                          name status server-name
                          (sibyl.mcp:mcp-client-url client)))
              ;; List tools from this server
              (let ((prefix (format nil "~a:" name))
                    (tool-count 0))
                (dolist (tool (sibyl.tools:list-tools))
                  (when (and (>= (length (sibyl.tools:tool-name tool)) (length prefix))
                             (string= prefix (subseq (sibyl.tools:tool-name tool)
                                                     0 (length prefix))))
                    (incf tool-count)
                    (format t "    · ~a — ~a~%"
                            (sibyl.tools:tool-name tool)
                            (sibyl.util:truncate-string
                             (sibyl.tools:tool-description tool) 60))))
                (when (zerop tool-count)
                  (format t "    (no tools)~%"))))))
        (format t "~%No MCP servers connected.~%")))
  nil)

(defun handle-history-command (agent input)
  "Handler for :history command."
  (declare (ignore input))
  (let* ((mem (sibyl.agent:agent-memory agent))
         (messages (sibyl.llm:conversation-to-list
                    (sibyl.agent:memory-conversation mem))))
    (format t "~%Conversation history (~a messages):~%" (length messages))
    (dolist (msg messages)
      (format t "  [~a] ~a~%"
              (sibyl.llm:message-role msg)
              (sibyl.util:truncate-string
               (or (sibyl.llm:message-content msg) "(tool call)")
               120))))
  nil)

(defun handle-improve-command-wrapper (agent input)
  "Wrapper handler for :improve command."
  (handle-improve-command agent input)
  nil)

(defun handle-review-command-wrapper (agent input)
  "Wrapper handler for :review command."
  (handle-review-command agent input)
  nil)


(defun handle-plan-command (agent input)
  "Handler for /plan command. Manages development plans.

   Usage:
   - /plan                       — List all plans
   - /plan <plan-id>             — Show plan details
   - /plan new <title>           — Create a new plan
   - /plan delete <plan-id>      — Delete a plan
   - /plan status <plan-id> <s>  — Update plan status"
  (declare (ignore agent))
  (let* ((trimmed (string-trim '(#\Space #\Tab) (or input "")))
         (args-str (if (search "/plan" trimmed :test #'string-equal)
                       (string-trim '(#\Space #\Tab)
                                    (subseq trimmed (length "/plan")))
                       trimmed)))
    (cond
      ;; No arguments: list all plans
      ((string= args-str "")
       (let ((result (sibyl.tools:execute-tool "list-plans" nil)))
         (format t "~%~a~%" result)))

      ;; /plan new <title>
      ((and (>= (length args-str) 4)
            (string-equal (subseq args-str 0 3) "new"))
       (let ((title (string-trim '(#\Space #\Tab) (subseq args-str 3))))
         (if (string= title "")
             (format t "~%Usage: /plan new <title>~%")
             (let ((plan-id (sibyl.tools:execute-tool
                             "save-plan"
                             (list (cons "title" title)))))
               (format t "~%Plan created: ~a~%" plan-id)))))

      ;; /plan delete <plan-id>
      ((and (>= (length args-str) 7)
            (string-equal (subseq args-str 0 6) "delete"))
       (let ((plan-id (string-trim '(#\Space #\Tab) (subseq args-str 6))))
         (if (string= plan-id "")
             (format t "~%Usage: /plan delete <plan-id>~%")
             (let ((result (sibyl.tools:execute-tool
                            "delete-plan"
                            (list (cons "plan-id" plan-id)))))
               (format t "~%~a~%" result)))))

      ;; /plan status <plan-id> <status>
      ((and (>= (length args-str) 7)
            (string-equal (subseq args-str 0 6) "status"))
       (let* ((rest (string-trim '(#\Space #\Tab) (subseq args-str 6)))
              (parts (cl-ppcre:split "\\s+" rest :limit 2))
              (plan-id (first parts))
              (new-status (second parts)))
         (if (or (null plan-id) (null new-status))
             (format t "~%Usage: /plan status <plan-id> <draft|in-progress|completed|abandoned>~%")
             (handler-case
                 (let ((result (sibyl.tools:execute-tool
                                "update-plan-status"
                                (list (cons "plan-id" plan-id)
                                      (cons "status" new-status)))))
                   (format t "~%~a~%" result))
               (error (e)
                 (format t "~%Error: ~a~%" e))))))

      ;; /plan <plan-id> — show details
      (t
       (handler-case
           (let ((result (sibyl.tools:execute-tool
                          "load-plan"
                          (list (cons "plan-id" args-str)))))
             (format t "~%~a~%" result))
         (error (e)
           (format t "~%Error: ~a~%" e))))))
  nil)


(defun format-token-usage (tracker)
  "Format token usage statistics for display. Returns a string."
  (let* ((input      (sibyl.llm::token-tracker-input-tokens tracker))
         (output     (sibyl.llm::token-tracker-output-tokens tracker))
         (cache-read (sibyl.llm::token-tracker-cache-read-tokens tracker))
         (cache-write (sibyl.llm::token-tracker-cache-write-tokens tracker))
         (requests   (sibyl.llm::token-tracker-request-count tracker))
         (hit-rate   (sibyl.llm::tracker-cache-hit-rate tracker))
         ;; Response cache (client-side) telemetry
         (cache-stats (sibyl.cache:get-cache-telemetry))
         (cache-hits   (getf cache-stats :client-hits))
         (cache-misses (getf cache-stats :client-misses))
         (cache-total  (getf cache-stats :total-requests))
         (cache-hit-rate (getf cache-stats :hit-rate))
         (rcache-stats (sibyl.cache:response-cache-stats))
         (cache-entries (if rcache-stats (getf rcache-stats :size) 0))
         (cache-max     (if rcache-stats (getf rcache-stats :max-size) 0)))
    (format nil "~%Token Usage (this session):~%  Input:       ~:d tokens~%  Output:      ~:d tokens~%  Cache Read:  ~:d tokens (~,1f% server hit rate)~%  Cache Write: ~:d tokens~%  Requests:    ~:d~%~%Response Cache:~%  Hits:        ~:d~%  Misses:      ~:d~%  Hit Rate:    ~,1f%  (~:d / ~:d requests)~%  Entries:     ~:d / ~:d~%"
            input output cache-read (* hit-rate 100.0) cache-write requests
            cache-hits cache-misses (* cache-hit-rate 100.0) cache-hits cache-total
            cache-entries cache-max)))

(defun handle-tokens-command (agent input)
  "Handler for :tokens command. Displays cumulative token usage statistics."
  (declare (ignore input))
  (let* ((tracker (sibyl.agent:agent-token-tracker agent))
         (client (sibyl.agent:agent-client agent))
         (model-name (ignore-errors (sibyl.llm::client-model client))))
    ;; Use new session summary display
    (sibyl.repl.display:format-session-summary tracker (or model-name "unknown")))
  nil)


(defun handle-model-command (agent input)
  "Handler for :model command. Shows current model information.

   Usage: /model — Show current model name, context window, and provider"
  (declare (ignore input))
  (%model-show-status agent)
  nil)

(defun %model-show-status (agent)
  "Show current model information (read-only)."
  (let* ((client (sibyl.agent:agent-client agent))
         (model-name (ignore-errors (sibyl.llm::client-model client)))
         (context-window (when model-name
                           (ignore-errors 
                             (sibyl.llm::context-window-for-model model-name))))
         (provider (type-of client)))
    (format t "~%Current Model Information:~%")
    (if *use-colors*
        (progn
          (format t "  Model: ")
          (if model-name
              (format-colored-text model-name :green)
              (format t "(unknown)"))
          (format t "~%")
          (when context-window
            (format t "  Context Window: ")
            (format-colored-text (format nil "~:d tokens" context-window) :cyan)
            (format t "~%"))
          (format t "  Provider: ")
          (format-colored-text (symbol-name provider) :yellow)
          (format t "~%"))
        (progn
          (format t "  Model: ~a~%" (or model-name "(unknown)"))
          (when context-window
            (format t "  Context Window: ~:d tokens~%" context-window))
          (format t "  Provider: ~a~%" (symbol-name provider))))))

(defun save-cost-log (report)
  "Persist a SESSION-COST-REPORT to ~/.sibyl/cost-log.json.
   Appends a new entry to the JSON array in the file.
   Returns the path written, or NIL on error."
  (let* ((dir (merge-pathnames ".sibyl/" (user-homedir-pathname)))
         (path (merge-pathnames "cost-log.json" dir)))
    (handler-case
        (progn
          (ensure-directories-exist dir)
          (let* ((existing
                   (if (probe-file path)
                       (handler-case
                           (yason:parse (uiop:read-file-string path)
                                        :object-as :hash-table)
                         (error () (make-array 0 :adjustable t :fill-pointer 0)))
                       (make-array 0 :adjustable t :fill-pointer 0)))
                 (arr (if (vectorp existing)
                          existing
                          (make-array 0 :adjustable t :fill-pointer 0)))
                 (entry (make-hash-table :test 'equal)))
            (setf (gethash "timestamp" entry)
                  (get-universal-time))
            (setf (gethash "task_count" entry)
                  (sibyl.llm::session-cost-report-task-count report))
            (setf (gethash "actual_cost_usd" entry)
                  (sibyl.llm::session-cost-report-total-actual-cost-usd report))
            (setf (gethash "cache_hit_rate" entry)
                  (sibyl.llm::session-cost-report-cache-hit-rate report))
            (vector-push-extend entry arr)
            (with-open-file (out path :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
              (yason:encode arr out))
            path))
      (error (e)
        (log-warn "repl" "Failed to save cost log: ~a" e)
        nil))))

(defun handle-cost-report-command (agent input)
  "Handler for /cost-report command.
   Displays a formatted session cost report.
   Saves the report to ~/.sibyl/cost-log.json."
  (declare (ignore input))
  (let* ((records (sibyl.agent::agent-cost-records agent))
         (n (length records)))
    (if (zerop n)
        (format t "~%No cost data yet. Run some tasks first.~%")
        (let ((report (sibyl.llm::compute-session-report records)))
          ;; Print the standard session report
          (sibyl.llm::format-session-report report)
          ;; Persist to disk
          (let ((saved-path (save-cost-log report)))
            (when saved-path
              (format t "  [Saved to ~a]~%" saved-path)))
          report)))
  nil)



;;; Session command handlers

(defun handle-sessions-command (agent input)
  "Handler for /sessions — list saved sessions."
  (declare (ignore agent input))
  (let ((sessions (list-sessions)))
    (if sessions
        (progn
          (format t "~%Saved sessions:~%")
          (format t "  ~30a ~20a ~8a~%" "ID" "Last Modified" "Messages")
          (format t "  ~30,,,'-a ~20,,,'-a ~8,,,'-a~%" "" "" "")
          (dolist (s sessions)
            (format t "  ~30a ~20a ~8a~%"
                    (getf s :id)
                    (or (getf s :last-modified) "-")
                    (or (getf s :message-count) 0))))
        (format t "~%No saved sessions.~%")))
  nil)

(defun handle-save-session-command (agent input)
  "Handler for /save — save current session to disk."
  (declare (ignore input))
  (if *current-session-id*
      (let* ((mem (sibyl.agent:agent-memory agent))
             (messages (sibyl.llm:conversation-to-list
                        (memory-conversation mem)))
             (summary (memory-summary mem)))
        (save-session *current-session-id* messages summary
                      :command-count *command-count*)
        (format t "~%Session saved: ~a~%" *current-session-id*))
      (format t "~%No active session to save.~%"))
  nil)

(defun handle-load-session-command (agent input)
  "Handler for /load <id> — switch to another session."
  (let ((session-id (string-trim '(#\Space #\Tab)
                                 (subseq input (min 5 (length input))))))
    (if (or (null session-id) (string= session-id ""))
        (format t "~%Usage: /load <session-id>~%")
        (progn
          ;; Auto-save current session first
          (when *current-session-id*
            (let* ((mem (sibyl.agent:agent-memory agent))
                   (messages (sibyl.llm:conversation-to-list
                              (memory-conversation mem)))
                   (summary (memory-summary mem)))
              (save-session *current-session-id* messages summary
                            :command-count *command-count*)))
          ;; Load requested session
          (multiple-value-bind (messages summary command-count)
              (load-session session-id)
            (if messages
                (let ((mem (sibyl.agent:agent-memory agent)))
                  ;; Replace conversation
                  (sibyl.llm:conversation-clear (memory-conversation mem))
                  (dolist (msg messages)
                    (sibyl.llm:conversation-push (memory-conversation mem) msg))
                  ;; Restore summary and counts
                  (setf (memory-summary mem) summary)
                  (setf *current-session-id* session-id)
                  (setf *command-count* (or command-count 0))
                  (format t "~%Loaded session: ~a (~a messages)~%"
                          session-id (length messages)))
                (format t "~%Session not found: ~a~%" session-id))))))
  nil)

;;; Command handler registry

(defparameter *command-handlers*
  (list (cons :quit    #'handle-quit-command)
        (cons :reset   #'handle-reset-command)
        (cons :list-tools #'handle-tools-command)
        (cons :help    #'handle-help-command)
        (cons :history #'handle-history-command)
        (cons :mcp     #'handle-mcp-command)
        (cons :plan    #'handle-plan-command)
        (cons :improve #'handle-improve-command-wrapper)
        (cons :review  #'handle-review-command-wrapper)
        (cons :tokens        #'handle-tokens-command)
        (cons :model         #'handle-model-command)
        (cons :cost-report   #'handle-cost-report-command)
        (cons :sessions     #'handle-sessions-command)
        (cons :save-session #'handle-save-session-command)
        (cons :load-session #'handle-load-session-command))
  "Mapping of command keywords to handler functions.")

(defun handle-repl-command (command agent &optional original-input)
  "Handle a REPL command. Returns :quit to exit, or NIL to continue.
   ORIGINAL-INPUT is the full command string including arguments.
   Uses dynamic dispatch via *command-handlers* alist."
  (log-debug "repl" "Command ~a" command)
  (let ((handler (cdr (assoc command *command-handlers*))))
    (if handler
        (funcall handler agent original-input)
        (progn
          (format t "~%Unknown command: ~a~%" command)
          nil))))

(defun parse-improve-args (input)
  "Parse /improve command arguments. Returns (values task-description auto-commit-p).
   Example: '/improve add feature --auto-commit' => ('add feature' T)"
  (let* ((trimmed (string-trim '(#\Space #\Tab) input))
         ;; Remove '/improve' prefix
         (args-str (if (search "/improve" trimmed :test #'string-equal)
                       (string-trim '(#\Space #\Tab)
                                    (subseq trimmed (length "/improve")))
                       trimmed))
         ;; Check for --auto-commit flag
         (auto-commit-pos (search "--auto-commit" args-str :test #'string-equal))
         (task-desc (if auto-commit-pos
                        (string-trim '(#\Space #\Tab)
                                     (subseq args-str 0 auto-commit-pos))
                        args-str)))
    (values task-desc (not (null auto-commit-pos)))))

(defun parse-review-args (input)
  "Parse /review command arguments. Returns (values action id modification).
   Examples:
   - '/review' => (NIL NIL NIL) - list all
   - '/review approve 1' => ('approve' 1 NIL)
   - '/review reject 2' => ('reject' 2 NIL)
   - '/review modify 3 \"add unit tests\"' => ('modify' 3 'add unit tests')"
  (let* ((trimmed (string-trim '(#\Space #\Tab) input))
         ;; Remove '/review' prefix
         (args-str (if (search "/review" trimmed :test #'string-equal)
                       (string-trim '(#\Space #\Tab)
                                    (subseq trimmed (length "/review")))
                       trimmed)))
    (if (string= "" args-str)
        ;; No arguments - list all
        (values nil nil nil)
        ;; Parse arguments
        (let* ((parts (cl-ppcre:split "\\s+" args-str :limit 3))
               (action (first parts))
               (id-str (second parts))
               (modification (third parts)))
          (values action
                  (when id-str
                    (handler-case
                        (parse-integer id-str)
                      (error () nil)))
                   modification)))))


(defun list-pending-suggestions ()
  "Display all pending suggestions."
  (if (null *pending-suggestions*)
      (format t "~%No pending suggestions.~%")
      (progn
        (format t "~%Pending improvement suggestions:~%~%")
        (dolist (suggestion (reverse *pending-suggestions*))
          (format t "  [~a] ~a (Priority: ~a)~%"
                  (getf suggestion :id)
                  (getf suggestion :description)
                  (getf suggestion :priority))
          (format t "      Rationale: ~a~%"
                  (getf suggestion :rationale))
          (format t "      Status: ~a~%~%"
                  (getf suggestion :status))))))

(defun approve-and-implement (agent id)
  "Approve suggestion and delegate to /improve command."
  (let ((suggestion (get-suggestion-by-id id)))
    (if (null suggestion)
        (format t "~%Error: Suggestion #~a not found.~%" id)
        (progn
          (update-suggestion-status id "approved")
          (format t "~%Approving and implementing suggestion #~a:~%" id)
          (format t "  ~a~%~%" (getf suggestion :description))
          ;; Call /improve with the suggestion description
          (handle-improve-command agent 
                                  (format nil "/improve ~a" 
                                          (getf suggestion :description)))))))

(defun reject-suggestion (id)
  "Reject a suggestion."
  (let ((suggestion (get-suggestion-by-id id)))
    (if (null suggestion)
        (format t "~%Error: Suggestion #~a not found.~%" id)
        (progn
          (update-suggestion-status id "rejected")
          (format t "~%Rejected suggestion #~a.~%" id)))))

(defun modify-and-approve (agent id modification)
  "Modify a suggestion and approve it."
  (let ((suggestion (get-suggestion-by-id id)))
    (if (null suggestion)
        (format t "~%Error: Suggestion #~a not found.~%" id)
        (progn
          (update-suggestion-status id "modified")
          (format t "~%Modifying and implementing suggestion #~a:~%" id)
          (format t "  Original: ~a~%" (getf suggestion :description))
          (format t "  Modified: ~a~%~%" modification)
          ;; Call /improve with the modified description
          (handle-improve-command agent
                                  (format nil "/improve ~a" modification))))))

(defun handle-review-command (agent original-input)
  "Handle /review command for managing improvement suggestions.
   
   Usage:
   - /review                    — List all pending suggestions
   - /review approve <id>       — Approve and implement suggestion
   - /review reject <id>        — Reject suggestion
   - /review modify <id> <desc> — Modify and implement suggestion"
  (multiple-value-bind (action id modification)
      (parse-review-args original-input)
    
    (cond
      ;; No action: list all suggestions
      ((null action)
       (list-pending-suggestions))
      
      ;; Approve
      ((string-equal action "approve")
       (if (null id)
           (format t "~%Error: No suggestion ID provided.~%~
                       Usage: /review approve <id>~%")
           (approve-and-implement agent id)))
      
      ;; Reject
      ((string-equal action "reject")
       (if (null id)
           (format t "~%Error: No suggestion ID provided.~%~
                       Usage: /review reject <id>~%")
           (reject-suggestion id)))
      
      ;; Modify
      ((string-equal action "modify")
       (cond
         ((null id)
          (format t "~%Error: No suggestion ID provided.~%~
                      Usage: /review modify <id> <description>~%"))
         ((null modification)
          (format t "~%Error: No modification description provided.~%~
                      Usage: /review modify <id> <description>~%"))
         (t
          (modify-and-approve agent id modification))))
      
      ;; Unknown action
      (t
       (format t "~%Error: Unknown action '~a'.~%~
                   Valid actions: approve, reject, modify~%~
                   Usage: /review [approve|reject|modify] <id> [description]~%"
               action)))))

(defun handle-improve-command (agent original-input)
  "Handle /improve command for self-improvement tasks.
   Orchestrates full TDD cycle for requested improvement.
   
   Usage: /improve <task description> [--auto-commit]"
  (multiple-value-bind (task-desc auto-commit-p)
      (parse-improve-args original-input)
    
    ;; Validate task description
    (when (string= "" task-desc)
      (format t "~%Error: No task description provided.~%")
      (format t "Usage: /improve <task description> [--auto-commit]~%")
      (return-from handle-improve-command))
    
    ;; Build TDD prompt for agent
    (let ((prompt (format nil 
                    "Improve Sibyl's code: ~a~%~%~
                     Follow the TDD workflow strictly:~%~
                     1. UNDERSTAND: Use read-sexp and who-calls to understand current code~%~
                     2. RED: Write test first using write-test → confirm it fails with run-tests~%~
                     3. GREEN: Implement using safe-redefine → confirm test passes with run-tests~%~
                     4. REFACTOR (if needed): Clean up code → run-tests to ensure still green~%~
                     ~:[5. WAIT: Do NOT use sync-to-file yet - report results and wait for human approval~;~
                     5. PERSIST: Use sync-to-file to write changes to disk~]"
                    task-desc auto-commit-p)))
      
      (format t "~%[Starting self-improvement task]~%")
      (format t "Task: ~a~%" task-desc)
      (format t "Auto-commit: ~:[NO~;YES~]~%~%" auto-commit-p)
      
      ;; Execute agent step with TDD prompt
      (handler-case
          (let ((response (sibyl.agent:agent-run agent prompt)))
            (format t "~%~a~%~%" response)
            
            ;; If not auto-commit, ask for confirmation
            (unless auto-commit-p
              (format t "~%Do you want to commit these changes? (y/n): ")
              (force-output)
              (let ((confirmation (read-line *standard-input* nil nil)))
                (when (and confirmation 
                           (string-equal (string-trim '(#\Space #\Tab) confirmation) "y"))
                  (format t "~%Committing changes to disk...~%")
                  (let ((sync-response
                          (sibyl.agent:agent-run agent 
                            "Please use sync-to-file to persist all changes to disk, then use sibyl.system:unprotect-file.")))
                    (format t "~%~a~%~%" sync-response))))))
        
        (sibyl.conditions:llm-error (e)
          (format t "~%[LLM Error: ~a]~%~%" e))
        (error (e)
          (format t "~%[Error: ~a]~%~%" e))))))


;;; ============================================================
;;; Hook functions
;;; ============================================================

(defun make-tool-call-hook (&optional spinner)
  "Return a closure suitable for the :on-tool-call agent hook.
    The closure accepts a TOOL-CALL struct and displays the tool name in cyan,
    followed by a brief summary of the primary argument(s).
 
    If a spinner is active, it is stopped first so the message is not erased
    by the spinner's line-clear escape sequence.  After displaying the tool info,
    a fresh spinner is started with a 'Thinking...' message to provide visual
    feedback during tool execution.
 
    Usage:
      (cons :on-tool-call (make-tool-call-hook spinner))"
  (lambda (tc)
    (let* ((tool-name (sibyl.llm:tool-call-name tc))
           (summary (format-tool-call-summary tc))
           (display
            (if (string= summary "")
                (format nil "🔧 ~a ..." tool-name)
                (format nil "🔧 ~a ... ~a" tool-name summary))))
      (log-info "agent" "Tool call: ~a ~a" tool-name summary)
      ;; Serialize spinner stop → output → spinner restart across threads.
      ;; Parallel tool execution fires this hook from multiple worker threads;
      ;; without the lock, concurrent escape-sequence writes garble the terminal.
      (bt:with-lock-held (*spinner-output-lock*)
        (let ((active-spinner (or spinner *current-spinner*)))
          ;; Stop the spinner BEFORE printing so the line-clear (\r\e[2K) from
          ;; stop-spinner runs first, then our message is written on a clean line.
          (when (and active-spinner
                     (sibyl.repl.spinner:spinner-active-p active-spinner))
            (sibyl.repl.spinner:stop-spinner active-spinner)
            (setf *current-spinner* nil))
          (if *use-colors*
              (progn (format-colored-text display :cyan) (format t "~%"))
              (format t "~a~%" display))
          (force-output)
          ;; Restart spinner only on interactive terminal streams.
          ;; Non-interactive streams (e.g. with-output-to-string in tests)
          ;; must avoid background spinner threads.
          (when (%spinner-stream-supported-p *standard-output*)
            (let ((new-spinner (sibyl.repl.spinner:start-spinner "Thinking...")))
              (setf *current-spinner* new-spinner))))))))

(defun format-tool-call-summary (tc &key (max-length 50))
  "Format tool call arguments into a human-readable short string.
    Example: shell + ((command . \"ls -la\")) → \"(ls -la)\"
             eval-form + ((form . \"(defun foo () 42)\")) → \"(defun foo () 42)\"
    If arguments are long, truncate to max-length characters."
  (let* ((args (sibyl.llm:tool-call-arguments tc))
         (tool-name (sibyl.llm:tool-call-name tc))
          ;; Primary argument keys per tool (in priority order)
          (primary-keys
           (cond
             ((string= tool-name "shell")           '("command"))
             ((string= tool-name "read-file")       '("path"))
             ((string= tool-name "write-file")      '("path"))
             ((string= tool-name "eval-form")       '("form"))
             ((string= tool-name "grep")            '("pattern" "path"))
             ((string= tool-name "list-directory")  '("path"))
             ((string= tool-name "file-info")       '("path"))
             ((string= tool-name "read-sexp")       '("path" "name"))
             ((string= tool-name "describe-symbol") '("symbol"))
             ((string= tool-name "who-calls")       '("function"))
             ((string= tool-name "safe-redefine")   '("name"))
             ((string= tool-name "sync-to-file")    '("name" "file"))
             ((string= tool-name "run-tests")       '("suite" "test"))
             ((string= tool-name "write-test")      '("name"))
             ((string= tool-name "add-definition")  '("file"))
             ((string= tool-name "create-module")   '("path"))
              (t nil)))
          ;; Get primary argument value
          (values-to-show
           (if primary-keys
               (remove nil
                 (mapcar (lambda (k)
                           (let ((pair (assoc k args :test #'string=)))
                             (when pair
                               (let ((v (cdr pair)))
                                 (if (stringp v) v (format nil "~a" v))))))
                          primary-keys))
                ;; Fallback: value of the first argument
                (when args
                 (let ((v (cdr (first args))))
                    (list (if (stringp v) v (format nil "~a" v)))))))
          ;; Join values
          (summary (if values-to-show
                      (format nil "~{~a~^ ~}" values-to-show)
                      "")))
     (cond
       ((string= summary "") "")
       ;; If value already starts with a paren (e.g. Lisp form), display as-is
       ((char= (char summary 0) #\()
       (if (> (length summary) max-length)
            (format nil "~a..." (subseq summary 0 max-length))
            summary))
       ;; Otherwise wrap in parens
       (t
       (if (> (length summary) max-length)
           (format nil "(~a...)" (subseq summary 0 max-length))
           (format nil "(~a)" summary))))))

(defun make-before-step-hook (&optional spinner)
  "Return a closure suitable for the :before-step agent hook.
   Currently a no-op stub; Task 7 will wire the spinner into this."
  (declare (ignore spinner))
  (lambda (step-count)
    (log-debug "agent" "Starting step ~a" step-count)))

(defun make-after-step-hook (&optional spinner)
  "Return a closure suitable for the :after-step agent hook.
   Currently a no-op stub; Task 7 will wire the spinner into this."
  (declare (ignore spinner))
  (lambda (&rest args)
    (declare (ignore args))))

(defun make-on-error-hook ()
  "Return a closure suitable for the :on-error agent hook."
  (lambda (err)
    (log-error "agent" "Agent error: ~a" err)))

(defun make-compact-hook ()
  "Return a closure suitable for the :on-compact agent hook.
   Displays a context compaction notification when the conversation history
   is compressed.  Mirrors the style of make-tool-result-hook but uses a
   dedicated 🗜️  prefix and dim ANSI styling so it is visually distinct.

   The closure receives (summary-text) and outputs:
     🗜️  Context compacted

   If a spinner is currently active it is stopped first (to avoid the
   spinner's \\r\\e[2K from erasing the message), then a fresh 'Thinking...'
   spinner is started because a new LLM call is imminent.

   Usage:
     (cons :on-compact (make-compact-hook))"
  (lambda (summary)
    (declare (ignore summary))
    (bt:with-lock-held (*spinner-output-lock*)
      (let ((active-spinner *current-spinner*))
        (cond
          ;; Spinner active — stop it, then clear the line for our message.
          ((and active-spinner
                (sibyl.repl.spinner:spinner-active-p active-spinner))
           (sibyl.repl.spinner:stop-spinner active-spinner)
           (setf *current-spinner* nil)
           (format t "~C~C[2K" #\Return #\Escape))
          ;; No spinner — cursor may be mid-line from streamed text.
          ;; fresh-line avoids erasing content that \r\e[2K would destroy.
          (t
           (fresh-line)))
        (if *use-colors*
            (format t "~C[2m🗜️  Context compacted~C[0m~%" #\Escape #\Escape)
            (format t "🗜️  Context compacted~%"))
        (force-output)
        ;; Restart spinner only on interactive terminal streams.
        (when (%spinner-stream-supported-p *standard-output*)
          (let ((new-spinner (sibyl.repl.spinner:start-spinner "Thinking...")))
            (setf *current-spinner* new-spinner)))))))

(defun make-tool-call-hook-v2 (&optional spinner)
  "Return a closure suitable for the :on-tool-call agent hook (v2).
   Displays '🔧 tool-name ...' during execution.

   If a spinner is active, it is stopped first. After displaying the tool start
   line (without a trailing newline), a fresh spinner is started with
   'Thinking...' message.  The spinner and the result hook both use \\r\\e[2K
   to overwrite the same terminal line, so only one line per tool call appears.

   Usage:
     (cons :on-tool-call (make-tool-call-hook-v2 spinner))"
  (lambda (tc)
    (let ((display (sibyl.repl.display:format-tool-start-line tc)))
      (log-info "agent" "Tool call: ~a" (sibyl.llm:tool-call-name tc))
      (bt:with-lock-held (*spinner-output-lock*)
        (let ((active-spinner (or spinner *current-spinner*)))
          (cond
            ;; Spinner active — stop it.  stop-spinner clears the line via
            ;; \r\e[2K so the tool display can be written on the now-blank line.
            ((and active-spinner
                  (sibyl.repl.spinner:spinner-active-p active-spinner))
             (sibyl.repl.spinner:stop-spinner active-spinner)
             (setf *current-spinner* nil))
            ;; No spinner — the streaming callback already stopped it and wrote
            ;; response text.  The cursor may be mid-line (text without trailing
            ;; newline).  Emit fresh-line so the subsequent spinner's \r\e[2K
            ;; won't erase streamed content.
            (t
             (fresh-line)))
          ;; No trailing newline — spinner and result hook will overwrite this line.
          (if *use-colors*
              (format-colored-text display :cyan)
              (format t "~a" display))
          (force-output)
          (when (%spinner-stream-supported-p *standard-output*)
            (let ((new-spinner (sibyl.repl.spinner:start-spinner "Thinking...")))
              (setf *current-spinner* new-spinner))))))))



(defun make-thinking-display-callback ()
  "Return a lambda suitable for *streaming-thinking-callback*.
Displays thinking chunks in magenta ANSI style, prefixed with '💭 ' on the
first chunk of EACH thinking block.

A new thinking block is detected when *current-spinner* is active at the
time a chunk arrives — indicating that a tool call restarted the spinner
since the last thinking block ended.  In that case:
  1. The spinner is stopped (preventing \\r\\e[2K from overwriting the text).
  2. The '💭' prefix is re-emitted for the new block.

Newline handling:
  - After stopping a spinner: cursor is already at column 0 of a cleared
    line (spinner's \\r\\e[2K), so the prefix is written on that line directly
    (no leading newline needed, avoids a spurious blank line).
  - Without an active spinner: cursor may be mid-line, so fresh-line is used
    before emitting the prefix.

Sets *thinking-output-active* to T after writing any chunk, so downstream
callbacks (e.g. text streaming) can emit a newline separator.

Returns NIL when *stream-enabled* is NIL (streaming disabled)."
  (when *stream-enabled*
    (let ((in-thinking-block nil))
      (lambda (thinking-chunk)
        (when (and thinking-chunk (plusp (length thinking-chunk)))
          (let ((just-stopped-spinner nil))
            ;; If a spinner is active it means a tool call has occurred since
            ;; the last thinking block.  Stop it and reset the block flag so
            ;; the prefix is re-emitted for this new block.
            (when (and *current-spinner*
                       (sibyl.repl.spinner:spinner-active-p *current-spinner*))
              (bt:with-lock-held (*spinner-output-lock*)
                (sibyl.repl.spinner:stop-spinner *current-spinner*)
                (setf *current-spinner* nil))
              (setf in-thinking-block nil
                    just-stopped-spinner t))
            ;; Emit the block header at the start of each new thinking block.
            (unless in-thinking-block
              (setf in-thinking-block t)
              (if just-stopped-spinner
                  ;; Spinner's \r\e[2K left cursor at col 0 of a cleared line.
                  ;; Write the prefix directly on that line — no extra newline.
                  (if *use-colors*
                      (format t "~C[35m💭 ~C[0m" #\Escape #\Escape)
                      (format t "[thinking] "))
                  ;; No spinner was active; cursor may be mid-line after streamed
                  ;; text or a previous thinking block.  Ensure a new line first.
                  (if *use-colors*
                      (format t "~%~C[35m💭 ~C[0m" #\Escape #\Escape)
                      (format t "~%[thinking] ")))
              (force-output))
            ;; Write the chunk in magenta (or plain text).
            (if *use-colors*
                (format t "~C[35m~a~C[0m" #\Escape thinking-chunk #\Escape)
                (write-string thinking-chunk *standard-output*))
            ;; Signal that thinking output was produced so the text streaming
            ;; callback can emit a trailing newline separator.
            (setf *thinking-output-active* t)
            (force-output *standard-output*)))))))

(defun make-tool-result-hook ()
  "Return a closure suitable for the :on-tool-result agent hook.
   Overwrites the current line with a result line showing ✓/✗, tool name, args, time, size.
   After displaying the result, restarts a 'Thinking...' spinner so the user has
   visual feedback while the LLM processes the tool result and decides next steps.

   The closure receives (tool-call result elapsed-seconds) and displays:
     ✓ read-file (src/repl.lisp) 0.02s 14.9 KB
     ✗ shell (bad-cmd) 1.20s Error

   Usage:
     (cons :on-tool-result (make-tool-result-hook))"
  (lambda (tc result elapsed-seconds)
    (let ((bytes (if (stringp result) (length result) 0)))
      (bt:with-lock-held (*spinner-output-lock*)
        (let ((active-spinner *current-spinner*))
          (cond
            ;; Spinner active — stop it and clear the line for the result.
            ((and active-spinner
                  (sibyl.repl.spinner:spinner-active-p active-spinner))
             (sibyl.repl.spinner:stop-spinner active-spinner)
             (setf *current-spinner* nil)
             (format t "~C~C[2K~a~%"
                     #\Return #\Escape
                     (sibyl.repl.display:format-tool-result-line tc result elapsed-seconds bytes)))
            ;; No spinner — cursor may be mid-line; use fresh-line + normal output.
            (t
             (fresh-line)
             (format t "~a~%"
                     (sibyl.repl.display:format-tool-result-line tc result elapsed-seconds bytes))))
          (force-output)
          ;; Restart spinner so user has feedback during the next LLM call.
          ;; The streaming callback or on-tool-call hook will stop it when appropriate.
          (let ((new-spinner (sibyl.repl.spinner:start-spinner "Thinking...")))
            (setf *current-spinner* new-spinner)))))))

;;; ============================================================
;;; Elapsed time utilities
;;; ============================================================

(defun elapsed-seconds (start-time)
  "Compute elapsed seconds since START-TIME (from get-internal-real-time)."
  (/ (- (get-internal-real-time) start-time) 
     (float internal-time-units-per-second)))

(defun format-elapsed-time (seconds &key (stream *standard-output*) model tokens input-tokens output-tokens
                                      cache-hits cache-total)
  "Format elapsed time as [model · elapsed: X.Xs · In X / Out Y · Cache N/M] with optional dim styling.
   When MODEL is non-NIL, includes the model name before the elapsed time.
   When INPUT-TOKENS and OUTPUT-TOKENS are non-NIL, appends 'In X / Out Y'.
   TOKENS (legacy scalar) is still accepted as fallback.
   When CACHE-HITS and CACHE-TOTAL are non-NIL, appends 'Cache N/M'.
   When *use-colors* is nil, outputs plain text. Otherwise uses ANSI dim code."
  (let ((label
         (with-output-to-string (s)
           (when model (format s "~a · " model))
           (format s "elapsed: ~,1fs" seconds)
           (cond
             ((and input-tokens output-tokens)
              (format s " · In ~:d / Out ~:d" input-tokens output-tokens))
             (tokens
              (format s " · ~:d tok" tokens)))
           (when (and cache-total (plusp cache-total))
             (format s " · Cache ~d/~d" cache-hits cache-total)))))
    (if *use-colors*
        (format stream "~C[2m[~a]~C[0m" #\Escape label #\Escape)
        (format stream "[~a]" label))))

(defmacro with-elapsed-time (&body body)
  "Execute BODY and measure wall-clock elapsed time."
  (let ((start-time (gensym "START-TIME-")))
    `(let ((,start-time (get-internal-real-time)))
       (prog1
           (progn ,@body)
         (format-elapsed-time (elapsed-seconds ,start-time))))))

;;; ============================================================
;;; Main REPL loop
;;; ============================================================

(defun print-banner ()
  "Print the enhanced Sibyl welcome banner with colors."
  (if *use-colors*
      (progn
        (format t "~%")
        (format-colored-text "╔═══════════════════════════════════════╗" :cyan)
        (format t "~%")
        (format-colored-text "║          " :cyan)
        (format-colored-text "S I B Y L" :green)
        (format-colored-text "  v0.1.0           ║" :cyan)
        (format t "~%")
        (format-colored-text "║     " :cyan)
        (format-colored-text "Lisp-based Coding Agent" :yellow)
        (format-colored-text "          ║" :cyan)
        (format t "~%")
        (format-colored-text "╚═══════════════════════════════════════╝" :cyan)
        (format t "~%~%")
        (format-colored-text "Type " :white)
        (format-colored-text "/help" :green)
        (format-colored-text " for commands, or start chatting." :white)
        (format t "~%~%"))
      ;; Fallback to original banner if colors disabled
      (format t "~%~
╔═══════════════════════════════════════╗~%~
║          S I B Y L  v0.1.0           ║~%~
║     Lisp-based Coding Agent          ║~%~
╚═══════════════════════════════════════╝~%~
~%Type /help for commands, or start chatting.~%~%")))

(defun readline-available-p ()
  "Returns T if cl-readline is loaded and available, NIL otherwise."
  (not (null (find-package :cl-readline))))

(defun %strip-ctrl-j (input)
  "Remove Ctrl+J (linefeed) characters from INPUT string."
  (when input
    (remove-if (lambda (ch) (= (char-code ch) 10)) input)))

(defun read-user-input ()
  "Read a line of input using cl-readline if available, read-line otherwise.
   Returns NIL on EOF."
  (let* ((prompt (if *use-colors*
                     (format-enhanced-prompt "sibyl" *command-count*)
                     "sibyl> "))
         (input (if (readline-available-p)
                    ;; cl-readline handles prompt display and history navigation.
                    ;; Use find-symbol to avoid compile-time package dependency.
                    ;; Wrap prompt with %rl-escape so readline knows which bytes
                    ;; are ANSI escapes (invisible) — fixes cursor drift with
                    ;; CJK/full-width characters.
                     (handler-case
                         (funcall (find-symbol "READLINE" :cl-readline)
                                  :prompt (%rl-escape prompt)
                                  :add-history t
                                  :novelty-check #'string/=)
                       (error () (read-line *standard-input* nil nil)))
                    ;; Fallback: manual prompt + read-line (no rl-escape needed)
                    (progn
                      (format t "~A" prompt)
                      (force-output)
                      (read-line *standard-input* nil nil))))
         (sanitized (if *ignore-ctrl-j*
                        (%strip-ctrl-j input)
                        input)))
    (when sanitized
      ;; History is now added by cl-readline:readline via :add-history t
      ;; (the old separate ADD-HISTORY call used a symbol that was never
      ;; fbound in cl-readline, causing an undefined-function error).
      (incf *command-count*)
      (push sanitized *command-history*))
    sanitized))

(defun %interrupt-action (now last-time window)
  "Determine interrupt action: :exit for double-press within WINDOW, :hint otherwise."
  (if (< (- now last-time) window)
      :exit
      :hint))

#+sbcl
(defun install-interrupt-handler (exit-fn)
  "Install SBCL-specific Ctrl+C handler using handler-bind around the REPL loop.
   EXIT-FN is called when a double Ctrl+C press is detected within 2 seconds.
   Returns a thunk that, when called, wraps BODY with the interrupt handler.

   Single Ctrl+C at idle: prints hint message and throws to catch wrapper.
   Double Ctrl+C within 2 seconds: calls EXIT-FN to leave the REPL."
  (lambda (body-thunk)
    (handler-bind
        ((sb-sys:interactive-interrupt
          (lambda (c)
            (declare (ignore c))
            (let ((now (get-internal-real-time))
                  (window (* 2 internal-time-units-per-second)))
              (ecase (%interrupt-action now *last-interrupt-time* window)
                (:exit (funcall exit-fn))
                (:hint
                 (setf *last-interrupt-time* now)
                 (format *standard-output*
                         "~%[Press Ctrl+C again to exit.]~%")
                 (force-output *standard-output*)
                 (throw 'idle-interrupt nil)))))))
      (funcall body-thunk))))

(defun %select-system-prompt (client explicit-prompt)
  "Choose the system prompt based on the client type and user preference.
When EXPLICIT-PROMPT is supplied and differs from the default, respect it.
Otherwise, use model-specific optimized prompt for Ollama models."
  (if (and (string= explicit-prompt sibyl.agent::*default-system-prompt*)
           (typep client 'sibyl.llm:ollama-client))
      (sibyl.agent::select-ollama-system-prompt
       (sibyl.llm:client-model client))
      explicit-prompt))

(defun start-repl (&key client
                     (system-prompt sibyl.agent::*default-system-prompt*)
                     (name "Sibyl")
                     session-id
                     (auto-save-interval 300))
  "Start the interactive REPL.

   Usage:
     (sibyl:with-config ()
       (sibyl:start-repl :client (sibyl:make-anthropic-client)))

   Or with an existing agent:
     (start-repl :client my-client)"
  
  (let* ((effective-prompt (%select-system-prompt client system-prompt))
         (agent (sibyl.agent:make-agent
                 :client client
                 :name name
                 :system-prompt effective-prompt)))
    (let ((model (and client (ignore-errors (sibyl.llm::client-model client)))))
      (if model
          (log-info "repl" "Starting REPL (model: ~a)" model)
          (log-info "repl" "Starting REPL")))
    (setf (sibyl.agent:agent-hooks agent)
          (list (cons :on-tool-call (make-tool-call-hook-v2))
                (cons :on-tool-result (make-tool-result-hook))
                (cons :before-step (make-before-step-hook))
                (cons :after-step (make-after-step-hook))
                (cons :on-error (make-on-error-hook))
                (cons :on-compact (make-compact-hook))))
    (let ((*ignore-ctrl-j*
           (not (null (sibyl.config:config-value "repl.ignore-ctrl-j" nil)))))
      ;; When *ignore-ctrl-j* is T, Ctrl+J is unbound from accept-line at the
      ;; readline level (see unbind-key call after %ensure-readline).
      ;; The post-processing %strip-ctrl-j in read-user-input remains as
      ;; defense-in-depth for the fallback read-line path (no cl-readline).
      ;;
      ;; Auto-load cl-readline for line-editing support (history, Emacs keys).
      ;; Must happen before %ensure-utf8-locale so readline sees the correct locale.
      (%ensure-readline)
      ;; Unbind Ctrl+J (linefeed) from accept-line when *ignore-ctrl-j* is T.
      (when (and *ignore-ctrl-j* (readline-available-p))
        (let ((result (funcall (find-symbol "UNBIND-KEY" :cl-readline)
                               (code-char 10))))
          (if result
              (log-warn "repl" "Failed to unbind Ctrl+J in readline")
              (log-info "repl" "Ctrl+J (linefeed) unbound from accept-line"))))
      ;; SBCL starts with LC_ALL="C" — fix locale before any readline use
      ;; so that mbrtowc/wcwidth correctly handle multibyte characters.
      (%ensure-utf8-locale))
    ;; Session initialization.
    (if session-id
        ;; Resume existing session.
        (multiple-value-bind (messages summary command-count)
            (load-session session-id)
          (if messages
              (let ((mem (sibyl.agent:agent-memory agent)))
                (dolist (msg messages)
                  (sibyl.llm:conversation-push
                   (sibyl.agent:memory-conversation mem) msg))
                (when summary
                  (setf (sibyl.agent:memory-summary mem) summary))
                (when command-count
                  (setf *command-count* command-count))
                (setf *current-session-id* session-id)
                (log-info "repl" "Resumed session ~a (~a messages)"
                          session-id (length messages)))
              (progn
                (format *error-output*
                        "~&Warning: session ~a not found, starting fresh~%"
                        session-id)
                (setf *current-session-id* (generate-session-id)))))
        ;; New session.
        (setf *current-session-id* (generate-session-id)))
    (print-banner)
    (format t "  Session: ~a~%" *current-session-id*)
    ;; Start auto-save timer
    (start-auto-save-timer
     (lambda ()
       (values *current-session-id*
               (sibyl.llm:conversation-to-list
                (sibyl.agent:memory-conversation
                 (sibyl.agent:agent-memory agent)))
               (sibyl.agent:memory-summary
                (sibyl.agent:agent-memory agent))
               *command-count*))
     auto-save-interval)
    ;; Pre-warm Ollama model — large models use blocking ensure-warm with
    ;; progress display; small models use lightweight background pre-warm.
    (when (typep client 'sibyl.llm:ollama-client)
      (let* ((ollama-client client)
             (profile (sibyl.llm::lookup-model-profile
                       (sibyl.llm:client-model ollama-client)))
             (large-p (getf profile :large-model)))
        (if large-p
            ;; Large model: block with progress reporting
            (progn
              (format t "~%[Loading model ~a into VRAM — this may take several minutes...]~%"
                      (sibyl.llm:client-model ollama-client))
              (bt:make-thread
               (lambda ()
                 (sibyl.llm::ollama-ensure-warm
                  ollama-client
                  :timeout (or (getf profile :load-timeout) 600)
                  :poll-interval 5
                  :on-progress (lambda (elapsed loaded-p)
                                 (if loaded-p
                                     (format t "[Model loaded (~,0fs)]~%" elapsed)
                                     (format t "." )))))
               :name "ollama-ensure-warm"))
            ;; Small model: lightweight background pre-warm
            (bt:make-thread
             (lambda ()
               (log-info "repl" "Pre-warming Ollama model ~a..."
                         (sibyl.llm:client-model ollama-client))
               (sibyl.llm::ollama-pre-warm ollama-client))
             :name "ollama-pre-warm"))))
    ;; Connect to configured MCP servers
    (handler-case
        (let ((count (sibyl.mcp:connect-configured-mcp-servers)))
          (when (plusp count)
            (format t "~%[Connected to ~a MCP server~:p]~%" count)))
      (error (e)
        (log-warn "repl" "MCP auto-connect failed: ~a" e)))
    ;; Load readline history if cl-readline is available
    (when (and (readline-available-p) (probe-file "~/.sibyl_history"))
      (funcall (find-symbol "READ-HISTORY" :cl-readline) "~/.sibyl_history"))
    (block repl-loop
      (setf *last-interrupt-time* 0)
      (labels ((save-history ()
                 (when (readline-available-p)
                   (funcall (find-symbol "WRITE-HISTORY" :cl-readline) "~/.sibyl_history")))
                 (exit-repl (&optional message)
                    (when message
                      (format t "~%~a~%" message))
                    (log-info "repl" "Shutting down REPL")
                    ;; Disconnect MCP servers
                    (handler-case (sibyl.mcp:disconnect-all-mcp-servers)
                      (error (e)
                        (log-debug "repl" "MCP disconnect error: ~a" e)))
                    (save-history)
                    ;; Stop auto-save timer before final save
                    (stop-auto-save-timer)
                    ;; Save session before exit.
                    (when *current-session-id*
                      (handler-case
                          (let* ((mem (sibyl.agent:agent-memory agent))
                                 (messages (sibyl.llm:conversation-to-list
                                            (sibyl.agent:memory-conversation mem)))
                                 (summary (sibyl.agent:memory-summary mem)))
                            (save-session *current-session-id* messages summary
                                          :command-count *command-count*)
                            (format t "~&Session saved: ~a~%" *current-session-id*)
                            (format t "Resume with: (start-repl :client ... :session-id ~s)~%"
                                    *current-session-id*))
                        (error (e)
                          (format *error-output*
                                  "~&Warning: failed to save session: ~a~%" e))))
                    (return-from repl-loop))
               (repl-body ()
                   (tagbody
                     next-iteration
                       (catch 'idle-interrupt
                         (let ((input (read-user-input)))
                           ;; EOF
                           (unless input
                             (exit-repl "Goodbye."))
                           ;; Empty input
                           (when (string= (string-trim '(#\Space #\Tab) input) "")
                             (go next-iteration))
                        ;; REPL command
                        (let ((cmd (repl-command-p input)))
                          (when cmd
                            (when (eq (handle-repl-command cmd agent input) :quit)
                              (exit-repl))
                            (go next-iteration)))
                        ;; Agent interaction
                        (let ((spinner nil)
                              (start-time nil))
                          (setf *cancel-requested* nil)
                          (flet ((stop-current-spinner ()
                                   ;; Stop the globally-tracked spinner — the tool-call hook
                                   ;; may have replaced the original with a new instance.
                                   (when *current-spinner*
                                     (sibyl.repl.spinner:stop-spinner *current-spinner*))
                                   ;; Also stop stale local reference if different
                                   (when (and spinner (not (eq spinner *current-spinner*)))
                                     (sibyl.repl.spinner:stop-spinner spinner))
                                   (setf spinner nil
                                         *current-spinner* nil)))
                            (unwind-protect
                                 (let ((result nil))
                                   (setf result
                                         (catch 'repl-cancelled
                                           (handler-bind
                                               #+sbcl ((sb-sys:interactive-interrupt
                                                         (lambda (c)
                                                           (declare (ignore c))
                                                           (let ((now (get-internal-real-time))
                                                                 (window (* 2 internal-time-units-per-second)))
                                                             (if (< (- now *last-interrupt-time*) window)
                                                                 (progn
                                                                   (stop-current-spinner)
                                                                   (exit-repl))
                                                                 (progn
                                                                   (setf *cancel-requested* t
                                                                         *last-interrupt-time* now)
                                                                   (throw 'repl-cancelled :cancelled)))))))
                                              (handler-case
                                                  (progn
                                                    (setf start-time (get-internal-real-time)
                                                          spinner (sibyl.repl.spinner:start-spinner "Thinking...")
                                                          *current-spinner* spinner
                                                          *thinking-output-active* nil)
                            (let* ((first-chunk-p t)
                                   (ollama-client-p (typep (sibyl.agent:agent-client agent)
                                                            'sibyl.llm:ollama-client))
                                   (stripper (when ollama-client-p
                                                (make-function-block-stripper)))
                                   (sibyl.llm:*streaming-text-callback*
                                     (when *stream-enabled*
                                      (lambda (text)
                                        (let ((spinner-was-active
                                                (and *current-spinner*
                                                     (sibyl.repl.spinner:spinner-active-p *current-spinner*))))
                                          ;; Stop any active spinner before writing streaming text.
                                          ;; Handles both the initial spinner AND spinners restarted
                                          ;; by the tool-call hook between LLM calls.
                                          (when spinner-was-active
                                            (stop-current-spinner)
                                            (format t "~%"))
                                          ;; Emit newline separator after thinking output.
                                          ;; When a spinner was active, its stop + ~% already provides
                                          ;; the line break; otherwise we need an explicit separator.
                                          (when *thinking-output-active*
                                            (unless spinner-was-active
                                              (format t "~%"))
                                            (setf *thinking-output-active* nil))
                                          (setf first-chunk-p nil)
                                          (let ((clean (if stripper
                                                           (funcall stripper text)
                                                           text)))
                                            (when (and clean (plusp (length clean)))
                                              (write-string clean *standard-output*)
                                              (force-output *standard-output*)))))))
                                    (sibyl.llm::*streaming-thinking-callback*
                                      (make-thinking-display-callback))
                                    (tracker (sibyl.agent:agent-token-tracker agent))
                                    (in-before (sibyl.llm::token-tracker-input-tokens tracker))
                                    (out-before (sibyl.llm::token-tracker-output-tokens tracker))
                                    (thinking-before (sibyl.llm::token-tracker-thinking-tokens tracker))
                                    ;; Cache telemetry snapshot before agent-run
                                    (cache-stats-before (sibyl.cache:get-cache-telemetry))
                                    (cache-hits-before (getf cache-stats-before :client-hits))
                                    (cache-total-before (getf cache-stats-before :total-requests))
                                   (response (sibyl.agent:agent-run agent input))
                                   (in-delta (- (sibyl.llm::token-tracker-input-tokens tracker) in-before))
                                   (out-delta (- (sibyl.llm::token-tracker-output-tokens tracker) out-before))
                                   ;; Cache telemetry delta
                                   (cache-stats-after (sibyl.cache:get-cache-telemetry))
                                   (cache-hits-delta (- (getf cache-stats-after :client-hits) cache-hits-before))
                                   (cache-total-delta (- (getf cache-stats-after :total-requests) cache-total-before)))
                              (stop-current-spinner)
                              (when (or (not *stream-enabled*)
                                        (not sibyl.llm:*streaming-text-callback*)
                                        first-chunk-p)
                                (format t "~%~a~%"
                                        (if ollama-client-p
                                            (strip-function-blocks response)
                                            response)))
                               (unless first-chunk-p
                                 (format t "~%"))
                               ;; Calculate footer metrics
                               (let* ((model-name (ignore-errors
                                                   (sibyl.llm::client-model
                                                    (sibyl.agent:agent-client agent))))
                                      (thinking-delta (- (sibyl.llm::token-tracker-thinking-tokens tracker)
                                                        thinking-before))
                                      ;; Calculate cost (nil for Ollama clients)
                                      (cost-result (when (and model-name
                                                             (not (typep (sibyl.agent:agent-client agent)
                                                                        'sibyl.llm:ollama-client)))
                                                    (sibyl.llm:estimate-cost-usd
                                                     model-name
                                                     :input-tokens in-delta
                                                     :output-tokens out-delta
                                                     :cache-read-tokens cache-hits-delta
                                                     :cache-write-tokens (- cache-total-delta cache-hits-delta))))
                                      (total-cost (when cost-result (getf cost-result :total)))
                                      ;; Calculate context percentage from conversation fill ratio
                                      ;; (using message count / max-messages avoids inflated values
                                      ;;  when compaction LLM calls are included in in-delta)
                                      (context-pct (let* ((mem (sibyl.agent:agent-memory agent))
                                                          (msgs (sibyl.llm:conversation-length
                                                                 (sibyl.agent:memory-conversation mem)))
                                                          (max-msgs (sibyl.agent:memory-max-messages mem)))
                                                     (if (plusp max-msgs)
                                                         (* 100.0 (/ (float msgs) max-msgs))
                                                         0.0))))
                                 ;; Accumulate cost in tracker
                                 (when total-cost
                                   (sibyl.llm:tracker-add-cost tracker total-cost))
                                 ;; Display footer
                                 (sibyl.repl.display:format-turn-footer
                                  :model model-name
                                  :elapsed-seconds (elapsed-seconds start-time)
                                  :input-tokens in-delta
                                  :output-tokens out-delta
                                  :thinking-tokens thinking-delta
                                  :cache-read-tokens cache-hits-delta
                                  :cache-write-tokens (- cache-total-delta cache-hits-delta)
                                  :context-percentage context-pct
                                  :cost-usd total-cost))
                               (format t "~%")))
                                                #+sbcl (sb-sys:interactive-interrupt (e)
                                                        (declare (ignore e))
                                                        (stop-current-spinner)
                                                        (sibyl.agent:memory-sanitize
                                                         (sibyl.agent:agent-memory agent))
                                                        (setf *cancel-requested* nil)
                                                        (format t "~%[Cancelled]~%~%"))
                                                 (sibyl.conditions:llm-error (e)
                                                   (stop-current-spinner)
                                                   (sibyl.agent:memory-sanitize
                                                    (sibyl.agent:agent-memory agent))
                                                   (log-error "repl" "LLM error: ~a" e)
                                                   (format t "~%[LLM Error: ~a]~%~%" e))
                                                 (error (e)
                                                   (stop-current-spinner)
                                                   (sibyl.agent:memory-sanitize
                                                    (sibyl.agent:agent-memory agent))
                                                   (log-error "repl" "Error: ~a" e)
                                                   (format t "~%[Error: ~a]~%~%" e))))))
                                    (when (eq result :cancelled)
                                      (stop-current-spinner)
                                      (sibyl.agent:memory-sanitize
                                       (sibyl.agent:agent-memory agent))
                                      (setf *cancel-requested* nil)
                                      (format t "~%[Cancelled]~%~%"))))
                               (stop-current-spinner)))))
                       (fresh-line)
                       (go next-iteration))))
        #+sbcl
        (let ((wrapper (install-interrupt-handler (lambda () (exit-repl)))))
          (funcall wrapper #'repl-body))
        #-sbcl
        (repl-body)))
    ;; After the REPL loop exits (via /quit, Ctrl+D, or double Ctrl+C),
    ;; terminate the SBCL process so the user returns to the shell.
    #+sbcl (sb-ext:exit :code 0)
    #-sbcl (uiop:quit 0)))
