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
    ("/tools"         . :list-tools)
    ("/help"          . :help)
    ("/history"       . :history)
    ("/mcp"           . :mcp)
    ("/plan"          . :plan)
    ("/improve"       . :improve)
    ("/review"        . :review)
    ("/evolve"        . :evolve)
    ("/test-parallel" . :test-parallel)
    ("/tokens"        . :tokens))
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
   Lock order: tool-registry (1st) < evolution-state (2nd) < modified-files (3rd) < command-handlers (4th)")

(defvar *use-colors* t
  "Whether to use colored output in the REPL. Defaults to t (colors enabled).")

(defvar *stream-enabled* t
  "When T, LLM responses are streamed token-by-token to the terminal.")

(defvar *ignore-ctrl-j* nil
  "When T, strip Ctrl+J (linefeed) characters from REPL input.")

(defvar *command-count* 0
  "Counter tracking the number of commands executed in the current REPL session.")

(defvar *command-history* nil
  "List of command strings entered in the current REPL session, in reverse chronological order.")

(defvar *cancel-requested* nil
  "When non-NIL, the current LLM call should be cancelled.
   Set to T by the Ctrl+C handler (single press); reset to NIL before each agent-run call.")

(defvar *current-spinner* nil
  "Currently active spinner for LLM calls, or NIL when idle.")

(defvar *spinner-output-lock* (bt:make-lock "spinner-output")
  "Lock serializing spinner stop/start and terminal output in the tool-call hook.
   Prevents garbled escape sequences when parallel tool calls each fire the hook
   from separate worker threads.")

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
        (format-colored-text "  /evolve" :green)
        (format t "        — Autonomous continuous improvement loop~%")
        (format-colored-text "  /test-parallel" :green)
        (format t "  — Run test suite in parallel mode~%")
        (format-colored-text "  /tokens" :green)
        (format t "         — Show cumulative token usage statistics~%")
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
        (format t "  /evolve          — Autonomous continuous improvement loop~%")
        (format t "  /test-parallel   — Run test suite in parallel mode~%")
        (format t "  /tokens          — Show cumulative token usage statistics~%")
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

(defun handle-evolve-command-wrapper (agent input)
  "Wrapper handler for :evolve command."
  (handle-evolve-command agent input)
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

(defun handle-test-parallel-command (agent input)
  "Handler for /test-parallel command. Runs test suite using parallel runner.
   Uses uiop:symbol-call for late binding since sibyl.tests is only available
   when sibyl/tests system is loaded (not at repl.lisp compile time)."
  (declare (ignore agent input))
  (format t "~%Running tests in parallel mode...~%~%")
  (handler-case
      (let* ((start (get-internal-real-time))
             (results (uiop:symbol-call '#:sibyl.tests '#:run-tests-parallel))
             (elapsed (/ (float (- (get-internal-real-time) start))
                         internal-time-units-per-second)))
        (format t "~%Parallel test run complete. ~a total checks in ~,2fs.~%" (length results) elapsed))
    (error (e)
      (format t "~%Test run error: ~a~%" e)))
  nil)

(defun format-token-usage (tracker)
  "Format token usage statistics for display. Returns a string."
  (let* ((input      (sibyl.llm::token-tracker-input-tokens tracker))
         (output     (sibyl.llm::token-tracker-output-tokens tracker))
         (cache-read (sibyl.llm::token-tracker-cache-read-tokens tracker))
         (cache-write (sibyl.llm::token-tracker-cache-write-tokens tracker))
         (requests   (sibyl.llm::token-tracker-request-count tracker))
         (hit-rate   (sibyl.llm::tracker-cache-hit-rate tracker)))
    (format nil "~%Token Usage (this session):~%  Input:       ~:d tokens~%  Output:      ~:d tokens~%  Cache Read:  ~:d tokens (~,1f% hit rate)~%  Cache Write: ~:d tokens~%  Requests:    ~:d~%"
            input output cache-read (* hit-rate 100.0) cache-write requests)))

(defun handle-tokens-command (agent input)
  "Handler for :tokens command. Displays cumulative token usage statistics."
  (declare (ignore input))
  (let* ((tracker (sibyl.agent:agent-token-tracker agent))
         (usage-str (format-token-usage tracker)))
    (format t "~a" usage-str))
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
        (cons :evolve  #'handle-evolve-command-wrapper)
        (cons :test-parallel #'handle-test-parallel-command)
        (cons :tokens        #'handle-tokens-command))
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

(defun %evolve-normalize-priority (value)
  (when value
    (let ((normalized (string-downcase (string-trim '(#\Space #\Tab) value))))
      (when (member normalized '("high" "medium" "low") :test #'string=)
        normalized))))

(defun parse-evolve-args (input)
  "Parse /evolve command arguments. Returns (values max-cycles priority-threshold).
   Examples:
   - '/evolve' => (10 "medium")
   - '/evolve 5 high' => (5 "high")
   - '/evolve max-cycles=3 priority=low' => (3 "low")"
  (let* ((trimmed (string-trim '(#\Space #\Tab) (or input "")))
         (args-str (if (search "/evolve" trimmed :test #'string-equal)
                       (string-trim '(#\Space #\Tab)
                                    (subseq trimmed (length "/evolve")))
                       trimmed))
         (parts (if (string= args-str "")
                    nil
                    (cl-ppcre:split "\\s+" args-str)))
         (max-cycles nil)
         (priority nil))
    (dolist (part parts)
      (let* ((clean (string-trim '(#\Space #\Tab) part))
             (token (string-left-trim '(#\-) clean)))
        (cond
          ((search "=" token)
           (let* ((pair (cl-ppcre:split "=" token :limit 2))
                  (key (string-downcase (first pair)))
                  (value (second pair)))
             (cond
               ((member key '("max-cycles" "cycles" "max") :test #'string=)
                (let ((parsed (ignore-errors (parse-integer value :junk-allowed t))))
                  (when parsed
                    (setf max-cycles parsed))))
               ((member key '("priority" "priority-threshold" "min-priority" "threshold")
                        :test #'string=)
                (let ((parsed (%evolve-normalize-priority value)))
                  (when parsed
                    (setf priority parsed)))))))
          (t
           (let ((parsed-int (ignore-errors (parse-integer token :junk-allowed t)))
                 (parsed-priority (%evolve-normalize-priority token)))
             (when (and parsed-int (null max-cycles))
               (setf max-cycles parsed-int))
             (when (and parsed-priority (null priority))
               (setf priority parsed-priority)))))))
    (values (max 0 (or max-cycles 10))
            (or priority "medium"))))

(defun %evolve-parse-json (json)
  (handler-case
      (yason:parse json :object-as :hash-table)
    (error () nil)))

(defun %evolve-ensure-list (value)
  (cond
    ((vectorp value) (coerce value 'list))
    ((null value) nil)
    (t value)))

(defun %evolve-format-location (file line)
  (cond
    ((and file (integerp line))
     (format nil "~a:~a" file line))
    (file file)
    (t "unknown")))

(defun %evolve-build-prompt (suggestion)
  (let* ((description (or (gethash "description" suggestion) ""))
         (rationale (or (gethash "rationale" suggestion) ""))
         (priority (or (gethash "priority" suggestion) "medium"))
         (category (or (gethash "category" suggestion) "unknown"))
         (file (gethash "file" suggestion))
         (line (gethash "line" suggestion))
         (location (%evolve-format-location file line)))
    (format nil
            "Improve Sibyl autonomously based on this suggestion:~%~%~
             Description: ~a~%~
             Rationale: ~a~%~
             Priority: ~a~%~
             Category: ~a~%~
             Location: ~a~%~%~
             Follow the TDD workflow strictly:~%~
             1. UNDERSTAND: use read-sexp and who-calls to inspect current code~%~
             2. RED: write a failing test using write-test; confirm with run-tests~%~
             3. GREEN: implement using safe-redefine; re-run run-tests~%~
             4. REFACTOR if needed; re-run run-tests~%~
             5. PERSIST: use sync-to-file to write changes to disk and unprotect file~%~%~
             Constraints: stay within sibyl.* packages, compile new code, do not modify system prompt."
            description rationale priority category location)))

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

(defun handle-evolve-command (agent original-input)
  "Handle /evolve command for autonomous continuous improvement.
   Usage: /evolve [max-cycles] [priority]
          /evolve max-cycles=5 priority=high"
  (multiple-value-bind (max-cycles priority-threshold)
      (parse-evolve-args original-input)
    (let ((cycles-run 0)
          (productive-cycles 0)
          (total-succeeded 0)
          (total-skipped 0)
          (consecutive-skip-cycles 0)
          (baseline-tests 0)
          (current-tests 0)
          (baseline-tools (length (sibyl.tools:list-tools)))
          (current-tools (length (sibyl.tools:list-tools))))
      (sibyl.tools:evolution-state-init)
      (when (<= max-cycles 0)
        (sibyl.tools:evolution-report-final-summary 0 0 0 0 baseline-tests current-tests)
        (return-from handle-evolve-command))
      (block evolve-loop
        (loop for cycle from 1 to max-cycles do
          (setf cycles-run cycle)
          (setf current-tools (length (sibyl.tools:list-tools)))
          (sibyl.tools:evolution-report-cycle-start cycle max-cycles)
          (let* ((suggestions-json
                   (sibyl.tools:execute-tool
                    "suggest-improvements"
                    (list (cons "min-priority" priority-threshold)
                          (cons "exclude-attempted" "true"))))
                 (parsed (%evolve-parse-json suggestions-json))
                 (suggestions (%evolve-ensure-list
                               (and parsed (gethash "suggestions" parsed))))
                 (suggestion-count (length suggestions)))
            (when (or (null suggestions) (zerop suggestion-count))
              (format t "~%No new suggestions. Stopping.~%")
              (return-from evolve-loop))
            (incf productive-cycles)
            (let ((cycle-succeeded 0)
                  (cycle-skipped 0))
              (loop for suggestion in suggestions
                    for index from 1 do
                      (let* ((name (or (gethash "description" suggestion) "(unnamed)"))
                             (prompt (%evolve-build-prompt suggestion)))
                        (sibyl.tools:evolution-report-improvement-start index suggestion-count name)
                        (handler-case
                            (let ((response (sibyl.agent:agent-run agent prompt)))
                              (sibyl.tools:evolution-state-record-attempt name "success")
                              (incf cycle-succeeded)
                              (incf total-succeeded)
                              (sibyl.tools:evolution-report-improvement-result t)
                              (when response
                                (format t "~%~a~%" response)))
                          (sibyl.conditions:llm-error (e)
                            (sibyl.tools:evolution-state-record-attempt name (format nil "failed: ~a" e))
                            (incf cycle-skipped)
                            (incf total-skipped)
                            (format t "~%[LLM Error: ~a]~%" e)
                            (sibyl.tools:evolution-report-improvement-result nil))
                          (error (e)
                            (sibyl.tools:evolution-state-record-attempt name (format nil "failed: ~a" e))
                            (incf cycle-skipped)
                            (incf total-skipped)
                            (format t "~%[Error: ~a]~%" e)
                            (sibyl.tools:evolution-report-improvement-result nil)))))
              (let* ((tests-json (sibyl.tools:execute-tool "run-tests" nil))
                     (tests-parsed (%evolve-parse-json tests-json))
                     (failed (or (and tests-parsed (gethash "failed" tests-parsed)) 0))
                     (total (or (and tests-parsed (gethash "total" tests-parsed)) current-tests)))
                (setf current-tests total)
                (setf current-tools (length (sibyl.tools:list-tools)))
                (when (and (integerp failed) (> failed 0))
                  (format t "~%Test regression detected. Stopping.~%")
                  (sibyl.tools:evolution-report-cycle-summary
                   cycle-succeeded cycle-skipped baseline-tests current-tests baseline-tools current-tools)
                  (return-from evolve-loop))
                (sibyl.tools:evolution-report-cycle-summary
                 cycle-succeeded cycle-skipped baseline-tests current-tests baseline-tools current-tools)
                (setf baseline-tests current-tests)
                (setf baseline-tools current-tools)
                (if (zerop cycle-succeeded)
                    (incf consecutive-skip-cycles)
                    (setf consecutive-skip-cycles 0))
                (when (>= consecutive-skip-cycles 3)
                  (format t "~%Three consecutive skip cycles. Stopping.~%")
                  (return-from evolve-loop)))))))
      (sibyl.tools:evolution-report-final-summary
       cycles-run productive-cycles total-succeeded total-skipped baseline-tests current-tests)
      nil)))

;;; ============================================================
;;; Hook functions
;;; ============================================================

(defun make-tool-call-hook (&optional spinner)
  "Return a closure suitable for the :on-tool-call agent hook.
   The closure accepts a TOOL-CALL struct and displays the tool name in cyan,
   followed by a brief summary of the primary argument(s).

   If a spinner is active, it is stopped first so the message is not erased
   by the spinner's line-clear escape sequence.  After displaying the tool info,
   a fresh spinner is started with a '考え中...' message to provide visual
   feedback during tool execution.

   Usage:
     (cons :on-tool-call (make-tool-call-hook spinner))"
  (lambda (tc)
    (let* ((tool-name (sibyl.llm:tool-call-name tc))
           (summary (format-tool-call-summary tc))
           (display
            (if (string= summary "")
                (format nil "🔧 ~a を実行中..." tool-name)
                (format nil "🔧 ~a を実行中... ~a" tool-name summary))))
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
          ;; Restart spinner to give visual feedback during tool execution.
          ;; The spinner runs until the next LLM streaming chunk arrives
          ;; (which stops it via the streaming callback).
          (let ((new-spinner (sibyl.repl.spinner:start-spinner "考え中...")))
            (setf *current-spinner* new-spinner)))))))

(defun format-tool-call-summary (tc &key (max-length 50))
  "ツールコールの引数を人間が読みやすい短い文字列に整形する。
   例: shell + ((command . \"ls -la\")) → \"(ls -la)\"
       eval-form + ((form . \"(defun foo () 42)\")) → \"(defun foo () 42)\"
   引数が長い場合は max-length 文字で切り詰める。"
  (let* ((args (sibyl.llm:tool-call-arguments tc))
         (tool-name (sibyl.llm:tool-call-name tc))
         ;; ツールごとの主要引数キー（優先順位順）
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
         ;; 主要引数の値を取得
         (values-to-show
           (if primary-keys
               (remove nil
                 (mapcar (lambda (k)
                           (let ((pair (assoc k args :test #'string=)))
                             (when pair
                               (let ((v (cdr pair)))
                                 (if (stringp v) v (format nil "~a" v))))))
                         primary-keys))
               ;; フォールバック: 最初の引数の値
               (when args
                 (let ((v (cdr (first args))))
                   (list (if (stringp v) v (format nil "~a" v)))))))
         ;; 値を結合
         (summary (if values-to-show
                      (format nil "~{~a~^ ~}" values-to-show)
                      "")))
    (cond
      ((string= summary "") "")
      ;; 値が既に括弧で始まる場合（Lispフォームなど）はそのまま表示
      ((char= (char summary 0) #\()
       (if (> (length summary) max-length)
           (format nil "~a..." (subseq summary 0 max-length))
           summary))
      ;; 通常は括弧で囲む
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

;;; ============================================================
;;; Elapsed time utilities
;;; ============================================================

(defun elapsed-seconds (start-time)
  "Compute elapsed seconds since START-TIME (from get-internal-real-time)."
  (/ (- (get-internal-real-time) start-time) 
     (float internal-time-units-per-second)))

(defun format-elapsed-time (seconds &key (stream *standard-output*) model tokens input-tokens output-tokens)
  "Format elapsed time as [model · elapsed: X.Xs · In X / Out Y] with optional dim styling.
   When MODEL is non-NIL, includes the model name before the elapsed time.
   When INPUT-TOKENS and OUTPUT-TOKENS are non-NIL, appends 'In X / Out Y'.
   TOKENS (legacy scalar) is still accepted as fallback.
   When *use-colors* is nil, outputs plain text. Otherwise uses ANSI dim code."
  (let ((label
         (with-output-to-string (s)
           (when model (format s "~a · " model))
           (format s "elapsed: ~,1fs" seconds)
           (cond
             ((and input-tokens output-tokens)
              (format s " · In ~:d / Out ~:d" input-tokens output-tokens))
             (tokens
              (format s " · ~:d tok" tokens))))))
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

#+sbcl
(defun install-interrupt-handler (exit-fn)
  "Install SBCL-specific Ctrl+C handler using handler-bind around the REPL loop.
   EXIT-FN is called when a double Ctrl+C press is detected within 2 seconds.
   Returns a thunk that, when called, wraps BODY with the interrupt handler.

   Single Ctrl+C: sets *CANCEL-REQUESTED* to T (for cooperative cancellation).
   Double Ctrl+C within 2 seconds: calls EXIT-FN to leave the REPL.

   NOTE: This function is defined for SBCL only (#+sbcl guard).
   On non-SBCL implementations the REPL loop omits interrupt handling gracefully."
  (lambda (body-thunk)
    (handler-bind
        ((sb-sys:interactive-interrupt
          (lambda (c)
            (declare (ignore c))
            (let ((now (get-internal-real-time))
                  (window (* 2 internal-time-units-per-second)))
              (if (< (- now *last-interrupt-time*) window)
                  ;; Double press — exit REPL
                  (funcall exit-fn)
                  ;; Single press — request cancellation
                  (progn
                    (setf *cancel-requested* t
                          *last-interrupt-time* now)
                    (format *standard-output*
                            "~%[^C: LLM call cancelled. Press Ctrl+C again to exit.]~%")
                    (force-output *standard-output*))))
            (invoke-restart 'continue))))
      (funcall body-thunk))))

(defun start-repl (&key client
                     (system-prompt sibyl.agent::*default-system-prompt*)
                     (name "Sibyl")
                     (use-model-selector nil))
  "Start the interactive REPL.

   Usage:
     (sibyl:with-config ()
       (sibyl:start-repl :client (sibyl:make-anthropic-client)))

   With adaptive model selection:
     (sibyl:start-repl :client (sibyl:make-anthropic-client) :use-model-selector t)

   Or with an existing agent:
     (start-repl :client my-client)"
  (let ((agent (if (or use-model-selector
                       (sibyl.config:config-value "optimization.auto-model-routing"))
                   (make-instance 'sibyl.llm::adaptive-agent
                                  :client client
                                  :name name
                                  :system-prompt system-prompt
                                  :model-selector (sibyl.llm::make-default-model-selector))
                   (sibyl.agent:make-agent
                    :client client
                    :name name
                    :system-prompt system-prompt))))
    (let ((model (and client (ignore-errors (sibyl.llm::client-model client)))))
      (if model
          (log-info "repl" "Starting REPL (model: ~a)" model)
          (log-info "repl" "Starting REPL")))
    (setf (sibyl.agent:agent-hooks agent)
          (list (cons :on-tool-call (make-tool-call-hook))
                (cons :before-step (make-before-step-hook))
                (cons :after-step (make-after-step-hook))
                (cons :on-error (make-on-error-hook))))
    (setf *ignore-ctrl-j*
          (not (null (sibyl.config:config-value "repl.ignore-ctrl-j" nil))))
    ;; Ctrl+J filtering is handled at the application level by %strip-ctrl-j
    ;; in read-user-input.  Kernel-level termios manipulation (INLCR+IGNCR)
    ;; was removed because it also suppresses normal Enter (CR), making the
    ;; REPL unusable.
    ;; Auto-load cl-readline for line-editing support (history, Emacs keys).
    ;; Must happen before %ensure-utf8-locale so readline sees the correct locale.
    (%ensure-readline)
    ;; SBCL starts with LC_ALL="C" — fix locale before any readline use
    ;; so that mbrtowc/wcwidth correctly handle multibyte characters.
    (%ensure-utf8-locale)
    (print-banner)
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
                   (return-from repl-loop))
               (repl-body ()
                   (tagbody
                    next-iteration
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
                                                         spinner (sibyl.repl.spinner:start-spinner "考え中...")
                                                         *current-spinner* spinner)
                            (let* ((first-chunk-p t)
                                   (sibyl.llm:*streaming-text-callback*
                                     (when *stream-enabled*
                                      (lambda (text)
                                          ;; Stop any active spinner before writing streaming text.
                                          ;; Handles both the initial spinner AND spinners restarted
                                          ;; by the tool-call hook between LLM calls.
                                          (when (and *current-spinner*
                                                     (sibyl.repl.spinner:spinner-active-p *current-spinner*))
                                            (stop-current-spinner)
                                            (format t "~%"))
                                          (setf first-chunk-p nil)
                                          (write-string text *standard-output*)
                                          (force-output *standard-output*))))
                                   (tracker (sibyl.agent:agent-token-tracker agent))
                                   (in-before (sibyl.llm::token-tracker-input-tokens tracker))
                                   (out-before (sibyl.llm::token-tracker-output-tokens tracker))
                                   (response (sibyl.agent:agent-run agent input))
                                   (in-delta (- (sibyl.llm::token-tracker-input-tokens tracker) in-before))
                                   (out-delta (- (sibyl.llm::token-tracker-output-tokens tracker) out-before)))
                              (stop-current-spinner)
                              (when (or (not *stream-enabled*)
                                        (not sibyl.llm:*streaming-text-callback*)
                                        first-chunk-p)
                                (format t "~%~a~%" response))
                              (unless first-chunk-p
                                (format t "~%"))
                              (format-elapsed-time (elapsed-seconds start-time)
                                                   :model (ignore-errors
                                                            (sibyl.llm::client-model
                                                             (sibyl.agent:agent-client agent)))
                                                   :input-tokens (when (plusp in-delta) in-delta)
                                                   :output-tokens (when (plusp out-delta) out-delta))
                              (format t "~%")))
                                               #+sbcl (sb-sys:interactive-interrupt (e)
                                                       (declare (ignore e))
                                                       (stop-current-spinner)
                                                       (setf *cancel-requested* nil)
                                                       (format t "~%[Cancelled]~%~%"))
                                                (sibyl.conditions:llm-error (e)
                                                  (stop-current-spinner)
                                                  (log-error "repl" "LLM error: ~a" e)
                                                  (format t "~%[LLM Error: ~a]~%~%" e))
                                                (error (e)
                                                  (stop-current-spinner)
                                                  (log-error "repl" "Error: ~a" e)
                                                  (format t "~%[Error: ~a]~%~%" e))))))
                                   (when (eq result :cancelled)
                                     (stop-current-spinner)
                                     (setf *cancel-requested* nil)
                                     (format t "~%[Cancelled]~%~%"))))
                              (stop-current-spinner))))
                      (go next-iteration))))
        #+sbcl
        (let ((wrapper (install-interrupt-handler (lambda () (exit-repl)))))
          (funcall wrapper #'repl-body))
        #-sbcl
        (repl-body)))))
