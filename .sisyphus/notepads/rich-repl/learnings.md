# Rich REPL — Learnings

## [2026-02-17] Session ses_3954c162bffeQlfSjVgjT9e4lr

### Critical Architecture Facts

#### Hook Wiring (IMPORTANT)
- `make-agent` does NOT accept `:hooks` parameter even though `agent` class has `:hooks` slot
- Correct pattern: `(setf (sibyl.agent:agent-hooks agent) '((:on-tool-call . fn) ...))`
- `agent-hooks` accessor is public — use it directly
- DO NOT try to pass `:hooks` to `make-agent` — it will be silently ignored or error

#### Streaming Call Path (IMPORTANT)  
- `agent-step` calls `complete`/`complete-with-tools` without any streaming options
- Dynamic variable `*streaming-text-callback*` in `sibyl.llm` is the mechanism
- REPL binds it around `agent-run` → propagates via dynamic binding to provider methods
- Provider checks: `(when sibyl.llm:*streaming-text-callback* ...)` to decide streaming vs blocking
- This works because `agent-run → agent-step → complete-with-tools` all run in same thread

#### defvar Bug (PREREQUISITE)
- `*use-colors*`, `*command-count*`, `*command-history*` missing `defvar` in repl.lisp
- `(incf *command-count*)` will CRASH without defvar because NIL + 1 = error
- Must fix FIRST before any other work

#### SBCL Interrupt Handling
- Use `sb-sys:interactive-interrupt` — NOT caught by `(error ...)` handler
- Flag-based cooperative cancellation: set `*cancel-requested*` in handler, check after `agent-run`
- Use `#+sbcl` guards for portability

#### Terminal Ownership Protocol
- Spinner and cl-readline MUST NOT be active simultaneously  
- Order: readline reads → STOP readline → start spinner → LLM completes → stop spinner → readline activates
- Spinner captures `*standard-output*` at creation time for thread safety

### File Locations
- Spinner: `src/repl/spinner.lisp` (new — currently doesn't exist)
- Main REPL: `src/repl.lisp` (764 lines, will grow)
- Agent core: `src/agent/core.lisp` — DO NOT MODIFY
- HTTP client: `src/llm/client.lisp` (122 lines)
- Providers: `src/llm/providers.lisp`
- Conditions: `src/conditions.lisp` (112 lines)
- Test (unregistered): `tests/rich-repl-test.lisp` (31 lines)

### Key Line References
- `repl.lisp:709-720` — `read-user-input` (readline replaces line 716 only)
- `repl.lisp:722-763` — `start-repl` main loop (integration target)
- `repl.lisp:733-736` — `make-agent` call (add `setf (agent-hooks ...)` after)
- `repl.lisp:756-762` — `agent-run` call site (wrap with spinner/timing/interrupt)
- `core.lisp:10-38` — `agent` class definition (hooks slot at lines 33-37)
- `core.lisp:104-115` — `make-agent` function (does NOT pass :hooks to make-instance)
- `core.lisp:121-128` — `run-hook` function
- `core.lisp:142` — `:before-step` hook fires here
- `core.lisp:165` — `:on-tool-call` hook fires here with `tc` (tool-call struct)
- `core.lisp:182` — `:after-step` hook fires here
- `client.lisp:85-108` — `http-post-json` (don't modify, add `http-post-stream` alongside)
- `conditions.lisp:28-58` — `llm-error` hierarchy (add `llm-cancelled`, `llm-stream-error` here)
- `sibyl.asd:41` — `(:file "repl")` entry (replace with `(:module "repl" ...)` for spinner)
- `sibyl.asd:44-61` — test system (add `rich-repl-test` here)

## [2026-02-17] Spinner Module — Implementation Notes

### ASDF Duplicate Name Pitfall
- `(:module "repl" ...)` and `(:file "repl")` in the SAME parent module both produce a component
  named "repl" → ASDF errors: "multiple components are given same name 'repl'"
- Fix: give the sub-module a different name + explicit `:pathname`:
  ```lisp
  (:module "repl-module" :pathname "repl" :components ((:file "spinner")))
  (:file "repl")
  ```

### `defstruct` Lock Slot Initializer
- Default forms in `defstruct` ARE re-evaluated for each `make-<struct>` call (unlike `defclass`).
- So `(lock (bt:make-recursive-lock "spinner-lock"))` in `defstruct` is safe — each instance gets its own lock.

### Thread Dynamic Bindings
- `*standard-output*` is thread-local in SBCL — capture it in the calling thread BEFORE spawning.
- The pattern `(let* ((out *standard-output*) ...) (bt:make-thread (lambda () ... out ...)))` is correct.

### Responsive Stop with Short Sleeps
- Instead of one `(sleep 0.1)`, use `(loop repeat 2 while (not stop-flag) do (sleep 0.05))`.
- This checks the flag every 50ms, keeping stop latency ≤100ms while maintaining ~100ms frame rate.

### unwind-protect for Terminal Cleanup
- `unwind-protect` in the thread body guarantees the "clear line" escape code runs even on error.
- Duplicate clear in `stop-spinner` (after waiting for thread) is harmless belt-and-suspenders.

### Verification Confirmed
- `sbcl ... --eval '(ql:quickload :sibyl :silent t)'` → loads cleanly
- Spinner runs for 1.5s, `spinner-active-p` returns T, then stop clears line and returns NIL → SPINNER-OK

### Quicklisp Dependencies
- `bordeaux-threads` — already in depends-on
- `cl-readline` — MUST be optional (separate `sibyl/readline` subsystem)
- `flexi-streams` — may be needed for Phase 2 streaming (dexador may pull it in)

### Test Status
- `(asdf:test-system :sibyl)` runs suite `:sibyl-tests`
- Current test count: ~1094 checks (from parallel-tests completion)
- `rich-repl-test.lisp` exists but NOT registered in sibyl.asd

## [2026-02-17] cl-readline Optional Integration

### Critical: Compile-Time Package Resolution
- Directly using `cl-readline:foo` in source causes COMPILE-FILE-ERROR when cl-readline is not loaded
- Even inside `(when (readline-available-p) ...)` — the package-qualified symbol is resolved at **read time**
- Fix: use `(funcall (find-symbol "READLINE" :cl-readline) :prompt prompt)` — purely runtime dispatch
- `find-symbol` on a non-existent package returns NIL; since we guard with `(readline-available-p)` first, this is safe

### Pattern Used
```lisp
;; Check
(defun readline-available-p ()
  (not (null (find-package :cl-readline))))

;; Call (only inside (when (readline-available-p) ...) guards)
(funcall (find-symbol "READLINE" :cl-readline) :prompt prompt)
(funcall (find-symbol "ADD-HISTORY" :cl-readline) input)
(funcall (find-symbol "READ-HISTORY" :cl-readline) "~/.sibyl_history")
(funcall (find-symbol "WRITE-HISTORY" :cl-readline) "~/.sibyl_history")
```

### sibyl/readline Subsystem
- Defined in `sibyl.asd` as a SEPARATE defsystem — never add cl-readline to main `:depends-on`
- `:components ()` is valid (empty component list) — just pulls in both systems as dependencies
- Loading `sibyl/readline` makes `readline-available-p` return T; main `:sibyl` stays independent

### History File
- Path: `"~/.sibyl_history"` (string — SBCL expands `~` in probe-file and write-history)
- Load on REPL start: inside `start-repl` after `(print-banner)`
- Save on REPL exit: both EOF path and `:quit` command path

### Verification Command
```bash
sbcl --eval '(ql:quickload :sibyl :silent t)' \
     --eval '(assert (not (sibyl.repl::readline-available-p)))' \
     --eval '(with-input-from-string (*standard-input* "hello") (assert (string= "hello" (sibyl.repl::read-user-input))))' \
     --eval '(format t "READLINE-FALLBACK-OK~%")' --quit
```
→ Outputs `READLINE-FALLBACK-OK` ✓

## [2026-02-17] Task 6 — Tool Call Hook Factory Functions

### tool-call accessor: `tool-call-name` (CONFIRMED)
- Accessor: `sibyl.llm:tool-call-name` (exported, single colon)
- `tool-call-id`: id accessor
- `tool-call-arguments`: arguments accessor
- All three exported from `sibyl.llm` package (see packages.lisp:74-78)

### CRITICAL: tool-call changed from defstruct to defclass
- Original: `(defstruct (tool-call (:constructor make-tool-call)) ...)`
- Changed to `defclass` in `src/llm/message.lisp` to support `make-instance` with initargs
- Reason: SBCL `defstruct` does NOT register slot initargs for `make-instance` —
  calling `(make-instance 'tool-call :name ...)` fails with "Invalid initialization arguments"
- Fix: `(defclass tool-call () ((id :initarg :id ...) (name :initarg :name ...) ...))`
- `make-tool-call` function preserved: `(defun make-tool-call (&key id name arguments) ...)`
- All existing callers unaffected (providers.lisp, tests still pass 1094/1094)
- After FASL change: clean cache with `find ~/.cache/common-lisp -name "*.fasl" -path "*/sibyl/*" -delete`

### Hook factory functions added to src/repl.lisp
- Section: "Hook functions" added before "Main REPL loop" (around line 693)
- `make-tool-call-hook (&optional spinner)` — returns closure for `:on-tool-call`
- `make-before-step-hook (&optional spinner)` — no-op stub for `:before-step`
- `make-after-step-hook (&optional spinner)` — no-op stub for `:after-step`
- Display format: `🔧 <tool-name> を実行中...` in cyan (or plain when `*use-colors*` nil)
- Uses `sibyl.llm:tool-call-name` (single colon — symbol is exported)
- Spinner update: `(sibyl.repl.spinner:update-spinner-message spinner (format nil "🔧 ~a" tool-name))`

### sibyl.repl package does NOT use sibyl.llm
- `sibyl.repl` uses `#:sibyl.agent #:sibyl.config` only
- Must use fully qualified `sibyl.llm:tool-call-name` (NOT just `tool-call-name`)

## [2026-02-17] Task 4 — Ctrl+C Cancellation Variables & llm-cancelled Condition

### What Was Done
- Added `llm-cancelled` condition to `src/conditions.lisp` as a direct subtype of `llm-error`
  - No extra slots; report: `"LLM call cancelled: <message>"`
- `#:llm-cancelled` already in `sibyl.conditions` exports in HEAD; committed in conditions.lisp
- `*cancel-requested*` (nil) and `*last-interrupt-time*` (0) defvars already in HEAD repl.lisp
- `install-interrupt-handler` with `#+sbcl` guard already in HEAD repl.lisp

### Handler Pattern (#+sbcl) — returns a thunk-wrapper, not a direct installer
```lisp
#+sbcl
(defun install-interrupt-handler (exit-fn)
  (lambda (body-thunk)
    (handler-bind ((sb-sys:interactive-interrupt
                    (lambda (c) (declare (ignore c))
                      (let ((now (get-internal-real-time))
                            (window (* 2 internal-time-units-per-second)))
                        (if (< (- now *last-interrupt-time*) window)
                            (funcall exit-fn)
                            (progn (setf *cancel-requested* t *last-interrupt-time* now)
                                   (format *standard-output* "~%[^C: ...]~%")
                                   (force-output *standard-output*))))
                      (invoke-restart 'continue))))
      (funcall body-thunk))))
```

### Key SBCL Facts
- `sb-sys:interactive-interrupt` is the Ctrl+C condition (NOT a subtype of `error`)
- `handler-bind` safer than `sb-sys:enable-interrupt` — avoids lock corruption
- Must `(invoke-restart 'continue)` to avoid stack unwinding
- Function returns a wrapper thunk; Task 7 wraps the REPL loop with it

### State Discovery
- Previous session had already committed repl.lisp/packages.lisp changes to HEAD
- Only `conditions.lisp` needed a new commit for `llm-cancelled`
- ASDF stale cache caused false "CL-READLINE package does not exist" on first run
  - Fix: `rm -rf ~/.cache/common-lisp/sbcl-*/path/to/sibyl/`

### Verification
```
sbcl --eval '(ql:quickload :sibyl :silent t)' \
     --eval '(assert (not sibyl.repl::*cancel-requested*))' \
     --eval '(assert (= 0 sibyl.repl::*last-interrupt-time*))' \
     --eval '(assert (typep (make-condition (quote sibyl.conditions:llm-cancelled) :message "test") (quote sibyl.conditions:llm-error)))' \
     --eval '(format t "CTRLC-OK~%")' --quit
→ CTRLC-OK ✓
(asdf:test-system :sibyl) → 1094 checks, 100% pass
```
