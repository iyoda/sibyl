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
