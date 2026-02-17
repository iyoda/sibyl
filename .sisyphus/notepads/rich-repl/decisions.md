# Rich REPL — Architectural Decisions

## [2026-02-17] Session ses_3954c162bffeQlfSjVgjT9e4lr

### D1: Hook-based enrichment (not core modification)
- **Decision**: All spinner/tool display/interrupt handling is external to `core.lisp` via agent hooks
- **Rationale**: Preserves clean separation between agent logic and presentation. Hooks exist precisely for this.
- **Implication**: `src/agent/core.lisp` has ZERO modifications

### D2: Hook wiring via `setf (agent-hooks ...)` post-construction
- **Decision**: After `make-agent`, use accessor to set hooks: `(setf (agent-hooks agent) ...)`
- **Rationale**: `make-agent` doesn't accept `:hooks`. The `agent` class DOES have `:hooks` slot with accessor.
- **Alternative rejected**: Adding `:hooks` to `make-agent` — would require touching `core.lisp`

### D3: Phase 2 streaming via dynamic variable
- **Decision**: `*streaming-text-callback*` in `sibyl.llm` — providers check this; REPL binds it
- **Rationale**: Dynamic binding propagates through same-thread call chain (REPL → agent-run → agent-step → complete-with-tools). No changes to agent core needed.
- **Alternative rejected**: `:stream t` keyword on `complete-with-tools` — would require `agent-step` changes

### D4: cl-readline is optional (separate ASDF subsystem)
- **Decision**: `sibyl/readline` subsystem. Base `sibyl` has NO dependency on cl-readline.
- **Rationale**: CFFI dependency requires libreadline installed. Base system must load everywhere.
- **Runtime probe**: `(readline-available-p)` → `(find-package :cl-readline)`

### D5: Spinner in separate file (`src/repl/spinner.lisp`)
- **Decision**: New module, not inlined into 764-line repl.lisp
- **Rationale**: Spinner is 100+ lines of threading code. Keeps repl.lisp maintainable.
- **Package**: `sibyl.repl.spinner` with public API: `start-spinner`, `stop-spinner`, `spinner-active-p`, `update-spinner-message`

### D6: SBCL-first Ctrl+C with feature guards
- **Decision**: `sb-sys:interactive-interrupt` handler with `#+sbcl` guards
- **Rationale**: README says "SBCL recommended". Non-SBCL implementations get no signal handling.
- **Behavior**: 1x Ctrl+C = set `*cancel-requested*` flag; 2x within 2s = exit REPL

### D7: Flag-based cooperative cancellation (not throw/invoke-restart)
- **Decision**: `*cancel-requested*` flag set in signal handler; checked after `agent-run` returns
- **Rationale**: Throwing from inside signal handler is unsafe if LLM call holds locks (deadlock risk)
- **Limitation**: LLM call can't be instantly terminated — it finishes current HTTP request

### D8: Elapsed time = total wall clock
- **Decision**: `get-internal-real-time` before/after `agent-run` — includes LLM + tool execution
- **Format**: `[elapsed: X.Xs]`

### D9: History file at `~/.sibyl_history`
- **Decision**: File-backed history, loaded on REPL start, saved on exit
- **Scope**: Only main REPL input (line 716 of repl.lisp). Confirmation prompts (line 572) stay as read-line.
