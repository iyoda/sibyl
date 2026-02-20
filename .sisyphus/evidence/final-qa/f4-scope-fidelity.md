# F4 Scope Fidelity Check: session-persistence

## Verdict
Tasks [3/7 compliant] | Contamination [CLEAN/0 issues] | Unaccounted [1 files] | VERDICT: REJECT

## Scope Audit by Task

### Task 1 - Serialization Layer
- Status: COMPLIANT
- `generate-session-id` format implemented as `session-YYYYMMDD-HHMMSS-NNNNNN` with random suffix via `(random 1000000)` in `src/repl/session.lisp:5`.
- `tool-call->sexp` / `sexp->tool-call` preserve nil arguments in `src/repl/session.lisp:12` and `src/repl/session.lisp:19`.
- `message->sexp` / `sexp->message` handle tool-calls and tool-call-id in `src/repl/session.lisp:27` and `src/repl/session.lisp:44`.
- `session->sexp` includes `(:session-version 1)` and filters `:system` messages in `src/repl/session.lisp:60`.
- No serialization of `thinking` or `thinking-signature` (explicitly set nil on deserialize only) in `src/repl/session.lisp:57`.

### Task 2 - Package Exports + ASDF
- Status: COMPLIANT
- `src/packages.lisp:428` exports `save-session`, `load-session`, `list-sessions`, `generate-session-id`.
- `sibyl.asd:71` includes `(:file "session")` in `repl-module`.
- No new package definitions or new dependencies were added.

### Task 3 - File I/O + Index
- Status: NON-COMPLIANT
- Implemented: atomic write temp+rename, lock, save/load/list, rebuild path (`src/repl/session.lisp:186`, `src/repl/session.lisp:209`, `src/repl/session.lisp:224`).
- Implemented: missing/corrupt handling with warnings in `src/repl/session.lisp:217` and `src/repl/session.lisp:221`.
- Violation: index format does not match required tagged structure `(:session-index (:entry ...))`; implementation stores raw list of plists in `%save-session-index` / `%load-session-index` (`src/repl/session.lisp:140`, `src/repl/session.lisp:149`).
- Violation: `%update-session-index` signature uses full session sexp instead of required metadata argument (`src/repl/session.lisp:159`).

### Task 4 - REPL Commands
- Status: COMPLIANT
- 3 commands registered in `*repl-commands*` at `src/repl.lisp:24`.
- 3 handlers registered in `*command-handlers*` at `src/repl.lisp:787`.
- Help text updated with `/sessions`, `/save`, `/load <id>` at `src/repl.lisp:404` and `src/repl.lisp:428`.
- Handler behaviors (`list`, `save`, `load`, usage on empty ID, pre-load auto-save, conversation replace) are present in `src/repl.lisp:708`, `src/repl.lisp:725`, `src/repl.lisp:739`.

### Task 5 - REPL Integration
- Status: NON-COMPLIANT
- Implemented: `start-repl` accepts `:session-id` and `:auto-save-interval` in `src/repl.lisp:1338`.
- Implemented: exit path saves session and prints resume hint in `src/repl.lisp:1477`.
- Implemented: `/reset` saves before reset in `src/repl.lisp:344`.
- Violation: required dynamic variable `*auto-save-interval*` is missing (no definition in `src/repl.lisp`).

### Task 6 - Auto-save Timer
- Status: NON-COMPLIANT
- Implemented: granular 1s sleep loop and stop-flag check in `src/repl/session.lisp:248`.
- Implemented: timer errors are caught and logged as warnings in `src/repl/session.lisp:257`.
- Implemented: stop called before final save in REPL exit path (`src/repl.lisp:1478`).
- Violation: `stop-auto-save-timer` does not join with a 5s timeout (plain `bt:join-thread` only) in `src/repl/session.lisp:275`.
- Violation: no debug log emitted on stop in `src/repl/session.lisp:269`.

### Task 7 - Tests
- Status: NON-COMPLIANT
- Implemented: `tests/session-test.lisp` exists with suite `session-tests` at `tests/session-test.lisp:5`.
- Implemented: suite classified UNSAFE in `tests/suite.lisp:124`.
- Implemented: `(:file "session-test")` in test system at `sibyl.asd:122`.
- Violation: command handler tests required by plan are missing (`test-sessions-command`, `test-save-command`, `test-load-command`, `test-load-missing-command` absent from `tests/session-test.lisp`).
- Violation: required `test-timer-fires` missing; only `test-timer-stops` exists at `tests/session-test.lisp:195`.
- Violation: session-id format test is weaker than required regex check (only prefix check at `tests/session-test.lisp:183`).

## Cross-task Contamination Check
- Result: CLEAN.
- No out-of-scope commands (e.g., `/delete`, `/rename`, `/export`) were found.
- No session deletion/rotation/cleanup features were found.

## Unaccounted File Check
- Expected modified set: `src/repl/session.lisp`, `src/repl.lisp`, `src/packages.lisp`, `sibyl.asd`, `tests/session-test.lisp`, `tests/suite.lisp`.
- Additional session-related artifact found by grep: `tests/timing-history.json` (contains `session-tests` timing data).
- Unaccounted result: NOT CLEAN (1 file).
