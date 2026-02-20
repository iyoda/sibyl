# Fix Ctrl+C Double-Press Exit in Sibyl REPL

## TL;DR

> **Quick Summary**: Fix the Ctrl+C interrupt handler so single-press at idle shows a hint and stays in the REPL, and double-press cleanly exits. Currently, single Ctrl+C at idle ejects to SBCL top-level, and double Ctrl+C opens the SBCL debugger.
> 
> **Deliverables**:
> - Fixed `install-interrupt-handler` using throw/catch instead of broken `invoke-restart`
> - Pure testable decision function `%interrupt-action`
> - Mock-based unit tests for the decision logic
> 
> **Estimated Effort**: Short
> **Parallel Execution**: YES — 2 waves + final verification
> **Critical Path**: Task 1 → Task 2 → Final Verification

---

## Context

### Original Request
User reported: pressing Ctrl+C at idle Sibyl REPL prompt changes to `*` (SBCL top-level), then pressing Ctrl+C again opens the SBCL debugger instead of cleanly exiting.

### Live Reproduction
```
┌─[sibyl #2]
└─> ^C
[^C: LLM call cancelled. Press Ctrl+C again to exit.]
* ^C
debugger invoked on a SB-SYS:INTERACTIVE-INTERRUPT ...
```

### Interview Summary
**Key Discussions**:
- Double Ctrl+C: silent exit (no farewell message, just return to shell)
- Single Ctrl+C at idle: print hint "Press Ctrl+C again to exit" and redisplay prompt
- Single Ctrl+C during LLM call: preserve existing cancel behavior
- Test strategy: mock-based unit tests (no real signal testing)

**Research Findings**:
- **Root cause**: `invoke-restart 'continue` at L1228 of `repl.lisp` has no `continue` restart in scope when Ctrl+C fires at the idle prompt (blocked on `cl-readline:readline`). SBCL exits to top-level.
- **Inner handler works**: L1376-1389 uses `throw 'repl-cancelled` with a matching `catch` — this pattern succeeds.
- **Inner handler is dead code**: The `handler-case` at L1480 is searched first by CL handler semantics and handles the interrupt. The `handler-bind` at L1376-1389 never actually executes during normal LLM calls. This is important context but NOT something to fix in this task.
- **3 handler locations**: outer (L1212-1228), inner handler-bind (L1376-1389), inner handler-case (L1480-1486).

### Metis Review
**Identified Gaps** (addressed):
- `*cancel-requested*` should NOT be set at idle (nothing to cancel) → incorporated
- `*last-interrupt-time*` should be reset at REPL start to prevent stale state → incorporated
- Hint message should NOT mention "LLM call cancelled" at idle → incorporated
- cl-readline FFI state on throw-through needs `fresh-line` after catch → incorporated
- Extract decision logic into a pure testable function → incorporated

---

## Work Objectives

### Core Objective
Fix the outer interrupt handler (`install-interrupt-handler`) to use throw/catch instead of `invoke-restart 'continue`, so Ctrl+C at idle stays in the REPL and double Ctrl+C cleanly exits.

### Concrete Deliverables
- Modified `install-interrupt-handler` in `src/repl.lisp` (throw instead of invoke-restart)
- New catch wrapper around `repl-body` tagbody
- New pure function `%interrupt-action` for testable decision logic
- Reset of `*last-interrupt-time*` at REPL start
- New test suite `interrupt-handler-tests` in `tests/rich-repl-test.lisp`

### Definition of Done
- [x] Single Ctrl+C at idle prints hint and returns to Sibyl prompt
- [x] Double Ctrl+C at idle exits silently (process exits, no farewell message)
- [x] Single Ctrl+C during LLM call still cancels (existing behavior preserved)
- [x] `grep -c 'invoke-restart.*continue' src/repl.lisp` returns 0
- [x] `(asdf:test-system :sibyl)` passes

### Must Have
- Pure decision function `%interrupt-action` that takes timestamp args and returns `:hint` or `:exit`
- Outer handler uses `throw` to a catch tag instead of `invoke-restart`
- Catch wrapper prints hint and re-enters the tagbody loop via `go next-iteration`
- `*last-interrupt-time*` reset to 0 at REPL start
- No `*cancel-requested*` set at idle
- Hint message: `[Press Ctrl+C again to exit.]` (no "LLM call cancelled" text)
- `fresh-line` after catch to handle cl-readline terminal state
- Mock-based unit tests for `%interrupt-action`

### Must NOT Have (Guardrails)
- DO NOT modify inner `handler-bind` at L1376-1389 — it is dead code but harmless; leave it
- DO NOT modify inner `handler-case` at L1480-1498 — this is the REAL LLM interrupt handler
- DO NOT modify `catch 'repl-cancelled` at L1375 — part of existing inner mechanism
- DO NOT add farewell message to double-press exit path — user wants silent exit
- DO NOT add new config options — this is a bug fix, not a feature
- DO NOT export new symbols from `sibyl.repl` package
- DO NOT fire real `sb-sys:interactive-interrupt` signals in tests — mock only
- DO NOT refactor all three handlers into a unified mechanism — out of scope
- DO NOT add `*last-interrupt-time*` update to handler-case B at L1480 — cross-context double-press is a separate concern
- DO NOT remove or restructure `exit-repl` or `repl-body` beyond the catch wrapper addition

---

## Verification Strategy

> **ZERO HUMAN INTERVENTION** — ALL verification is agent-executed. No exceptions.

### Test Decision
- **Infrastructure exists**: YES (FiveAM, `tests/suite.lisp`)
- **Automated tests**: YES (tests-after — decision function is extracted for testability)
- **Framework**: FiveAM
- **New suite**: `interrupt-handler-tests` added to `*safe-suites*` (pure logic, no I/O)

### QA Policy
Every task includes agent-executed QA scenarios.
Evidence saved to `.sisyphus/evidence/task-{N}-{scenario-slug}.{ext}`.

- **Unit tests**: Use Bash (`sbcl --eval ...`) — load system, run tests, verify pass count
- **Structural verification**: Use Bash (`grep`) — verify `invoke-restart` removed

---

## Execution Strategy

### Parallel Execution Waves

```
Wave 1 (Core fix — single task, foundational):
└── Task 1: Fix interrupt handler + extract decision function [quick]

Wave 2 (Tests — depends on Wave 1):
└── Task 2: Write mock-based unit tests [quick]

Wave FINAL (After ALL tasks — parallel verification):
├── Task F1: Plan compliance audit (oracle)
├── Task F2: Code quality review (unspecified-high)
├── Task F3: Structural verification + test run (unspecified-high)
└── Task F4: Scope fidelity check (deep)

Critical Path: Task 1 → Task 2 → F1-F4
```

### Dependency Matrix

| Task | Depends On | Blocks | Wave |
|------|-----------|--------|------|
| 1    | —         | 2, F1-F4 | 1  |
| 2    | 1         | F1-F4  | 2    |
| F1   | 1, 2      | —      | FINAL |
| F2   | 1, 2      | —      | FINAL |
| F3   | 1, 2      | —      | FINAL |
| F4   | 1, 2      | —      | FINAL |

### Agent Dispatch Summary

- **Wave 1**: 1 agent — T1 → `quick`
- **Wave 2**: 1 agent — T2 → `quick`
- **FINAL**: 4 agents — F1 → `oracle`, F2 → `unspecified-high`, F3 → `unspecified-high`, F4 → `deep`

---

## TODOs

- [x] 1. Fix Outer Interrupt Handler with throw/catch Pattern

  **What to do**:
  1. **Extract pure decision function** `%interrupt-action`:
     - Define `(defun %interrupt-action (now last-time window)` in `src/repl.lisp` (near L1200, before `install-interrupt-handler`)
     - Returns `:exit` if `(< (- now last-time) window)` (double-press within window)
     - Returns `:hint` otherwise (single press or timeout expired)
     - This is a pure function with no side effects — fully testable in isolation

  2. **Modify `install-interrupt-handler`** (L1201-1229):
     - Replace the lambda body to use `%interrupt-action` for decision
     - On `:exit`: call `(funcall exit-fn)` as before (calls `exit-repl`)
     - On `:hint`: update `*last-interrupt-time*`, print hint message `[Press Ctrl+C again to exit.]`, then `(throw 'idle-interrupt nil)` — do NOT set `*cancel-requested*` (nothing to cancel at idle)
     - Remove `(invoke-restart 'continue)` entirely from this function

  3. **Add catch wrapper in `repl-body`** (near L1342-1343):
     - Wrap the tagbody body inside `(catch 'idle-interrupt ...)` so that when the outer handler throws, control lands here
     - After catch: call `(fresh-line)` to clean terminal state after cl-readline interrupt, then `(go next-iteration)` to re-enter the loop
     - Structure: `(tagbody next-iteration (catch 'idle-interrupt (let ((input (read-user-input))) ...existing body...)) (go next-iteration))`
     - NOTE: The `(go next-iteration)` after the catch form handles both normal completion (existing `(go next-iteration)` at L1506 can be kept or the catch landing can provide it) and interrupt landing

  4. **Reset `*last-interrupt-time*`** at REPL start:
     - Add `(setf *last-interrupt-time* 0)` near the beginning of the `(block repl-loop ...)` body (after L1328), before `repl-body` is called
     - Prevents stale timestamp from a previous REPL session triggering false double-press detection

  **Must NOT do**:
  - DO NOT modify inner `handler-bind` at L1376-1389 (dead code but harmless)
  - DO NOT modify inner `handler-case` at L1480-1498 (the REAL LLM interrupt handler)
  - DO NOT set `*cancel-requested*` in the outer handler (nothing to cancel at idle)
  - DO NOT add farewell message to double-press exit
  - DO NOT add config options
  - DO NOT export new symbols

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: Focused change in a single file (~30 lines modified), well-defined scope, clear pattern to follow
  - **Skills**: []
    - No special skills needed — pure Common Lisp editing
  - **Skills Evaluated but Omitted**:
    - `playwright`: No browser interaction
    - `git-master`: Commit is deferred to end

  **Parallelization**:
  - **Can Run In Parallel**: NO (foundational — Task 2 depends on this)
  - **Parallel Group**: Wave 1 (solo)
  - **Blocks**: Task 2, F1-F4
  - **Blocked By**: None (can start immediately)

  **References** (CRITICAL):

  **Pattern References** (existing code to follow):
  - `src/repl.lisp:1201-1229` — Current `install-interrupt-handler` function. This is what you're modifying. Study the handler-bind structure and exit-fn pattern.
  - `src/repl.lisp:1375-1389` — Inner handler's throw/catch pattern. This is the PROVEN pattern to emulate: `(catch 'repl-cancelled (handler-bind (...) ...))` with `(throw 'repl-cancelled :cancelled)`. Your outer handler should use a similar throw/catch but with tag `'idle-interrupt`.
  - `src/repl.lisp:1342-1343` — Current `repl-body` tagbody structure. You need to wrap the body in a catch here.
  - `src/repl.lisp:1506` — Existing `(go next-iteration)` at end of loop. Your catch landing should also `(go next-iteration)`.

  **Variable References** (state to manage):
  - `src/repl.lisp:142-144` — `*last-interrupt-time*` defvar. Must be reset to 0 at REPL start.
  - `src/repl.lisp:130-132` — `*cancel-requested*` defvar. DO NOT set this in the outer handler. It is cleared at L1361 before agent-run.
  - `src/repl.lisp:1328` — `(block repl-loop ...)` — add `*last-interrupt-time*` reset after this line.

  **Exit Path References**:
  - `src/repl.lisp:1332-1341` — `exit-repl` labels function. Called by outer handler on double-press via `(funcall exit-fn)`. Uses `(return-from repl-loop)`. Already works correctly.
  - `src/repl.lisp:1507-1511` — Wrapper invocation. The #+sbcl path calls `(funcall wrapper #'repl-body)`. The catch must be INSIDE `repl-body`, not around the wrapper call.

  **CRITICAL CONTEXT for executor**:
  - The inner `handler-bind` at L1376-1389 is **dead code** — the `handler-case` at L1480 is searched first by CL semantics and handles the interrupt. The handler-bind never fires during normal LLM interaction. Do NOT rely on it for behavior and do NOT modify it.
  - `exit-repl` is a `labels` closure inside `start-repl` — it captures the `repl-loop` block name. It is NOT an exported or standalone function.
  - `(return-from repl-loop)` inside `exit-repl` will pass through the `catch 'idle-interrupt` without being intercepted — `catch` only catches `throw`, not `return-from`. This is safe.

  **Acceptance Criteria**:

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: invoke-restart removed from install-interrupt-handler
    Tool: Bash (grep)
    Preconditions: Task implementation complete
    Steps:
      1. Run: grep -c 'invoke-restart.*continue' src/repl.lisp
      2. Assert output is "0"
    Expected Result: No occurrences of invoke-restart with continue in repl.lisp
    Failure Indicators: grep returns any number > 0
    Evidence: .sisyphus/evidence/task-1-no-invoke-restart.txt

  Scenario: %interrupt-action function exists
    Tool: Bash (grep)
    Preconditions: Task implementation complete
    Steps:
      1. Run: grep -n '%interrupt-action' src/repl.lisp
      2. Assert output shows defun definition line
    Expected Result: Function definition found (defun %interrupt-action ...)
    Failure Indicators: No matches found
    Evidence: .sisyphus/evidence/task-1-decision-fn-exists.txt

  Scenario: catch idle-interrupt exists in repl-body
    Tool: Bash (grep)
    Preconditions: Task implementation complete
    Steps:
      1. Run: grep -n 'idle-interrupt' src/repl.lisp
      2. Assert both catch and throw forms are present
    Expected Result: At least 2 matches — one catch, one throw
    Failure Indicators: Fewer than 2 matches
    Evidence: .sisyphus/evidence/task-1-catch-exists.txt

  Scenario: *last-interrupt-time* reset at REPL start
    Tool: Bash (grep)
    Preconditions: Task implementation complete
    Steps:
      1. Run: grep -n 'last-interrupt-time.*0' src/repl.lisp
      2. Assert a setf to 0 exists near the start of start-repl (after block repl-loop)
    Expected Result: Reset found in the REPL initialization section
    Failure Indicators: No reset found, or reset is in the wrong location
    Evidence: .sisyphus/evidence/task-1-timestamp-reset.txt

  Scenario: *cancel-requested* NOT set in outer handler
    Tool: Bash (grep)
    Preconditions: Task implementation complete
    Steps:
      1. Run: grep -A5 'install-interrupt-handler' src/repl.lisp | grep 'cancel-requested'
      2. Assert no matches (cancel-requested not set in outer handler)
    Expected Result: No occurrences of *cancel-requested* in install-interrupt-handler
    Failure Indicators: Any match found
    Evidence: .sisyphus/evidence/task-1-no-cancel-at-idle.txt

  Scenario: Inner handlers unchanged
    Tool: Bash (grep)
    Preconditions: Task implementation complete
    Steps:
      1. Run: git diff src/repl.lisp and examine lines 1376-1389 and 1480-1498
      2. Assert these line ranges show NO modifications in the diff
    Expected Result: Inner handler-bind and handler-case are completely untouched
    Failure Indicators: Any changes in those regions
    Evidence: .sisyphus/evidence/task-1-inner-handlers-unchanged.txt

  Scenario: System loads without errors
    Tool: Bash (sbcl)
    Preconditions: Task implementation complete
    Steps:
      1. Run: sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(format t "LOAD-OK~%")' --quit
      2. Assert output contains "LOAD-OK"
    Expected Result: System loads cleanly without compile errors
    Failure Indicators: Compile errors, warnings about undefined functions/variables
    Evidence: .sisyphus/evidence/task-1-system-loads.txt
  ```

  **Evidence to Capture:**
  - [x] task-1-no-invoke-restart.txt
  - [x] task-1-decision-fn-exists.txt
  - [x] task-1-catch-exists.txt
  - [x] task-1-timestamp-reset.txt
  - [x] task-1-no-cancel-at-idle.txt
  - [x] task-1-inner-handlers-unchanged.txt
  - [x] task-1-system-loads.txt

  **Commit**: YES (groups with Task 2)
  - Message: `fix(repl): fix Ctrl+C handler to use throw/catch instead of invoke-restart`
  - Files: `src/repl.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(format t "OK")' --quit`

- [x] 2. Write Mock-Based Unit Tests for Interrupt Decision Logic

  **What to do**:
  1. **Add new test suite** `interrupt-handler-tests` in `tests/rich-repl-test.lisp`:
     - Define: `(def-suite interrupt-handler-tests :description "Tests for interrupt handler decision logic" :in sibyl-tests)`
     - Add `(in-suite interrupt-handler-tests)` before the tests

  2. **Write tests for `%interrupt-action`**:
     - Test: single press (large time gap) → returns `:hint`
     - Test: double press (within 2-second window) → returns `:exit`
     - Test: press after timeout expired (just over 2 seconds) → returns `:hint`
     - Test: first-ever press (last-time = 0, now = large) → returns `:hint`
     - Test: boundary condition — exactly at window edge

  3. **Classify suite in `tests/suite.lisp`**:
     - Add `interrupt-handler-tests` to `*safe-suites*` list (pure logic, no I/O, no global state)

  4. **Verify all tests pass**:
     - Run `(asdf:test-system :sibyl)` and confirm new tests are included and pass

  **Must NOT do**:
  - DO NOT fire real `sb-sys:interactive-interrupt` signals
  - DO NOT test the actual handler-bind mechanism (test the decision function only)
  - DO NOT add tests for the inner handler or handler-case (out of scope)
  - DO NOT export new symbols for testing — use `sibyl.repl::` internal access

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: Straightforward test writing, follows established patterns in the same file
  - **Skills**: []
    - No special skills needed
  - **Skills Evaluated but Omitted**:
    - `playwright`: No browser interaction

  **Parallelization**:
  - **Can Run In Parallel**: NO (depends on Task 1)
  - **Parallel Group**: Wave 2 (solo)
  - **Blocks**: F1-F4
  - **Blocked By**: Task 1

  **References** (CRITICAL):

  **Pattern References** (existing tests to follow):
  - `tests/rich-repl-test.lisp:1-12` — Suite definition pattern: `(def-suite NAME :description "..." :in sibyl-tests)` followed by `(in-suite NAME)`. Follow this exact pattern.
  - `tests/rich-repl-test.lisp:18-23` — Test pattern: `(test NAME "description" (let (...) (is ...)))`. Uses `sibyl.repl::` for internal symbol access. Follow this pattern.
  - `tests/rich-repl-test.lisp:340-397` — Ctrl+J behavioral tests (most recent addition). These mock internal functions and test behavior — closest pattern to what you need.

  **Classification References**:
  - `tests/suite.lisp:46-79` — `*safe-suites*` list. Add `interrupt-handler-tests` here. Place it after `session-summary-tests` (last entry).

  **API References** (function to test):
  - `src/repl.lisp` — `%interrupt-action` (created by Task 1). Pure function: `(now last-time window) → :hint | :exit`. Test it via `(sibyl.repl::%interrupt-action ...)`.
  - `internal-time-units-per-second` — CL constant. The window parameter is `(* 2 internal-time-units-per-second)`. Use this in tests for realistic values.

  **Acceptance Criteria**:

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: New test suite exists and is classified
    Tool: Bash (grep)
    Preconditions: Task implementation complete
    Steps:
      1. Run: grep -n 'interrupt-handler-tests' tests/rich-repl-test.lisp
      2. Assert def-suite and in-suite lines exist
      3. Run: grep -n 'interrupt-handler-tests' tests/suite.lisp
      4. Assert it appears in *safe-suites*
    Expected Result: Suite defined in test file and classified as safe
    Failure Indicators: Missing from either file
    Evidence: .sisyphus/evidence/task-2-suite-classified.txt

  Scenario: All tests pass including new interrupt tests
    Tool: Bash (sbcl)
    Preconditions: Task 1 and Task 2 complete
    Steps:
      1. Run: sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(let ((results (fiveam:run (quote sibyl.tests::interrupt-handler-tests)))) (format t "~%TESTS-RUN: ~a~%" (length results)) (format t "PASS: ~a~%" (every (lambda (r) (typep r (quote fiveam::test-passed))) results)))' --quit
      2. Assert TESTS-RUN is >= 4 (at least 4 test cases)
      3. Assert PASS is T
    Expected Result: All interrupt-handler-tests pass
    Failure Indicators: Any test failure, PASS is NIL
    Evidence: .sisyphus/evidence/task-2-tests-pass.txt

  Scenario: Full test suite still passes
    Tool: Bash (sbcl)
    Preconditions: Task 1 and Task 2 complete
    Steps:
      1. Run: sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit
      2. Assert no test failures (check output for "Fail: 0" or similar)
    Expected Result: Full test suite passes, no regressions
    Failure Indicators: Any test failure beyond the pre-existing ollama-tests failure
    Evidence: .sisyphus/evidence/task-2-full-suite.txt

  Scenario: Tests cover key decision scenarios
    Tool: Bash (grep)
    Preconditions: Task implementation complete
    Steps:
      1. Run: grep -c '(test ' tests/rich-repl-test.lisp | count new tests
      2. Verify at least 4 test functions exist for interrupt-action: single-press, double-press, timeout-expired, first-ever-press
    Expected Result: At least 4 distinct test cases for %interrupt-action
    Failure Indicators: Fewer than 4 test cases
    Evidence: .sisyphus/evidence/task-2-test-coverage.txt
  ```

  **Evidence to Capture:**
  - [x] task-2-suite-classified.txt
  - [x] task-2-tests-pass.txt
  - [x] task-2-full-suite.txt
  - [x] task-2-test-coverage.txt

  **Commit**: YES (groups with Task 1)
  - Message: `fix(repl): fix Ctrl+C handler to use throw/catch instead of invoke-restart`
  - Files: `src/repl.lisp`, `tests/rich-repl-test.lisp`, `tests/suite.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit`

---

## Final Verification Wave (MANDATORY — after ALL implementation tasks)

> 4 review agents run in PARALLEL. ALL must APPROVE. Rejection → fix → re-run.

- [x] F1. **Plan Compliance Audit** — `oracle` — APPROVE
  Read the plan end-to-end. For each "Must Have": verify implementation exists (read file, grep for patterns, run commands). For each "Must NOT Have": search codebase for forbidden patterns — reject with file:line if found. Check evidence files exist in `.sisyphus/evidence/`. Compare deliverables against plan.
  Output: `Must Have [N/N] | Must NOT Have [N/N] | Tasks [N/N] | VERDICT: APPROVE/REJECT`

- [x] F2. **Code Quality Review** — `unspecified-high` — APPROVE
  Review all changed lines in `src/repl.lisp` for: `as any`/`@ts-ignore` equivalents (unused `declare (ignore ...)`), empty catches, `format t` debugging left in prod code, commented-out code, unused variables. Check AI slop: excessive comments, over-abstraction, generic names. Verify indentation follows CL conventions (2 spaces, align with macros). Run `(asdf:test-system :sibyl)` to verify build+tests pass.
  Output: `Build [PASS/FAIL] | Tests [N pass/N fail] | Files [N clean/N issues] | VERDICT`

- [x] F3. **Structural Verification + Test Run** — `unspecified-high` — APPROVE
  1. Verify `grep -c 'invoke-restart.*continue' src/repl.lisp` returns 0 (broken pattern removed)
  2. Verify `%interrupt-action` function exists in `src/repl.lisp`
  3. Verify `catch 'idle-interrupt` (or chosen tag name) exists in `repl-body`
  4. Verify `*last-interrupt-time*` is reset to 0 near start of REPL
  5. Verify new test suite `interrupt-handler-tests` is in `*safe-suites*` in `tests/suite.lisp`
  6. Run `(asdf:test-system :sibyl)` and verify all tests pass including new ones
  7. Verify inner handlers at L1376-1389 and L1480-1498 are UNCHANGED (diff check)
  Output: `Structural [N/N pass] | Tests [PASS/FAIL] | Inner handlers [UNCHANGED/MODIFIED] | VERDICT`

- [x] F4. **Scope Fidelity Check** — `deep` — APPROVE (after whitespace fix)
  For each task: read "What to do", read actual diff (`git diff`). Verify 1:1 — everything in spec was built (no missing), nothing beyond spec was built (no creep). Check "Must NOT do" compliance. Detect cross-task contamination. Flag unaccounted changes.
  Output: `Tasks [N/N compliant] | Scope [CLEAN/N issues] | Unaccounted [CLEAN/N files] | VERDICT`

---

## Commit Strategy

- **Task 1 + 2**: `fix(repl): fix Ctrl+C handler to use throw/catch instead of invoke-restart` — `src/repl.lisp`, `tests/rich-repl-test.lisp`, `tests/suite.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit`

---

## Success Criteria

### Verification Commands
```bash
# Broken pattern removed
grep -c 'invoke-restart.*continue' src/repl.lisp  # Expected: 0

# Decision function exists
grep '%interrupt-action' src/repl.lisp  # Expected: function definition found

# Tests pass
sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit  # Expected: all pass

# New suite classified
grep 'interrupt-handler-tests' tests/suite.lisp  # Expected: found in *safe-suites*
```

### Final Checklist
- [x] All "Must Have" present
- [x] All "Must NOT Have" absent
- [x] All tests pass
- [x] Inner handlers (L1376-1389, L1480-1498) unchanged
