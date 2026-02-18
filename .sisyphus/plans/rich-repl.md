# Rich REPL Interface Enhancement

## TL;DR

> **Quick Summary**: Enrich Sibyl's CLI interactive experience in two phases — Phase 1 adds spinner, Ctrl+C handling, input history, tool call display, and elapsed time; Phase 2 adds LLM response streaming with SSE support.
> 
> **Deliverables**:
> - Fix missing `defvar` declarations (prerequisite bug fix)
> - Spinner animation during LLM calls (background thread + ANSI)
> - Ctrl+C: single press cancels LLM call, double press exits REPL
> - Input history with ↑↓ arrow keys via cl-readline (optional dependency)
> - Tool call display during agent execution (via hook system)
> - Elapsed time shown after LLM responses
> - SSE streaming support for Anthropic and OpenAI (Phase 2)
> - Token-by-token display replacing spinner (Phase 2)
> 
> **Estimated Effort**: Large
> **Parallel Execution**: YES - 3 waves (Phase 1) + 2 waves (Phase 2)
> **Critical Path**: Task 1 → Task 2 → Task 5 → Task 6 → Task 7 → Task 8 (Phase 1) then Task 9 → Task 10 → Task 11 (Phase 2)

---

## Context

### Original Request
> "cliの対話インターフェス体験をリッチにしたいです。LLMの応答待ちの表現やCtrl+Cを二回でREPLを終了するなどの機能追加を検討してください"

### Interview Summary
**Key Discussions**:
- **Streaming approach**: User chose 段階的に両方 (phased: Phase 1 spinner, Phase 2 streaming)
- **Ctrl+C behavior**: 1回で中断 (cancel LLM call) + 2回で終了 (exit REPL)
- **External libraries**: 必要なら追加OK (permitted if needed)
- **Additional features selected**: ツール呼出し表示, 入力ヒストリ(↑↓), 経過時間表示
- **Explicitly excluded**: タブ補完, レスポンス書式化 (Markdown rendering)
- **Bug discovered**: `*use-colors*`, `*command-count*`, `*command-history*` missing `defvar` declarations — must fix first

**Research Findings**:
- **cl-readline** (Quicklisp): Wraps GNU libreadline via CFFI. Provides history, line editing. Needs `brew link readline --force` on macOS.
- **Agent hooks exist**: `:before-step`, `:after-step`, `:on-tool-call`, `:on-error` in `src/agent/core.lisp:121-128`. Currently unused by REPL — spinner and tool display should use these.
- **Spinner pattern**: Background `bt:make-thread` + ANSI `\r` + `\e[2K` (clear line). Must coordinate with cl-readline terminal ownership.
- **SBCL Ctrl+C**: `sb-sys:interactive-interrupt` condition (NOT a subclass of `error`). Flag-based cooperative cancellation is safest.
- **Streaming**: `dex:post :want-stream t` returns a stream. Anthropic SSE uses `event: content_block_delta` + `data: {"type":"content_block_delta","delta":{"type":"text_delta","text":"..."}}`.

### Metis Review
**Identified Gaps** (addressed):
- **cl-readline must be optional**: Adding to `:depends-on` breaks builds without libreadline. → Use runtime probe with `(find-package :cl-readline)` and graceful fallback to `read-line`.
- **Spinner + cl-readline terminal conflict**: Both manage terminal state. → Strict ownership protocol: readline returns → start spinner → LLM completes → stop spinner → readline activates.
- **Ctrl+C in recursive agent-step**: Interrupt during lock-held code → deadlock. → Flag-based cooperative cancellation, NOT throw/invoke-restart from signal handler.
- **defvar fix changes default behavior**: `*use-colors*` currently unbound (effectively NIL). After fix with `t` default, ANSI codes appear in test output. → Tests already use `search` which tolerates ANSI wrapping. No breakage expected, but add explicit `(let ((*use-colors* nil)) ...)` in any new tests that match exact output.
- **Spinner module placement**: Don't inline 100+ lines into 764-line `repl.lisp`. → Create separate `src/repl/spinner.lisp`.

### Momus Review — Blocking Issues Fixed
Two blocking issues identified and resolved:

**Issue 1 (FIXED): Hook wiring mechanism clarified**
- `make-agent` (line 104-115 of `core.lisp`) does NOT pass `:hooks` to `make-instance`, even though the `agent` class has a `:hooks` slot with `:initarg :hooks` and `:accessor agent-hooks`.
- **Resolution**: Task 7 (integration) uses `(setf (agent-hooks agent) ...)` **after** `make-agent` returns, using the public `agent-hooks` accessor. No changes to `core.lisp` needed.
- Example: `(let ((agent (make-agent ...))) (setf (agent-hooks agent) (list (cons :on-tool-call hook-fn) ...)) ...)`

**Issue 2 (FIXED): Phase 2 streaming call path via dynamic variable**
- `agent-step` calls `complete`/`complete-with-tools` without streaming options. Adding `:stream t` to `start-repl` wouldn't reach the provider layer without modifying `agent-step`.
- **Resolution**: Phase 2 uses a dynamic variable `*streaming-text-callback*` (defined in `sibyl.llm` package). When bound to a function in REPL, provider methods (`complete`/`complete-with-tools`) check it and stream tokens if non-nil. When nil (the default), behavior is unchanged.
- The REPL binds `*streaming-text-callback*` around the `agent-run` call, not inside `agent-step`. This requires provider method changes only — `agent-step` and `core.lisp` stay untouched.
- Pattern: `(let ((*streaming-text-callback* #'write-token-to-stdout)) (agent-run agent input))`
- **Missing cancellation condition**: No `llm-cancelled` in condition hierarchy. → Add under `llm-error` in `conditions.lisp`.
- **SBCL-only assumption**: Ctrl+C uses SBCL-specific `sb-sys:interactive-interrupt`. → Use `#+sbcl` / `#-sbcl` feature guards. Sibyl README says "SBCL recommended" so SBCL-first is fine.
- **agent-step hook-based approach**: Spinner and tool display via hooks keeps `core.lisp` untouched. → Register hooks when creating agent in `start-repl`.
- **Elapsed time scope**: Should include total wall time (LLM + tools), not just inference time. → Wrap `agent-run` with `get-internal-real-time`.

---

## Work Objectives

### Core Objective
Transform Sibyl's REPL from a basic read-eval-print loop into a responsive, modern CLI experience with visual feedback during LLM operations, graceful interrupt handling, and convenient input history.

### Concrete Deliverables
- `src/repl.lisp`: Fixed `defvar` declarations, refactored input function, hook-based agent invocation with spinner/timing
- `src/repl/spinner.lisp`: New file — spinner animation module (start/stop, thread management, ANSI output)
- `src/conditions.lisp`: New `llm-cancelled` condition
- `src/packages.lisp`: Updated exports for new symbols
- `sibyl.asd`: Optional `sibyl/readline` subsystem, spinner module registration
- `src/llm/client.lisp`: New `http-post-stream` function (Phase 2)
- `src/llm/providers.lisp`: Streaming `complete`/`complete-with-tools` methods (Phase 2)
- `tests/rich-repl-test.lisp`: Registered in `sibyl.asd`, expanded with spinner/signal/display tests

### Definition of Done
- [x] `(asdf:load-system :sibyl :force t)` completes with zero style-warnings about undefined variables
- [x] Spinner visibly animates during LLM calls, stops cleanly when response arrives
- [x] Single Ctrl+C during LLM call cancels and returns to prompt
- [x] Double Ctrl+C within 2 seconds exits REPL cleanly
- [x] ↑↓ arrows navigate input history (when cl-readline available)
- [x] Tool calls display name during execution (e.g., `🔧 read-file を実行中...`)
- [x] Elapsed time shown after each response (e.g., `[2.3s]`)
- [x] `(asdf:test-system :sibyl)` passes with zero failures (1559 checks, 100%)
- [x] Phase 2: LLM responses stream token-by-token to terminal

### Must Have
- Spinner during LLM calls (minimum viable UX improvement)
- Ctrl+C interrupt handling (single=cancel, double=exit)
- Input history with ↑↓ arrows
- Tool call display
- Elapsed time display
- All existing tests still pass

### Must NOT Have (Guardrails)
- **No tab completion** for `/commands` (explicitly excluded by user)
- **No Markdown rendering** of LLM responses (explicitly excluded by user)
- **No multiline input** support
- **No terminal capability detection** (`$TERM`, terminfo, terminal width)
- **No configurable spinner style** — hard-code one animation (braille dots)
- **No retry logic** for cancelled requests — cancel means cancel
- **No progress bar** — spinner only, no percentage
- **No debug logging** for spinner thread
- **No custom key bindings** beyond what cl-readline provides for free
- **No modifications to `src/agent/core.lisp`** — all REPL enrichment is external via hooks
- **No modifications to `agent-step`** or `agent-run` — hook-based approach only

---

## Verification Strategy

> **UNIVERSAL RULE: ZERO HUMAN INTERVENTION**
>
> ALL tasks in this plan MUST be verifiable WITHOUT any human action.
> ALL verification is executed by the agent using tools (SBCL REPL, Bash, tmux).

### Test Decision
- **Infrastructure exists**: YES (FiveAM, parallel test runner)
- **Automated tests**: YES (TDD where practical, tests-after for integration)
- **Framework**: FiveAM (already in use)

### Agent-Executed QA Strategy
- **Unit tests**: FiveAM assertions for each module
- **Integration tests**: SBCL REPL evaluation via `interactive_bash` (tmux)
- **Spinner verification**: Start/stop spinner programmatically, verify thread lifecycle
- **Ctrl+C verification**: `bt:interrupt-thread` to simulate interrupt, verify flag mechanism
- **cl-readline verification**: Test with/without package availability via runtime probe

---

## Execution Strategy

### Parallel Execution Waves

```
PHASE 1:

Wave 1 (Start Immediately):
├── Task 1: Fix defvar declarations [no dependencies]
└── Task 2: Create spinner module [no dependencies]

Wave 2 (After Wave 1):
├── Task 3: Add cl-readline optional dependency [depends: 1]
├── Task 4: Add Ctrl+C handling + llm-cancelled condition [depends: 1, 2]
└── Task 5: Add tool call display via hooks [depends: 1, 2]

Wave 3 (After Wave 2):
├── Task 6: Add elapsed time display [depends: 5]
├── Task 7: Integrate spinner + hooks into start-repl [depends: 3, 4, 5]
└── Task 8: Register rich-repl-test.lisp + expand tests [depends: all above]

PHASE 2:

Wave 4 (After Phase 1):
├── Task 9: Add SSE streaming to HTTP client [depends: Phase 1]
└── Task 10: Anthropic streaming provider [depends: 9]

Wave 5 (After Wave 4):
└── Task 11: OpenAI streaming + REPL integration [depends: 10]

Critical Path: Task 1 → Task 4 → Task 7 → Task 8
Parallel Speedup: ~35% faster than sequential
```

### Dependency Matrix

| Task | Depends On | Blocks | Can Parallelize With |
|------|------------|--------|---------------------|
| 1 | None | 3, 4, 5, 6, 7, 8 | 2 |
| 2 | None | 4, 5, 7 | 1 |
| 3 | 1 | 7 | 4, 5 |
| 4 | 1, 2 | 7 | 3, 5 |
| 5 | 1, 2 | 6, 7 | 3, 4 |
| 6 | 5 | 8 | 3, 4, 7 |
| 7 | 3, 4, 5 | 8 | 6 |
| 8 | All 1-7 | None | None (final Phase 1) |
| 9 | Phase 1 | 10 | None |
| 10 | 9 | 11 | None |
| 11 | 10 | None | None (final) |

### Agent Dispatch Summary

| Wave | Tasks | Recommended Agents |
|------|-------|-------------------|
| 1 | 1, 2 | task(category="quick") + task(category="unspecified-high") — parallel |
| 2 | 3, 4, 5 | task(category="unspecified-high") × 3 — parallel |
| 3 | 6, 7, 8 | task(category="quick") + task(category="deep") + task(category="unspecified-high") |
| 4 | 9, 10 | task(category="deep") — sequential (10 depends on 9) |
| 5 | 11 | task(category="deep") |

---

## TODOs

### Phase 1: Core UX Enhancements

---

- [x] 1. Fix missing defvar declarations

  **What to do**:
  - Add `defvar` declarations for `*use-colors*`, `*command-count*`, and `*command-history*` in `src/repl.lisp`
  - Place them in the top section of the file (after the existing `defvar` declarations around lines 27-41)
  - Initial values: `*use-colors*` → `t`, `*command-count*` → `0`, `*command-history*` → `nil`
  - Verify no SBCL style-warnings on fresh load
  - Verify `(incf *command-count*)` works without error (currently crashes because `(+ nil 1)` is invalid)

  **Must NOT do**:
  - Do not change the behavior of any code that references these variables
  - Do not add any new features — this is a pure bug fix

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: Single file, 3-line addition, trivial change with clear acceptance criteria
  - **Skills**: []
    - No special skills needed for simple defvar additions
  - **Skills Evaluated but Omitted**:
    - `playwright`: No browser interaction needed
    - `frontend-ui-ux`: Not a UI task

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Task 2)
  - **Blocks**: Tasks 3, 4, 5, 6, 7, 8
  - **Blocked By**: None (can start immediately)

  **References**:

  **Pattern References** (existing code to follow):
  - `src/repl.lisp:27-41` — Existing `defvar` declarations for `*pending-suggestions*`, `*next-suggestion-id*`, `*command-handlers-lock*`. Follow this exact pattern (defvar with docstring).

  **Bug Evidence** (where variables are used without declaration):
  - `src/repl.lisp:126` — `(setf *use-colors* t)` in `handle-colors-command`
  - `src/repl.lisp:130` — `(setf *use-colors* nil)` in `handle-colors-command`
  - `src/repl.lisp:133` — `(if *use-colors* ...)` in `handle-colors-command`
  - `src/repl.lisp:711` — `(if *use-colors* ...)` in `read-user-input`
  - `src/repl.lisp:712` — `(format-enhanced-prompt "sibyl" *command-count*)` in `read-user-input`
  - `src/repl.lisp:718` — `(incf *command-count*)` in `read-user-input` — THIS WILL CRASH without defvar (nil + 1)
  - `src/repl.lisp:719` — `(push input *command-history*)` in `read-user-input`

  **Package Reference**:
  - `src/packages.lisp:165-170` — `sibyl.repl` package. These variables are internal (not exported), so no package changes needed.

  **Acceptance Criteria**:

  - [ ] Three `defvar` forms added to `src/repl.lisp` with docstrings
  - [ ] `*use-colors*` initial value is `t` (colors on by default)
  - [ ] `*command-count*` initial value is `0`
  - [ ] `*command-history*` initial value is `nil`

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Fresh system load produces no style-warnings
    Tool: Bash (sbcl)
    Preconditions: Quicklisp available, sibyl symlinked
    Steps:
      1. sbcl --eval '(handler-bind ((style-warning (lambda (w) (format *error-output* "STYLE-WARNING: ~a~%" w)))) (ql:quickload :sibyl :silent t))' --eval '(format t "~%LOAD-OK~%")' --quit
      2. Assert: stdout contains "LOAD-OK"
      3. Assert: stderr does NOT contain "STYLE-WARNING"
    Expected Result: Clean load with no warnings about undefined variables
    Evidence: Terminal output captured

  Scenario: Command count increments correctly
    Tool: Bash (sbcl)
    Preconditions: System loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(assert (= 0 sibyl.repl::*command-count*))' --eval '(incf sibyl.repl::*command-count*)' --eval '(assert (= 1 sibyl.repl::*command-count*))' --eval '(format t "~%INCF-OK~%")' --quit
      2. Assert: stdout contains "INCF-OK"
      3. Assert: No error condition raised
    Expected Result: *command-count* starts at 0 and increments to 1
    Evidence: Terminal output captured

  Scenario: All existing tests still pass
    Tool: Bash (sbcl)
    Preconditions: System and tests loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(let ((results (fiveam:run :sibyl-tests))) (format t "~%PASS: ~a FAIL: ~a~%" (count t results :key #\'"'"'fiveam::test-passed-p) (count nil results :key #\'"'"'fiveam::test-passed-p)) (unless (every #\'"'"'fiveam::test-passed-p results) (error "Tests failed")))' --eval '(format t "~%TESTS-OK~%")' --quit
      2. Assert: stdout contains "TESTS-OK"
      3. Assert: FAIL count is 0
    Expected Result: All existing tests pass after defvar fix
    Evidence: Test output captured
  ```

  **Commit**: YES
  - Message: `fix(repl): add missing defvar declarations for *use-colors*, *command-count*, *command-history*`
  - Files: `src/repl.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(assert (every #\'"'"'fiveam::test-passed-p (fiveam:run :sibyl-tests)))' --quit`

---

- [x] 2. Create spinner module

  **What to do**:
  - Create new file `src/repl/spinner.lisp` with spinner animation module
  - Implement spinner using `bordeaux-threads` (already a dependency) for background animation
  - Use braille dot characters: `⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏` (hardcoded, not configurable)
  - Core API:
    - `(start-spinner &optional message)` → returns spinner object, starts background thread
    - `(stop-spinner spinner)` → stops thread, clears spinner line, returns nil
    - `(spinner-active-p spinner)` → predicate
    - `(update-spinner-message spinner new-message)` → changes displayed text
  - Animation: `\r` (carriage return) + `\e[2K` (clear line) + frame + message, at ~100ms interval
  - Thread safety: use atomic flag (`*spinner-running*` or struct slot) for start/stop coordination
  - Must handle: spinner stop during sleep (use short sleep intervals, check flag frequently)
  - Output to `*standard-output*` captured at spinner creation time (for thread safety)
  - Register file in `sibyl.asd` under a new `:module "repl"` containing `(:file "spinner")`
  - Add `sibyl.repl.spinner` package to `packages.lisp` with exported symbols
  - Write FiveAM tests in `tests/rich-repl-test.lisp` for spinner lifecycle

  **Must NOT do**:
  - Do not make spinner style configurable (no `*spinner-style*` variable)
  - Do not add terminal capability detection
  - Do not add any REPL integration yet (that's Task 7)
  - Do not modify `src/repl.lisp` — spinner is a standalone module
  - Do not add debug logging

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: New module creation with threading, ANSI control codes, and careful lifecycle management. Not trivially simple.
  - **Skills**: []
    - No special skills needed — this is pure Common Lisp with bordeaux-threads
  - **Skills Evaluated but Omitted**:
    - `playwright`: No browser interaction
    - `frontend-ui-ux`: Not a visual UI task, it's terminal animation

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Task 1)
  - **Blocks**: Tasks 4, 5, 7
  - **Blocked By**: None (can start immediately)

  **References**:

  **Pattern References** (existing code to follow):
  - `src/repl.lisp:39` — `(defvar *command-handlers-lock* (bt:make-recursive-lock "command-handlers-lock"))` — Lock declaration pattern with named lock and docstring
  - `src/repl.lisp:82-94` — `format-colored-text` function — ANSI escape code output pattern using `#\Escape` and format directives
  - `sibyl.asd:18-41` — Module structure pattern. The new `:module "repl"` will replace the current `(:file "repl")` entry at line 41, containing `(:file "spinner")` and moving repl.lisp into it.

  **Threading References**:
  - `bordeaux-threads` is already in `sibyl.asd:13` dependencies
  - `tests/parallel-runner-test.lisp` — Only existing thread creation in codebase. Shows `bt:make-thread` usage pattern.

  **External References**:
  - Spinner pattern from aider CLI: braille dots `⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏` at ~80-100ms per frame
  - ANSI clear line: `\e[2K` (ESC[2K) clears entire line, `\r` returns cursor to column 0

  **Package Reference**:
  - `src/packages.lisp:165-170` — Follow this pattern for new `sibyl.repl.spinner` package definition

  **Acceptance Criteria**:

  - [ ] File `src/repl/spinner.lisp` exists with `sibyl.repl.spinner` package
  - [ ] `start-spinner` creates and returns spinner object with running thread
  - [ ] `stop-spinner` terminates thread within 200ms and clears terminal line
  - [ ] `spinner-active-p` returns T while running, NIL after stop
  - [ ] `update-spinner-message` changes displayed text mid-animation
  - [ ] `sibyl.asd` updated with new module structure
  - [ ] `packages.lisp` has `sibyl.repl.spinner` package with exports

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Spinner starts and stops cleanly
    Tool: Bash (sbcl)
    Preconditions: System loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(let ((s (sibyl.repl.spinner:start-spinner "Thinking..."))) (assert (sibyl.repl.spinner:spinner-active-p s)) (sleep 2) (sibyl.repl.spinner:stop-spinner s) (assert (not (sibyl.repl.spinner:spinner-active-p s))) (format t "~%SPINNER-OK~%"))' --quit
      2. Assert: stdout contains "SPINNER-OK"
      3. Assert: No error
    Expected Result: Spinner starts, runs for 2s, stops cleanly
    Evidence: Terminal output captured

  Scenario: Spinner thread terminates after stop
    Tool: Bash (sbcl)
    Preconditions: System loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(let* ((s (sibyl.repl.spinner:start-spinner "Test")) (thread (sibyl.repl.spinner::spinner-thread s))) (sibyl.repl.spinner:stop-spinner s) (sleep 0.3) (assert (not (bt:thread-alive-p thread))) (format t "~%THREAD-DEAD-OK~%"))' --quit
      2. Assert: stdout contains "THREAD-DEAD-OK"
    Expected Result: Thread is dead within 300ms of stop-spinner call
    Evidence: Terminal output captured

  Scenario: Multiple start/stop cycles don't leak threads
    Tool: Bash (sbcl)
    Preconditions: System loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(dotimes (i 5) (let ((s (sibyl.repl.spinner:start-spinner (format nil "Test ~a" i)))) (sleep 0.5) (sibyl.repl.spinner:stop-spinner s))) (format t "~%CYCLE-OK~%")' --quit
      2. Assert: stdout contains "CYCLE-OK"
      3. Assert: No error about thread limits or zombie threads
    Expected Result: 5 start/stop cycles complete without issues
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `feat(repl): add spinner animation module with background thread`
  - Files: `src/repl/spinner.lisp`, `src/packages.lisp`, `sibyl.asd`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(format t "LOAD-OK~%")' --quit`

---

- [x] 3. Add cl-readline as optional dependency with graceful fallback

  **What to do**:
  - Add `sibyl/readline` as a separate ASDF subsystem in `sibyl.asd` that depends on `#:sibyl` and `#:cl-readline`
  - In `src/repl.lisp`, implement runtime probe: `(readline-available-p)` → checks `(find-package :cl-readline)`
  - Replace `read-line` call in `read-user-input` (line 716) with conditional:
    - If cl-readline available: `(cl-readline:readline :prompt prompt)` + `(cl-readline:add-history input)`
    - If not available: existing `(read-line *standard-input* nil nil)` (unchanged)
  - Add history file support: save to `~/.sibyl_history`, load on startup, save on exit
  - Only replace the main REPL input at line 716 — do NOT replace `read-line` in `handle-improve-command` confirmation prompt (line 572-573)
  - Export `readline-available-p` from `sibyl.repl` package
  - Document in REPL startup banner whether readline is active (e.g., "Input history: enabled" or "Input history: install cl-readline for ↑↓ history")

  **Must NOT do**:
  - Do not add cl-readline to main `sibyl` system `:depends-on` — it MUST be optional
  - Do not add tab completion callbacks
  - Do not add custom key bindings
  - Do not add multiline input support
  - Do not modify any other `read-line` calls besides the main REPL input (line 716)

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: ASDF subsystem configuration, runtime feature detection, conditional codepath — requires careful integration
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `playwright`: No browser interaction

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 4, 5)
  - **Blocks**: Task 7
  - **Blocked By**: Task 1 (defvar fix — so read-user-input is stable before modification)

  **References**:

  **Pattern References** (existing code to follow):
  - `src/repl.lisp:709-720` — Current `read-user-input` function. THIS is the only function to modify.
  - `src/repl.lisp:571-573` — `handle-improve-command` confirmation prompt using `read-line`. Do NOT touch this.
  - `sibyl.asd:44-61` — `sibyl/tests` subsystem definition. Follow this pattern for `sibyl/readline` subsystem.
  - `src/packages.lisp:165-170` — `sibyl.repl` exports. Add `readline-available-p` here.

  **External References**:
  - cl-readline API: `(cl-readline:readline :prompt "text")` returns string or NIL on EOF
  - cl-readline history: `(cl-readline:add-history line)`, `(cl-readline:read-history path)`, `(cl-readline:write-history path)`
  - macOS note: May need `brew install readline && brew link readline --force` for CFFI to find libreadline

  **Acceptance Criteria**:

  - [ ] `sibyl/readline` subsystem defined in `sibyl.asd`
  - [ ] `(ql:quickload :sibyl)` succeeds WITHOUT cl-readline installed (graceful degradation)
  - [ ] `(sibyl.repl::readline-available-p)` returns NIL when cl-readline not loaded
  - [ ] When cl-readline IS loaded, input history works with ↑↓ arrows
  - [ ] History is saved to `~/.sibyl_history` on REPL exit
  - [ ] History is loaded from `~/.sibyl_history` on REPL start (if file exists)
  - [ ] Startup banner indicates whether readline is active

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: System loads without cl-readline (fallback path)
    Tool: Bash (sbcl)
    Preconditions: sibyl loaded, cl-readline NOT loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(assert (not (sibyl.repl::readline-available-p)))' --eval '(format t "~%FALLBACK-OK~%")' --quit
      2. Assert: stdout contains "FALLBACK-OK"
    Expected Result: readline-available-p returns NIL
    Evidence: Terminal output captured

  Scenario: read-user-input works without cl-readline
    Tool: Bash (sbcl)
    Preconditions: sibyl loaded, cl-readline NOT loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(with-input-from-string (*standard-input* "hello world") (let ((result (sibyl.repl::read-user-input))) (assert (string= "hello world" result)) (format t "~%INPUT-OK~%")))' --quit
      2. Assert: stdout contains "INPUT-OK"
    Expected Result: read-user-input falls back to read-line correctly
    Evidence: Terminal output captured

  Scenario: All existing tests pass with readline changes
    Tool: Bash (sbcl)
    Preconditions: System and tests loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(assert (every #\'"'"'fiveam::test-passed-p (fiveam:run :sibyl-tests)))' --eval '(format t "~%TESTS-OK~%")' --quit
      2. Assert: stdout contains "TESTS-OK"
    Expected Result: No regressions
    Evidence: Test output captured
  ```

  **Commit**: YES
  - Message: `feat(repl): add optional cl-readline integration for input history`
  - Files: `src/repl.lisp`, `src/packages.lisp`, `sibyl.asd`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(assert (every #\'"'"'fiveam::test-passed-p (fiveam:run :sibyl-tests)))' --quit`

---

- [x] 4. Implement Ctrl+C handling with cooperative cancellation

  **What to do**:
  - Add `llm-cancelled` condition to `src/conditions.lisp` under `llm-error`
  - Implement flag-based cooperative cancellation in `src/repl.lisp`:
    - `*cancel-requested*` — dynamic variable (flag), set by signal handler
    - `*last-interrupt-time*` — dynamic variable, for double-press detection
    - `install-interrupt-handler` — sets up `#+sbcl sb-sys:interactive-interrupt` handler in REPL context
  - Signal handler behavior:
    - First Ctrl+C: Set `*cancel-requested*` to `t`, stop any active spinner
    - Second Ctrl+C within 2 seconds: Signal REPL exit (return-from repl-loop)
    - Ctrl+C during user input (not waiting for LLM): Just show "^C" and redisplay prompt
  - Wrap `agent-run` call in `start-repl` with:
    - `handler-case` for `sb-sys:interactive-interrupt`
    - Check `*cancel-requested*` flag, display "[Cancelled]" message
  - The agent loop itself (`agent-step` in core.lisp) is NOT modified. The interrupt is handled at the REPL level, where `agent-run` is called from.
  - Use `#+sbcl` feature guard for all SBCL-specific code. On non-SBCL, Ctrl+C falls through to implementation default.
  - Reset `*cancel-requested*` before each `agent-run` call

  **Must NOT do**:
  - Do not modify `src/agent/core.lisp` — handle everything at REPL level
  - Do not use `throw`/`invoke-restart` from inside the signal handler (unsafe with locks)
  - Do not add retry logic — cancelled request is done
  - Do not catch Ctrl+C during non-LLM operations (e.g., tool execution listing)

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: SBCL-specific signal handling, thread coordination with spinner, careful lifecycle management
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `playwright`: No browser interaction

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 3, 5)
  - **Blocks**: Task 7
  - **Blocked By**: Task 1 (defvar fix), Task 2 (spinner — need to stop spinner on cancel)

  **References**:

  **Pattern References** (existing code to follow):
  - `src/conditions.lisp:28-58` — `llm-error` hierarchy. Add `llm-cancelled` here following same pattern.
  - `src/repl.lisp:756-762` — Current `handler-case` around `agent-run`. This is where interrupt handling wraps.
  - `src/repl.lisp:738-763` — `start-repl` main loop with `block repl-loop` and `tagbody`. The double Ctrl+C exit uses `return-from repl-loop`.
  - `src/repl.lisp:27-41` — Existing `defvar` declarations. Add `*cancel-requested*` and `*last-interrupt-time*` here.

  **SBCL References**:
  - `sb-sys:interactive-interrupt` — The condition signaled by Ctrl+C in SBCL. NOT a subclass of `error`.
  - `sb-ext:with-timeout` in `src/tools/lisp-tools.lisp:561` — Only existing SBCL-specific code in src/. Shows `#+sbcl` guard pattern.

  **Spinner Coordination**:
  - `src/repl/spinner.lisp` (from Task 2) — Need to call `stop-spinner` when interrupt is received

  **External References**:
  - SBCL manual: `sb-sys:interactive-interrupt` is delivered asynchronously to the main thread
  - Flag-based pattern: Set `*cancel-requested*` in handler, check in REPL loop after `agent-run` returns or is interrupted

  **Acceptance Criteria**:

  - [ ] `llm-cancelled` condition defined in `conditions.lisp` under `llm-error`
  - [ ] `*cancel-requested*` and `*last-interrupt-time*` dynamic variables defined
  - [ ] `install-interrupt-handler` function exists and works on SBCL
  - [ ] `#+sbcl` feature guard on all SBCL-specific code
  - [ ] Interrupt during LLM call returns to prompt with "[Cancelled]" message
  - [ ] Double Ctrl+C within 2 seconds exits REPL cleanly
  - [ ] Spinner is stopped when interrupt is received

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: llm-cancelled condition is properly defined
    Tool: Bash (sbcl)
    Preconditions: System loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(let ((c (make-condition (quote sibyl.conditions:llm-cancelled) :message "User cancelled"))) (assert (typep c (quote sibyl.conditions:llm-error))) (assert (typep c (quote sibyl.conditions:llm-cancelled))) (format t "~%CONDITION-OK~%"))' --quit
      2. Assert: stdout contains "CONDITION-OK"
    Expected Result: llm-cancelled is a subtype of llm-error
    Evidence: Terminal output captured

  Scenario: Cancel flag mechanism works
    Tool: Bash (sbcl)
    Preconditions: System loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(progn (assert (not sibyl.repl::*cancel-requested*)) (setf sibyl.repl::*cancel-requested* t) (assert sibyl.repl::*cancel-requested*) (setf sibyl.repl::*cancel-requested* nil) (format t "~%FLAG-OK~%"))' --quit
      2. Assert: stdout contains "FLAG-OK"
    Expected Result: Cancel flag can be set and cleared
    Evidence: Terminal output captured

  Scenario: Double interrupt detection via timestamp
    Tool: Bash (sbcl)
    Preconditions: System loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(progn (setf sibyl.repl::*last-interrupt-time* (get-internal-real-time)) (sleep 0.5) (let ((now (get-internal-real-time))) (assert (< (/ (- now sibyl.repl::*last-interrupt-time*) internal-time-units-per-second) 2)) (format t "~%DOUBLE-OK~%")))' --quit
      2. Assert: stdout contains "DOUBLE-OK"
    Expected Result: Timestamp-based double-press detection works within 2s window
    Evidence: Terminal output captured

  Scenario: All existing tests still pass
    Tool: Bash (sbcl)
    Preconditions: System and tests loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(assert (every #\'"'"'fiveam::test-passed-p (fiveam:run :sibyl-tests)))' --eval '(format t "~%TESTS-OK~%")' --quit
      2. Assert: stdout contains "TESTS-OK"
    Expected Result: No regressions from Ctrl+C additions
    Evidence: Test output captured
  ```

  **Commit**: YES
  - Message: `feat(repl): add Ctrl+C handling with cooperative cancellation and double-press exit`
  - Files: `src/conditions.lisp`, `src/repl.lisp`, `src/packages.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(assert (every #\'"'"'fiveam::test-passed-p (fiveam:run :sibyl-tests)))' --quit`

---

- [x] 5. Add tool call display via agent hooks

  **What to do**:
  - Create hook functions in `src/repl.lisp` for tool call display:
    - `:on-tool-call` hook: Display `🔧 <tool-name> を実行中...` when a tool is called
    - `:before-step` hook: (reserved for spinner integration in Task 7)
    - `:after-step` hook: (reserved for spinner integration in Task 7)
  - The hook receives a `tool-call` struct (from `src/agent/core.lisp:164-165`). Extract tool name via `(tool-call-name tc)`.
  - Hook output format: colored line showing tool name, cleared when next action starts
  - Use `format-colored-text` with `:cyan` for tool display
  - DO NOT register hooks yet — just define the hook functions. Task 7 will wire them into `make-agent`.
  - Also add spinner message update: when tool call fires, update spinner message to show tool name (call `update-spinner-message` from Task 2)

  **Must NOT do**:
  - Do not modify `src/agent/core.lisp`
  - Do not wire hooks into `make-agent` call yet (that's Task 7)
  - Do not add verbose tool result display

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Requires understanding hook system, tool-call struct, and ANSI output coordination
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `playwright`: No browser interaction

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 3, 4)
  - **Blocks**: Tasks 6, 7
  - **Blocked By**: Task 1 (defvar fix), Task 2 (spinner — for message update)

  **References**:

  **Pattern References** (existing code to follow):
  - `src/agent/core.lisp:121-128` — `run-hook` function. Shows how hooks are called with args. `:on-tool-call` at line 165 passes `tc` (tool-call struct).
  - `src/agent/core.lisp:164-165` — `(run-hook agent :on-tool-call tc)` — the exact hook invocation. `tc` is from `(message-tool-calls response)`.
  - `src/repl.lisp:82-94` — `format-colored-text` — use for colored tool name display
  - `src/llm/message.lisp` — `tool-call` struct definition. Need to find `tool-call-name` accessor.

  **Tool-call struct reference** (need to verify accessor name):
  - Likely in `src/llm/message.lisp` — look for `defstruct tool-call` or `defclass tool-call`

  **Spinner Integration**:
  - `src/repl/spinner.lisp` (from Task 2) — `update-spinner-message` to change spinner text to show tool name

  **Acceptance Criteria**:

  - [ ] Hook function `make-tool-call-hook` defined in `src/repl.lisp`
  - [ ] Hook displays tool name in colored format when called
  - [ ] Hook calls `update-spinner-message` if spinner is active
  - [ ] Hook function is testable in isolation (accepts tool-call-like input)

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Tool call hook produces correct output
    Tool: Bash (sbcl)
    Preconditions: System loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(let ((*use-colors* nil)) (let ((output (with-output-to-string (*standard-output*) (let ((tc (sibyl.llm::make-tool-call :name "read-file" :id "tc_1" :arguments (make-hash-table)))) (funcall (sibyl.repl::make-tool-call-hook) tc))))) (assert (search "read-file" output)) (format t "~%HOOK-OK~%")))' --quit
      2. Assert: stdout contains "HOOK-OK"
      3. Assert: Output contains tool name "read-file"
    Expected Result: Hook function displays tool name
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `feat(repl): add tool call display hook functions`
  - Files: `src/repl.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(assert (every #\'"'"'fiveam::test-passed-p (fiveam:run :sibyl-tests)))' --quit`

---

- [x] 6. Add elapsed time display

  **What to do**:
  - Add elapsed time measurement wrapper in `src/repl.lisp`
  - Implement `with-elapsed-time` macro or utility function:
    - Uses `get-internal-real-time` before and after body execution
    - Computes wall-clock seconds including tool execution time
  - Display format: `[elapsed: 2.3s]` in dim/gray color after LLM response
  - Place the display AFTER response output and BEFORE next prompt
  - Use `format-colored-text` with a dim style (e.g., `:white` with reduced emphasis, or raw ANSI dim `\e[2m`)

  **Must NOT do**:
  - Do not separate LLM inference time from tool execution time — just total wall time
  - Do not make format configurable
  - Do not add timing to non-LLM operations (commands like /help, /tools)

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: Simple timing wrapper with `get-internal-real-time` and format output. ~20 lines of code.
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `playwright`: No browser interaction

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 3 (with Tasks 7, 8)
  - **Blocks**: Task 8
  - **Blocked By**: Task 5 (tool display hooks — same output area)

  **References**:

  **Pattern References**:
  - `src/repl.lisp:756-758` — Current `agent-run` call site. Timing wraps this.
  - `src/repl.lisp:82-94` — `format-colored-text` for colored output

  **CL Standard References**:
  - `get-internal-real-time` — returns time in `internal-time-units-per-second` units
  - `internal-time-units-per-second` — for converting to seconds

  **Acceptance Criteria**:

  - [ ] Elapsed time displayed after every LLM response
  - [ ] Format matches `[elapsed: X.Xs]` pattern
  - [ ] Time includes total wall clock (LLM + tool calls)
  - [ ] No timing display for REPL commands (/help, /tools, etc.)

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Timing utility computes correct elapsed time
    Tool: Bash (sbcl)
    Preconditions: System loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(let ((start (get-internal-real-time))) (sleep 1) (let* ((end (get-internal-real-time)) (elapsed (/ (- end start) internal-time-units-per-second))) (assert (> elapsed 0.9)) (assert (< elapsed 1.5)) (format t "~%TIMING-OK: ~,1fs~%" elapsed)))' --quit
      2. Assert: stdout contains "TIMING-OK"
      3. Assert: elapsed value is between 0.9 and 1.5
    Expected Result: Timing measurement is accurate
    Evidence: Terminal output captured
  ```

  **Commit**: YES (groups with Task 7)
  - Message: `feat(repl): add elapsed time display after LLM responses`
  - Files: `src/repl.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(assert (every #\'"'"'fiveam::test-passed-p (fiveam:run :sibyl-tests)))' --quit`

---

- [x] 7. Integrate spinner + hooks + Ctrl+C into start-repl

  **What to do**:
  - This is the INTEGRATION task that wires together Tasks 2-6 into `start-repl`
  - Modify `start-repl` in `src/repl.lisp` (lines 722-763):
    1. **Install interrupt handler** at REPL start (from Task 4)
    2. **Create agent then set hooks via accessor**: Call `make-agent` as before, then use `setf` on the `agent-hooks` accessor:
       ```lisp
       (let ((agent (sibyl.agent:make-agent ...)))
         (setf (sibyl.agent:agent-hooks agent)
               (list (cons :on-tool-call (make-tool-call-hook))
                     (cons :before-step (make-before-step-hook))
                     (cons :after-step (make-after-step-hook))))
         ...)
       ```
       **IMPORTANT**: Do NOT pass `:hooks` to `make-agent` — it doesn't accept that parameter. Use `setf (agent-hooks ...)` after construction. The `agent` class has `agent-hooks` accessor via `:accessor agent-hooks`.
    3. **Wrap agent-run call** (line 757) with:
       - Start spinner before `agent-run`
       - Stop spinner after `agent-run` returns
       - Elapsed time measurement and display (from Task 6)
       - `handler-case` for `#+sbcl sb-sys:interactive-interrupt` (from Task 4)
       - Reset `*cancel-requested*` before call
    4. **Terminal ownership protocol**:
       - cl-readline reads input → spinner starts → LLM runs → spinner stops → response displayed → cl-readline reads input
       - Spinner must NEVER run while cl-readline is active
    5. **History file management**: Load history on REPL start, save on REPL exit (if cl-readline available)
    6. **Clean exit**: Ensure spinner is stopped on any exit path (normal quit, double Ctrl+C, EOF)

  **Must NOT do**:
  - Do not modify `src/agent/core.lisp` — hooks handle everything externally
  - Do not add new features beyond integration of existing components
  - Do not change the fundamental REPL loop structure (tagbody/go pattern)

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: Complex integration of 5 components with careful lifecycle management, terminal ownership, and error handling. Requires deep understanding of all preceding tasks.
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `playwright`: No browser interaction

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 3 (sequential after 3, 4, 5)
  - **Blocks**: Task 8
  - **Blocked By**: Tasks 3, 4, 5 (all components must exist before integration)

  **References**:

  **Primary modification target**:
  - `src/repl.lisp:722-763` — `start-repl` function. This is the integration site.
  - `src/repl.lisp:733-736` — `make-agent` call. Add `:hooks` parameter here.
  - `src/repl.lisp:756-762` — `handler-case` around `agent-run`. Expand with spinner/timing/interrupt.

  **Component References (from preceding tasks)**:
  - `src/repl/spinner.lisp` (Task 2) — `start-spinner`, `stop-spinner`, `update-spinner-message`
  - `src/repl.lisp` Ctrl+C handling (Task 4) — `*cancel-requested*`, `*last-interrupt-time*`, `install-interrupt-handler`
  - `src/repl.lisp` tool hooks (Task 5) — `make-tool-call-hook`
  - `src/repl.lisp` elapsed time (Task 6) — timing utility

  **Hook system reference**:
  - `src/agent/core.lisp:108-115` — `make-agent` accepts `:hooks` keyword. Hooks are an alist of `(hook-name . function)`.
  - `src/agent/core.lisp:121-128` — `run-hook` calls hooks by name
  - `src/agent/core.lisp:142` — `:before-step` hook called with step count
  - `src/agent/core.lisp:165` — `:on-tool-call` hook called with tool-call struct
  - `src/agent/core.lisp:182` — `:after-step` hook called with response message

  **Acceptance Criteria**:

  - [ ] `make-agent` call includes `:hooks` with `:on-tool-call`, `:before-step`, `:after-step`
  - [ ] Spinner starts when LLM call begins, stops when response arrives
  - [ ] Tool names displayed during multi-tool agent runs
  - [ ] Elapsed time shown after response
  - [ ] Single Ctrl+C cancels LLM call and returns to prompt
  - [ ] Double Ctrl+C exits REPL cleanly (spinner stopped, history saved)
  - [ ] All existing tests pass
  - [ ] REPL works with AND without cl-readline loaded

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: REPL starts and accepts input without errors
    Tool: interactive_bash (tmux)
    Preconditions: SBCL installed, sibyl loadable
    Steps:
      1. tmux new-session -d -s sibyl-test
      2. Send: sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sibyl:with-config () (sibyl:start-repl :client nil))'
      3. Wait for: "sibyl" in output (prompt appears, timeout: 15s)
      4. Send: /help
      5. Wait for: "Sibyl REPL commands" in output (timeout: 5s)
      6. Send: /quit
      7. Wait for: "Goodbye" in output (timeout: 5s)
      8. Assert: Process exits cleanly
    Expected Result: REPL starts, accepts commands, exits cleanly
    Evidence: Terminal output captured

  Scenario: System loads and all tests pass
    Tool: Bash (sbcl)
    Preconditions: System and tests loadable
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(assert (every #\'"'"'fiveam::test-passed-p (fiveam:run :sibyl-tests)))' --eval '(format t "~%TESTS-OK~%")' --quit
      2. Assert: stdout contains "TESTS-OK"
    Expected Result: Zero test failures after integration
    Evidence: Test output captured
  ```

  **Commit**: YES
  - Message: `feat(repl): integrate spinner, hooks, Ctrl+C, and timing into REPL loop`
  - Files: `src/repl.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(assert (every #\'"'"'fiveam::test-passed-p (fiveam:run :sibyl-tests)))' --quit`

---

- [x] 8. Register rich-repl-test.lisp in sibyl.asd and expand tests

  **What to do**:
  - Register `tests/rich-repl-test.lisp` in `sibyl.asd` under `sibyl/tests` components (it exists but is NOT registered)
  - Add `rich-repl-tests` suite as a child of `sibyl-tests` suite (currently standalone)
  - Add FiveAM tests for all Phase 1 features:
    - **Spinner tests**: start/stop lifecycle, thread termination, message update
    - **Ctrl+C tests**: Cancel flag mechanism, double-press timestamp logic, `llm-cancelled` condition
    - **Tool hook tests**: Hook function produces expected output with mock tool-call
    - **Elapsed time tests**: Timing accuracy, format output
    - **cl-readline probe test**: `readline-available-p` returns correct value
    - **defvar tests**: Variables bound with correct initial values
  - All tests must be non-interactive (no terminal required)
  - Use `(let ((*use-colors* nil)) ...)` in tests that check output format
  - Use `with-output-to-string` to capture and assert on output

  **Must NOT do**:
  - Do not add tests that require actual LLM calls
  - Do not add tests that require interactive terminal (real readline, real spinner animation)
  - Do not modify existing test files

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Comprehensive test suite covering multiple modules, requires understanding of all Phase 1 components
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `playwright`: No browser interaction

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 3 (after all other Phase 1 tasks)
  - **Blocks**: None (final Phase 1 task)
  - **Blocked By**: Tasks 1-7 (all Phase 1 components must exist to test)

  **References**:

  **Existing test to expand**:
  - `tests/rich-repl-test.lisp` — 31 lines, 3 tests. Currently defines own package `sibyl.tests.rich-repl` and standalone `rich-repl-tests` suite. Needs to join `sibyl-tests` parent suite.

  **Test pattern references**:
  - `tests/repl-test.lisp:5-8` — `def-suite repl-tests` pattern (note: also NOT a child of `sibyl-tests` — existing issue)
  - `tests/suite.lisp` — Main test suite definition. `rich-repl-tests` should be registered as child here.
  - `tests/tools-test.lisp` — Example of well-structured test file with section comments
  - `sibyl.asd:44-61` — Test system components list. Add `(:file "rich-repl-test")` here.

  **Components to test**:
  - `src/repl/spinner.lisp` — `start-spinner`, `stop-spinner`, `spinner-active-p`
  - `src/repl.lisp` — `*cancel-requested*`, `*last-interrupt-time*`, tool hook functions, timing utility
  - `src/conditions.lisp` — `llm-cancelled` condition

  **Acceptance Criteria**:

  - [ ] `tests/rich-repl-test.lisp` registered in `sibyl.asd`
  - [ ] `rich-repl-tests` is a child suite of `sibyl-tests`
  - [ ] ≥15 test cases covering all Phase 1 features
  - [ ] `(asdf:test-system :sibyl)` runs all tests including rich-repl-tests
  - [ ] All tests pass with zero failures

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Full test suite includes rich-repl-tests
    Tool: Bash (sbcl)
    Preconditions: System and tests loadable
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(let ((results (fiveam:run :sibyl-tests))) (format t "~%TOTAL: ~a~%" (length results)) (format t "PASS: ~a~%" (count t results :key #\'"'"'fiveam::test-passed-p)) (format t "FAIL: ~a~%" (count nil results :key #\'"'"'fiveam::test-passed-p)) (unless (every #\'"'"'fiveam::test-passed-p results) (fiveam:explain! results) (error "Tests failed")))' --eval '(format t "~%ALL-TESTS-OK~%")' --quit
      2. Assert: stdout contains "ALL-TESTS-OK"
      3. Assert: FAIL count is 0
      4. Assert: TOTAL count is greater than before (includes new rich-repl tests)
    Expected Result: All tests pass including new rich-repl tests
    Evidence: Test output captured with counts
  ```

  **Commit**: YES
  - Message: `test(repl): register and expand rich-repl-test with spinner, Ctrl+C, and hook tests`
  - Files: `tests/rich-repl-test.lisp`, `sibyl.asd`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(assert (every #\'"'"'fiveam::test-passed-p (fiveam:run :sibyl-tests)))' --quit`

---

### Phase 2: LLM Response Streaming

---

- [x] 9. Add SSE streaming support to HTTP client

  **What to do**:
  - Add new function `http-post-stream` to `src/llm/client.lisp` alongside existing `http-post-json`
  - Uses `dex:post` with `:want-stream t` to get a stream response
  - Implement SSE (Server-Sent Events) line parser:
    - Read lines from stream
    - Parse `event: <type>` and `data: <json>` lines
    - Handle `[DONE]` sentinel (OpenAI) and `event: message_stop` (Anthropic)
    - Call a provided callback function with each parsed event
  - Function signature: `(http-post-stream url headers body &key on-event on-error on-done)`
    - `on-event`: `(lambda (event-type data-hash-table) ...)` — called per SSE event
    - `on-error`: `(lambda (condition) ...)` — called on stream/parse errors
    - `on-done`: `(lambda () ...)` — called when stream completes
  - Handle connection drops gracefully (stream closes mid-response)
  - Add `flexi-streams` dependency if needed for stream decoding (dexador may already handle this)
  - Add `llm-stream-error` condition to `conditions.lisp` under `llm-error`

  **Must NOT do**:
  - Do not modify existing `http-post-json` — streaming is a separate codepath
  - Do not add retry/reconnection logic for dropped streams
  - Do not handle non-SSE streaming formats (only SSE)

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: SSE protocol implementation, stream handling, error recovery — requires careful protocol understanding
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `playwright`: No browser interaction

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 4 (first Phase 2 task)
  - **Blocks**: Tasks 10, 11
  - **Blocked By**: Phase 1 complete (Task 8)

  **References**:

  **Pattern References**:
  - `src/llm/client.lisp:85-108` — Existing `http-post-json`. Follow same error handling pattern (`dex:http-request-failed`, status code checks).
  - `src/llm/client.lisp:88` — `yason:encode` for request body
  - `src/llm/client.lisp:107` — `yason:parse` for JSON parsing of data payloads

  **Condition References**:
  - `src/conditions.lisp:28-58` — `llm-error` hierarchy. Add `llm-stream-error` following same pattern.

  **External References**:
  - SSE protocol: Lines starting with `event:`, `data:`, `id:`, or empty line (event boundary)
  - Dexador `:want-stream t`: Returns a `flexi-streams:flexi-io-stream`. Read with `read-line`.
  - Anthropic SSE events: `event: message_start`, `event: content_block_start`, `event: content_block_delta`, `event: content_block_stop`, `event: message_delta`, `event: message_stop`
  - OpenAI SSE events: `data: {"choices":[{"delta":{"content":"..."}}]}` terminated by `data: [DONE]`

  **Acceptance Criteria**:

  - [ ] `http-post-stream` function defined in `src/llm/client.lisp`
  - [ ] SSE parser correctly splits event/data lines
  - [ ] `on-event` callback receives parsed JSON hash-tables
  - [ ] `on-done` callback fires when stream completes
  - [ ] `on-error` callback fires on stream errors
  - [ ] `llm-stream-error` condition defined

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: SSE parser handles Anthropic event format
    Tool: Bash (sbcl)
    Preconditions: System loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(let ((events nil)) (sibyl.llm::parse-sse-lines (format nil "event: content_block_delta~%data: {\"type\":\"content_block_delta\",\"delta\":{\"type\":\"text_delta\",\"text\":\"Hello\"}}~%~%event: message_stop~%data: {}~%~%") (lambda (evt data) (push (list evt data) events)) (lambda () nil)) (assert (= 2 (length events))) (format t "~%SSE-PARSE-OK~%"))' --quit
      2. Assert: stdout contains "SSE-PARSE-OK"
    Expected Result: Parser extracts both events from SSE stream
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `feat(llm): add SSE streaming support to HTTP client`
  - Files: `src/llm/client.lisp`, `src/conditions.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(assert (every #\'"'"'fiveam::test-passed-p (fiveam:run :sibyl-tests)))' --quit`

---

- [x] 10. Add Anthropic streaming provider

  **What to do**:
  - Add streaming variants of `complete` and `complete-with-tools` for Anthropic client in `src/llm/providers.lisp`
  - **Mechanism**: Dynamic variable `*streaming-text-callback*` defined in `sibyl.llm` package (or `sibyl.repl` that providers can see). When non-nil, providers use streaming; when nil (default), they use the existing blocking path.
  - The REPL (Task 11) binds this variable around the `agent-run` call: `(let ((*streaming-text-callback* #'write-char-to-stdout)) (agent-run agent input))`. This propagates into `agent-step → complete-with-tools` because `agent-step` calls `complete-with-tools` in the same thread (dynamic binding is thread-local, same thread as REPL).
  - Provider implementation: at the top of `complete`/`complete-with-tools` methods, check `(when *streaming-text-callback* ...)` and branch to streaming path.
  - When streaming is enabled:
    - Add `"stream": true` to the Anthropic API request body
    - Use `http-post-stream` (from Task 9) instead of `http-post-json`
    - Parse Anthropic-specific SSE events:
      - `content_block_delta` with `delta.type == "text_delta"` → call `(funcall *streaming-text-callback* delta.text)`
      - `content_block_delta` with `delta.type == "input_json_delta"` → accumulate tool input JSON
      - `content_block_stop` → finalize current block
      - `message_delta` → extract `stop_reason`
      - `message_stop` → signal completion
  - Reconstruct full response message at the end (same format as non-streaming) so `agent-step` works unchanged
  - Export `*streaming-text-callback*` from `sibyl.llm` package

  **Must NOT do**:
  - Do not modify existing non-streaming `complete`/`complete-with-tools` methods
  - Do not modify `agent-step` or `agent-run`
  - Do not handle OpenAI streaming (that's Task 11)

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: Complex API integration with streaming protocol, event reconstruction, and maintaining backward compatibility
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `playwright`: No browser interaction

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 4 (sequential after Task 9)
  - **Blocks**: Task 11
  - **Blocked By**: Task 9 (SSE client must exist)

  **References**:

  **Pattern References**:
  - `src/llm/providers.lisp:151-182` — Existing Anthropic `complete-with-tools` method. This is the non-streaming version to model after.
  - `src/llm/providers.lisp:1-50` — Anthropic client class definition and helper functions
  - `src/llm/client.lisp` — `http-post-stream` (from Task 9) for making streaming requests

  **Message reconstruction reference**:
  - `src/llm/providers.lisp` — `parse-anthropic-response` function. Streaming must reconstruct equivalent output.
  - `src/llm/message.lisp` — Message data structures that `agent-step` expects

  **External References**:
  - Anthropic streaming docs: https://docs.anthropic.com/en/api/messages-streaming
  - Event types: `message_start`, `content_block_start`, `content_block_delta`, `content_block_stop`, `message_delta`, `message_stop`
  - Text delta: `{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"..."}}`

  **Acceptance Criteria**:

  - [ ] `*streaming-text-callback*` dynamic variable defined and exported from `sibyl.llm`
  - [ ] When `*streaming-text-callback*` is non-nil, Anthropic provider uses streaming
  - [ ] Text deltas invoke callback with each text chunk: `(funcall *streaming-text-callback* text)`
  - [ ] Tool calls are properly reconstructed from streamed input_json_delta events
  - [ ] Final reconstructed message is identical format to non-streaming response
  - [ ] Non-streaming path completely unchanged when `*streaming-text-callback*` is nil

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Anthropic streaming complete method exists and is callable
    Tool: Bash (sbcl)
    Preconditions: System loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(assert (fboundp (quote sibyl.llm::complete-streaming))) (format t "~%STREAM-FN-OK~%")' --quit
      2. Assert: stdout contains "STREAM-FN-OK"
    Expected Result: Streaming function is defined
    Evidence: Terminal output captured

  Scenario: Non-streaming path still works (regression check)
    Tool: Bash (sbcl)
    Preconditions: System loaded, API key available
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sibyl:with-config () (let* ((client (sibyl:make-anthropic-client)) (response (sibyl.llm:complete client (list (sibyl.llm:user-message "Say hi"))))) (assert (stringp (sibyl.llm:message-content response))) (format t "~%NON-STREAM-OK~%")))' --quit
      2. Assert: stdout contains "NON-STREAM-OK"
    Expected Result: Existing non-streaming API unchanged
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `feat(llm): add Anthropic streaming support with SSE event parsing`
  - Files: `src/llm/providers.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(assert (every #\'"'"'fiveam::test-passed-p (fiveam:run :sibyl-tests)))' --quit`

---

- [x] 11. Add OpenAI streaming + integrate streaming display into REPL

  **What to do**:
  - Add streaming variant for OpenAI client in `src/llm/providers.lisp`:
    - Add `"stream": true` to OpenAI request body
    - Parse OpenAI SSE: `data: {"choices":[{"delta":{"content":"..."}}]}`
    - Handle `data: [DONE]` sentinel
    - Reconstruct full response message at end
  - Integrate streaming into REPL (`src/repl.lisp`):
    - When streaming mode is active, bind `sibyl.llm:*streaming-text-callback*` around the `agent-run` call:
      ```lisp
      (let ((sibyl.llm:*streaming-text-callback*
             (when *stream-enabled*
               (lambda (text) (write-string text *standard-output*) (force-output)))))
        (agent-run agent input))
      ```
    - This causes providers to stream tokens directly to stdout while `agent-run` executes
    - Flow: user input → start spinner (initial connection) → first token arrives (stop spinner, start printing) → tokens stream → `agent-run` returns → show elapsed time
    - Spinner for initial connection: before first token, spinner runs; callback stops spinner on first text chunk
    - Tool call handling: between tool calls, re-show spinner while waiting for next LLM response
  - Add `:stream` keyword to `start-repl` to enable/disable streaming (default: `t` when available)
  - Add dynamic variable `*stream-enabled*` in `sibyl.repl` (default `t`)

  **Must NOT do**:
  - Do not remove spinner entirely — it's still needed for initial connection and tool execution waits
  - Do not modify `agent-step` or `agent-run` in `core.lisp`
  - Do not break non-streaming fallback path

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: Final integration of streaming across provider + REPL layers, complex state management (spinner → streaming → spinner → streaming)
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `playwright`: No browser interaction

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 5 (final task)
  - **Blocks**: None (final task)
  - **Blocked By**: Task 10 (Anthropic streaming must work first)

  **References**:

  **Pattern References**:
  - `src/llm/providers.lisp:270+` — Existing OpenAI `complete`/`complete-with-tools` methods
  - `src/llm/providers.lisp` — Anthropic streaming (from Task 10) as pattern for OpenAI
  - `src/repl.lisp:756-762` — Current agent-run call site. Needs streaming integration.
  - `src/repl/spinner.lisp` — Spinner module for initial connection phase

  **External References**:
  - OpenAI streaming: `data: {"id":"...","choices":[{"index":0,"delta":{"content":"Hello"}}]}`
  - OpenAI stream end: `data: [DONE]`

  **Acceptance Criteria**:

  - [ ] OpenAI streaming works with same callback pattern as Anthropic
  - [ ] REPL displays tokens as they arrive (no waiting for full response)
  - [ ] Spinner shows during initial connection (before first token)
  - [ ] Spinner shows between tool calls and subsequent LLM calls
  - [ ] `:stream` keyword on `start-repl` controls streaming behavior
  - [ ] Non-streaming fallback still works when `:stream nil`
  - [ ] All tests pass

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: REPL starts with streaming enabled (Anthropic)
    Tool: interactive_bash (tmux)
    Preconditions: SBCL installed, Anthropic API key set, sibyl loadable
    Steps:
      1. tmux new-session -d -s sibyl-stream
      2. Send: sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sibyl:with-config () (sibyl:start-repl :client (sibyl:make-anthropic-client) :stream t))'
      3. Wait for: "sibyl" prompt (timeout: 15s)
      4. Send: Say just "hello" and nothing else.
      5. Wait for: "hello" in output (timeout: 30s) — response streams in
      6. Assert: Response appeared (tokens streamed)
      7. Assert: Elapsed time displayed after response
      8. Send: /quit
      9. Wait for: "Goodbye" (timeout: 5s)
    Expected Result: Streaming response appears token-by-token, then elapsed time shows
    Evidence: Terminal output captured

  Scenario: Non-streaming fallback works
    Tool: interactive_bash (tmux)
    Preconditions: SBCL installed, API key set
    Steps:
      1. tmux new-session -d -s sibyl-nostream
      2. Send: sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sibyl:with-config () (sibyl:start-repl :client (sibyl:make-anthropic-client) :stream nil))'
      3. Wait for: "sibyl" prompt (timeout: 15s)
      4. Send: Say just "hi"
      5. Wait for: response (timeout: 30s) — appears all at once after spinner
      6. Send: /quit
    Expected Result: Response appears as complete block (non-streaming), spinner was visible
    Evidence: Terminal output captured

  Scenario: All tests pass after full integration
    Tool: Bash (sbcl)
    Preconditions: System and tests loadable
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(let ((results (fiveam:run :sibyl-tests))) (format t "~%TOTAL: ~a PASS: ~a FAIL: ~a~%" (length results) (count t results :key #\'"'"'fiveam::test-passed-p) (count nil results :key #\'"'"'fiveam::test-passed-p)) (assert (every #\'"'"'fiveam::test-passed-p results)))' --eval '(format t "~%FINAL-OK~%")' --quit
      2. Assert: stdout contains "FINAL-OK"
      3. Assert: FAIL is 0
    Expected Result: Complete test suite passes
    Evidence: Test output with counts captured
  ```

  **Commit**: YES
  - Message: `feat(repl): add OpenAI streaming and integrate token-by-token display into REPL`
  - Files: `src/llm/providers.lisp`, `src/repl.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(assert (every #\'"'"'fiveam::test-passed-p (fiveam:run :sibyl-tests)))' --quit`

---

## Commit Strategy

| After Task | Message | Key Files | Verification |
|------------|---------|-----------|--------------|
| 1 | `fix(repl): add missing defvar declarations` | `src/repl.lisp` | `(asdf:test-system :sibyl)` |
| 2 | `feat(repl): add spinner animation module` | `src/repl/spinner.lisp`, `sibyl.asd`, `src/packages.lisp` | `(asdf:load-system :sibyl)` |
| 3 | `feat(repl): add optional cl-readline integration` | `src/repl.lisp`, `sibyl.asd`, `src/packages.lisp` | `(asdf:test-system :sibyl)` |
| 4 | `feat(repl): add Ctrl+C handling with cooperative cancellation` | `src/conditions.lisp`, `src/repl.lisp` | `(asdf:test-system :sibyl)` |
| 5 | `feat(repl): add tool call display hook functions` | `src/repl.lisp` | `(asdf:test-system :sibyl)` |
| 6 | `feat(repl): add elapsed time display` | `src/repl.lisp` | `(asdf:test-system :sibyl)` |
| 7 | `feat(repl): integrate spinner, hooks, Ctrl+C, timing into REPL loop` | `src/repl.lisp` | `(asdf:test-system :sibyl)` |
| 8 | `test(repl): register and expand rich-repl-test` | `tests/rich-repl-test.lisp`, `sibyl.asd` | `(asdf:test-system :sibyl)` |
| 9 | `feat(llm): add SSE streaming to HTTP client` | `src/llm/client.lisp`, `src/conditions.lisp` | `(asdf:test-system :sibyl)` |
| 10 | `feat(llm): add Anthropic streaming support` | `src/llm/providers.lisp` | `(asdf:test-system :sibyl)` |
| 11 | `feat(repl): add OpenAI streaming + REPL streaming display` | `src/llm/providers.lisp`, `src/repl.lisp` | `(asdf:test-system :sibyl)` |

---

## Success Criteria

### Verification Commands
```bash
# All tests pass
sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
     --eval '(assert (every #'"'"'fiveam::test-passed-p (fiveam:run :sibyl-tests)))' \
     --eval '(format t "ALL TESTS PASS~%")' --quit

# System loads without warnings
sbcl --eval '(handler-bind ((warning (lambda (w) (format *error-output* "WARNING: ~a~%" w)))) (ql:quickload :sibyl :silent t))' \
     --eval '(format t "CLEAN LOAD~%")' --quit

# REPL starts and responds (manual smoke test with API key)
sbcl --eval '(ql:quickload :sibyl :silent t)' \
     --eval '(sibyl:with-config () (sibyl:start-repl :client (sibyl:make-anthropic-client)))'
```

### Final Checklist
- [x] All "Must Have" features present and working
- [x] All "Must NOT Have" guardrails respected (no tab completion, no markdown rendering, etc.)
- [x] All 11 tasks committed with passing tests (1559 checks, 100% pass)
- [x] `src/agent/core.lisp` has ZERO modifications (all enrichment via hooks)
- [x] `sibyl` system loads without cl-readline (graceful degradation)
- [x] Spinner, Ctrl+C, tool display, history, timing all work together in REPL
- [x] Phase 2: Streaming works for both Anthropic and OpenAI
- [x] Phase 2: Spinner transitions smoothly to streaming display
