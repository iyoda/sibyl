# REPL Display Overhaul — Rich Token & Tool Status

## TL;DR

> **Quick Summary**: Comprehensive redesign of Sibyl's REPL display experience — replacing the minimal 1-line footer with a rich, conditionally-expanding status display showing per-turn cost, tool execution timing, cache savings, thinking tokens, and context window usage. All UI text unified to English.
> 
> **Deliverables**:
> - New display infrastructure module (`src/repl/display.lisp`)
> - Rich per-turn footer with conditional expansion
> - Tool execution timing + completion feedback
> - Per-turn and session cost display
> - Cache savings visualization
> - Thinking token display
> - Context window usage percentage
> - Enhanced `/tokens` session summary
> - English-unified UI strings
> 
> **Estimated Effort**: Medium-Large
> **Parallel Execution**: YES — 3 waves
> **Critical Path**: Task 1 → Task 4/5 → Task 7

---

## Context

### Original Request
User wants to improve the REPL experience by redesigning token consumption and tool execution status displays. Currently the REPL shows minimal information: a single dim line after each turn and basic tool call notifications with no timing or result feedback.

### Interview Summary
**Key Discussions**:
- **All 7 improvement areas selected**: cost, tools, cache, thinking, context, footer, session summary
- **Rich display style**: Claude Code-inspired, multi-line, color-coded
- **Conditional expansion**: Base 2 lines, extra lines for cache/thinking when present
- **Tool display**: Spinner during execution → overwrite with result (time, size, status) on completion
- **Cost**: Accurate per-model calculation using existing `pricing.lisp` infrastructure
- **Context window**: Percentage only (compact)
- **Language**: English unified (convert 5 Japanese UI strings)
- **Test strategy**: TDD (RED-GREEN-REFACTOR)

**Research Findings**:
- `estimate-cost-usd` already exists with per-model pricing (pricing.lisp:49-68)
- `model-context-window` available per model on `enhanced-model-config` (model-selector.lisp:433-434)
- `decompose-savings` exists for cache savings breakdown (pricing.lisp:89-145)
- Hooks: `:before-step`, `:after-step`, `:on-tool-call`, `:on-error` — NO `:on-tool-result`
- Spinner is thread-safe via `*spinner-output-lock*` (repl.lisp:1128)
- Color: `format-colored-text` supports 8 colors + manual dim via ANSI codes
- Token tracker: input/output/cache-read/cache-write/request-count — NO cost accumulator, NO thinking tokens
- Tool execution: NO timing captured in `execute-tool` or `execute-tool-calls-parallel`
- `repl.lisp` is 1,626 lines — must not grow further

### Metis Review
**Identified Gaps** (addressed):
- **Thinking tokens not separate in API usage**: Bundled into `output_tokens`. Resolved: Check API response for separate field; fallback to text-length estimation `(ceiling (length text) 4)`
- **Session cost across model switching**: Token tracker doesn't record per-request model. Resolved: Accumulate per-turn cost in token-tracker (simple float accumulator)
- **No model→context-window lookup utility**: Resolved: Create `context-window-for-model` helper in model-selector.lisp
- **Parallel tool display**: Overwrite pattern doesn't work with concurrent tools. Resolved: Keep sequential result lines with spinner lock
- **repl.lisp bloat risk**: Resolved: Extract-first rule — new display module created BEFORE any display changes
- **Japanese string locations**: Exactly 5 user-visible strings identified (3× "考え中...", 2× "を実行中...")
- **Existing test breakage**: `rich-repl-test.lisp` checks for "考え中" — tests updated atomically with string changes
- **Provider-specific display**: Anthropic (full), OpenAI (partial cache), Ollama (local/no cost) — all handled conditionally

---

## Work Objectives

### Core Objective
Transform the REPL from a minimal text interface into a rich, informative development experience that shows the user exactly what's happening — what it costs, how cache helps, what tools are doing, how much context is used — with a clean, Claude Code-inspired aesthetic.

### Concrete Deliverables
- `src/repl/display.lisp` — New display infrastructure module with all formatting functions
- Modified `src/agent/core.lisp` — `:on-tool-result` hook + tool timing
- Modified `src/llm/token-tracker.lisp` — Cost accumulator + thinking tokens
- Modified `src/llm/model-selector.lisp` — `context-window-for-model` utility
- Modified `src/repl.lisp` — Hook wiring + call site updates (minimal changes)
- Modified `src/packages.lisp` — New package exports
- Modified `sibyl.asd` — Module registration
- New test file `tests/repl-display-test.lisp` — Comprehensive display tests
- Updated `tests/suite.lisp` — New suite classification

### Definition of Done
- [x] All existing tests pass (`asdf:test-system :sibyl` → T)
- [x] All new display tests pass
- [x] Per-turn footer shows model, time, tokens, cost
- [x] Tool execution shows timing and result size on completion
- [x] Cache savings visible when cache tokens present
- [x] Thinking tokens visible when thinking used
- [x] Context window percentage visible
- [x] `/tokens` shows rich session summary with costs
- [x] All UI strings in English
- [x] `*use-colors*` = nil produces clean plain-text fallback

### Must Have
- Per-turn cost display using `estimate-cost-usd`
- Tool execution timing (wall-clock seconds)
- Tool result feedback (size in bytes, success/failure indicator)
- Cache savings line (conditional, only when cache-read > 0)
- Thinking token line (conditional, only when thinking used)
- Context window percentage
- Redesigned multi-line footer
- Enhanced `/tokens` command
- English-unified UI strings
- Plain-text fallback for no-color mode
- Thread-safe display with `*spinner-output-lock*`

### Must NOT Have (Guardrails)
- **MUST NOT** add new display code to `repl.lisp` — all goes in `src/repl/display.lisp` (except hook wiring in `start-repl` and REPL loop call site)
- **MUST NOT** change `agent-step` control flow — only add `run-hook` calls and timing measurement
- **MUST NOT** convert Japanese in docstrings, comments, or log messages — only 5 user-visible UI strings
- **MUST NOT** add background colors, 256-color, or truecolor support — only add `:dim` and `:bold` to existing function
- **MUST NOT** add result content preview or syntax highlighting to tool result display
- **MUST NOT** add persistence, history, or export to `/tokens` command
- **MUST NOT** change tool execution order, error handling, or memory management in agent core
- **MUST NOT** create tests requiring real API calls or REPL interaction
- **MUST NOT** change tool protocol (protocol.lisp) — timing wraps AROUND execution in agent-step
- **MUST NOT** handle terminal width — let terminal wrap naturally
- **MUST NOT** show negative cache savings as savings — show write overhead separately or omit
- **MUST NOT** refactor/move existing functions out of repl.lisp (future task)

---

## Verification Strategy

> **UNIVERSAL RULE: ZERO HUMAN INTERVENTION**
>
> ALL tasks are verifiable WITHOUT any human action.
> ALL verification is executed by the agent using commands.

### Test Decision
- **Infrastructure exists**: YES (FiveAM)
- **Automated tests**: TDD (RED-GREEN-REFACTOR)
- **Framework**: FiveAM via `(asdf:test-system :sibyl)`

### TDD Workflow Per Task

Each TODO follows RED-GREEN-REFACTOR:
1. **RED**: Write failing test first in `tests/repl-display-test.lisp`
2. **GREEN**: Implement minimum code to pass
3. **REFACTOR**: Clean up while keeping green

### Agent-Executed QA Scenarios (MANDATORY — ALL tasks)

**Verification Tool by Deliverable Type:**

| Type | Tool | How Agent Verifies |
|------|------|-------------------|
| Display formatting | Bash (sbcl --eval) | Call function, compare output string |
| Hook integration | Bash (sbcl --eval) | Create mock agent, verify hook fires |
| Token tracking | Bash (sbcl --eval) | Create tracker, add usage, check fields |
| Full test suite | Bash (sbcl --eval) | Load tests, run system, check result = T |

---

## Execution Strategy

### Parallel Execution Waves

```
Wave 1 (Start Immediately — Foundation):
├── Task 1: Display infrastructure module (new file, no deps)
├── Task 2: Tool timing + on-tool-result hook (agent core)
└── Task 3: Token tracker enhancements (cost + thinking)

Wave 2 (After Wave 1 — Display Features):
├── Task 4: Tool display overhaul + English strings (depends: 1, 2)
├── Task 5: Per-turn footer redesign (depends: 1, 3)
└── Task 6: Session summary enhancement (depends: 1, 3)

Wave 3 (After Wave 2 — Integration):
└── Task 7: Integration verification + context window (depends: all)
```

### Dependency Matrix

| Task | Depends On | Blocks | Can Parallelize With |
|------|------------|--------|---------------------|
| 1 | None | 4, 5, 6, 7 | 2, 3 |
| 2 | None | 4, 7 | 1, 3 |
| 3 | None | 5, 6, 7 | 1, 2 |
| 4 | 1, 2 | 7 | 5, 6 |
| 5 | 1, 3 | 7 | 4, 6 |
| 6 | 1, 3 | 7 | 4, 5 |
| 7 | 4, 5, 6 | None | None (final) |

### Agent Dispatch Summary

| Wave | Tasks | Recommended Dispatch |
|------|-------|---------------------|
| 1 | 1, 2, 3 | 3 parallel agents |
| 2 | 4, 5, 6 | 3 parallel agents |
| 3 | 7 | 1 agent (integration) |

---

## TODOs

- [x] 1. Display Infrastructure Module

  **What to do**:

  Create `src/repl/display.lisp` as the central display formatting module. This is the foundation that all other tasks depend on.

  **Implementation steps (TDD)**:

  RED: Write tests for each formatting function first:
  - `format-cost` — format USD cost as string (e.g., `"$0.012"`, `"$1.23"`)
  - `format-tokens-compact` — format token count with commas (e.g., `"1,234"`)
  - `format-bytes-human` — format byte count (e.g., `"15.2 KB"`, `"1,847 B"`)
  - `format-duration` — format seconds (e.g., `"0.02s"`, `"1.2s"`, `"2m 3s"`)
  - `format-context-percentage` — format context usage with color (e.g., green `"42%"`, yellow `"75%"`, red `"92%"`)
  - `format-separator-line` — horizontal rule using `─` character, width-adaptive
  - `format-dim-text` — wrapper for dim ANSI output, respects `*use-colors*`
  - `format-bold-text` — wrapper for bold ANSI output, respects `*use-colors*`
  - `format-colored` — enhanced `format-colored-text` supporting `:dim` and `:bold` modifiers

  GREEN: Implement each function.

  REFACTOR: Extract common ANSI escape patterns.

  **Package**: Define `sibyl.repl.display` package in `packages.lisp` with explicit exports.

  **Registration**: Add `(:file "display" :depends-on ("spinner"))` under `:repl-module` in `sibyl.asd`.

  **Must NOT do**:
  - Move or modify existing functions in `repl.lisp`
  - Add 256-color or truecolor support
  - Create a theming system

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: New module creation with straightforward formatting functions
  - **Skills**: []
    - No specialized skills needed — pure Lisp formatting code

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Tasks 2, 3)
  - **Blocks**: Tasks 4, 5, 6, 7
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `src/repl/spinner.lisp` — Existing repl submodule pattern (package definition, ASDF registration)
  - `src/repl.lisp:186-198` — Existing `format-colored-text` function (color code mapping, `*use-colors*` gating)
  - `src/repl.lisp:1232-1253` — Existing `format-elapsed-time` (dim ANSI code `\e[2m`, format pattern)
  - `src/repl.lisp:238-267` — Readline-aware ANSI wrapping with `\001`/`\002` escape sequences

  **API/Type References**:
  - `src/llm/pricing.lisp:49-68` — `estimate-cost-usd` return plist format: `(:total N :input N :output N :cache-write N :cache-read N)`
  - `src/llm/token-tracker.lisp:5-11` — `token-tracker` struct fields for display data
  - `src/repl.lisp:47` — `*use-colors*` global variable that gates all ANSI output

  **Test References**:
  - `tests/anthropic-streaming-test.lisp` — Recent test file pattern (package definition, suite classification, FiveAM `def-suite` + `in-suite`)
  - `tests/suite.lisp` — `*safe-suites*` list for new suite registration

  **Acceptance Criteria**:

  **TDD Tests:**
  - [ ] Test file created: `tests/repl-display-test.lisp`
  - [ ] Suite `repl-display-tests` created and added to `*safe-suites*`
  - [ ] `format-cost`: `0.0123 → "$0.012"`, `1.5 → "$1.500"`, `0.0 → "$0.000"`
  - [ ] `format-tokens-compact`: `1234 → "1,234"`, `0 → "0"`, `1000000 → "1,000,000"`
  - [ ] `format-bytes-human`: `500 → "500 B"`, `15234 → "14.9 KB"`, `1048576 → "1.0 MB"`
  - [ ] `format-duration`: `0.02 → "0.02s"`, `1.5 → "1.50s"`, `125.0 → "2m 5s"`
  - [ ] `format-context-percentage`: `0.42 → "42%"` (green), `0.75 → "75%"` (yellow), `0.92 → "92%"` (red)
  - [ ] All functions tested with `*use-colors*` = nil (plain text, no ANSI codes)
  - [ ] `(ql:quickload :sibyl)` succeeds with new module loaded
  - [ ] `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit` → result T

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Display module loads without errors
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl source compiled
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(format t "~a" (fboundp (quote sibyl.repl.display:format-cost)))' --quit
      2. Assert: output contains "T"
    Expected Result: All display functions are defined and exported
    Evidence: Command output captured

  Scenario: Cost formatting produces correct output
    Tool: Bash (sbcl --eval)
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(let ((sibyl.repl::*use-colors* nil)) (format t "~a" (sibyl.repl.display:format-cost 0.0123)))' --quit
      2. Assert: output is "$0.012"
    Expected Result: Cost formatted to 3 decimal places with $ prefix
    Evidence: Command output captured

  Scenario: Plain text fallback works without colors
    Tool: Bash (sbcl --eval)
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(let ((sibyl.repl::*use-colors* nil)) (format t "~a" (sibyl.repl.display:format-context-percentage 0.85)))' --quit
      2. Assert: output contains "85%" and contains NO ESC character (char code 27)
    Expected Result: No ANSI escape codes in output
    Evidence: Command output captured
  ```

  **Commit**: YES
  - Message: `feat(repl): add display infrastructure module with formatting helpers`
  - Files: `src/repl/display.lisp`, `src/packages.lisp`, `sibyl.asd`, `tests/repl-display-test.lisp`, `tests/suite.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit`

---

- [x] 2. Tool Timing + on-tool-result Hook

  **What to do**:

  Add tool execution timing measurement and a new `:on-tool-result` hook to the agent core. This provides the data foundation for tool display improvements.

  **Implementation steps (TDD)**:

  RED: Write tests for:
  - `:on-tool-result` hook is called after tool execution with `(tool-call result elapsed-seconds)`
  - Timing measurement captures wall-clock seconds (> 0)
  - Hook fires for both successful and failed tool executions
  - Hook errors are caught (advisory, non-breaking)
  - Both sequential and parallel tool paths fire the hook

  GREEN: Modify `agent-step` in `core.lisp`:
  - Wrap `execute-tool-call` with `get-internal-real-time` timing in BOTH paths:
    - Sequential path (L236-242): Each tool call individually timed
    - Parallel path (L246-253): Each tool call individually timed within parallel execution
  - After each tool execution, call `(run-hook agent :on-tool-result tc result elapsed-seconds)`
  - `elapsed-seconds` = `(/ (- end start) internal-time-units-per-second)`

  REFACTOR: Extract timing wrapper if pattern repeats.

  **Must NOT do**:
  - Change agent-step control flow or error handling
  - Modify tool protocol (protocol.lisp)
  - Add timing to the tool registry or tool struct
  - Change the existing `:on-tool-call` hook behavior

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: Small, focused change to agent core with timing wrapper
  - **Skills**: []
    - No specialized skills needed

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Tasks 1, 3)
  - **Blocks**: Tasks 4, 7
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `src/agent/core.lisp:166-173` — `run-hook` implementation (advisory, errors caught with `handler-case`)
  - `src/agent/core.lisp:236-242` — Sequential tool execution path (`:on-tool-call` → `execute-tool-call`)
  - `src/agent/core.lisp:246-253` — Parallel tool execution path (`execute-tool-calls-parallel` → results)
  - `src/agent/core.lisp:37` — Hooks documentation: `:before-step :after-step :on-tool-call :on-error`

  **API/Type References**:
  - `src/tools/protocol.lisp` — `execute-tool-call` signature and return value (string result)
  - `src/llm/message.lisp` — `tool-call` struct (name, arguments, id)
  - `src/agent/core.lisp:10-41` — `agent` class definition with hooks slot

  **Test References**:
  - `tests/agent-test.lisp` — Existing agent tests (if any) for hook testing patterns
  - `tests/anthropic-streaming-test.lisp` — Mock pattern using `handler-case`

  **Acceptance Criteria**:

  **TDD Tests:**
  - [ ] Test suite `tool-timing-tests` created and added to `*safe-suites*` or `*unsafe-suites*`
  - [ ] Test: `:on-tool-result` hook fires after tool execution
  - [ ] Test: Hook receives tool-call object, result string, and elapsed-seconds (float > 0)
  - [ ] Test: Hook fires for error results (tool failure)
  - [ ] Test: Hook errors are caught and don't interrupt agent-step
  - [ ] Test: Timing measurement is wall-clock seconds (reasonable range 0.0-60.0)
  - [ ] `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit` → result T

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: on-tool-result hook fires with timing data
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl loaded
    Steps:
      1. Create a mock agent with :on-tool-result hook that captures args
      2. Execute a trivial tool call
      3. Assert: hook was called
      4. Assert: elapsed-seconds is a positive float
      5. Assert: result is a string
    Expected Result: Hook fires with all 3 arguments
    Evidence: REPL output captured

  Scenario: All existing tests still pass
    Tool: Bash (sbcl --eval)
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit
      2. Assert: result is T
    Expected Result: Zero regressions from hook addition
    Evidence: Test output captured
  ```

  **Commit**: YES
  - Message: `feat(agent): add on-tool-result hook with execution timing`
  - Files: `src/agent/core.lisp`, `tests/repl-display-test.lisp` (or separate test file), `tests/suite.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit`

---

- [x] 3. Token Tracker Enhancements

  **What to do**:

  Enhance the `token-tracker` struct with a cost accumulator and thinking token field. Add a `context-window-for-model` utility. These provide the data layer for footer and session summary improvements.

  **Implementation steps (TDD)**:

  RED: Write tests for:
  - New `token-tracker` fields: `cost-usd` (double-float, default 0.0d0), `thinking-tokens` (integer, default 0)
  - `tracker-add-usage` handles new `:thinking-tokens` key in usage plist (backward-compatible)
  - `tracker-add-cost` function accumulates cost per turn
  - `context-window-for-model` returns context window size for known models, fallback 200000
  - Backward compatibility: existing `tracker-add-usage` callers work without changes

  GREEN: Implement:
  - Add `cost-usd` and `thinking-tokens` fields to `token-tracker` defstruct
  - Extend `tracker-add-usage` to handle `:thinking-tokens` key (ignore if missing)
  - Create `tracker-add-cost` function: `(incf (token-tracker-cost-usd tracker) amount)`
  - Create `context-window-for-model` in `model-selector.lisp`:
    ```lisp
    (defun context-window-for-model (model-name)
      "Return context window size for MODEL-NAME. Falls back to 200000."
      ;; Lookup in *latest-model-tiers* by prefix match
      ...)
    ```
  - Export new functions from packages.lisp

  REFACTOR: Ensure clean integration with existing code.

  **Must NOT do**:
  - Change existing token-tracker field names or types
  - Break backward compatibility of `tracker-add-usage`
  - Add per-request model tracking (too complex for this scope)
  - Modify pricing.lisp

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: Struct extension + utility function, straightforward
  - **Skills**: []
    - No specialized skills needed

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Tasks 1, 2)
  - **Blocks**: Tasks 5, 6, 7
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `src/llm/token-tracker.lisp:5-11` — Current `token-tracker` struct definition
  - `src/llm/token-tracker.lisp:13-22` — `tracker-add-usage` function (pattern for adding new key handling)
  - `src/llm/token-tracker.lisp:24-32` — `tracker-cache-hit-rate` function (calculation pattern)

  **API/Type References**:
  - `src/llm/model-selector.lisp:430-435` — `enhanced-model-config` class with `context-window` slot
  - `src/llm/model-selector.lisp:450-535` — `*latest-model-tiers*` parameter with all model configs
  - `src/llm/pricing.lisp:49-68` — `estimate-cost-usd` return plist format
  - `src/llm/providers.lisp` — Usage plist format returned from `complete-with-tools`

  **Test References**:
  - `tests/cache-test.lisp` — Token tracker test patterns (if any)
  - `tests/anthropic-streaming-test.lisp:anthropic-thinking-config-tests` — Config/capability test pattern

  **Acceptance Criteria**:

  **TDD Tests:**
  - [ ] Test suite `token-tracker-enhancement-tests` created and added to `*safe-suites*`
  - [ ] Test: `make-token-tracker` creates struct with `cost-usd` = 0.0d0 and `thinking-tokens` = 0
  - [ ] Test: `tracker-add-usage` with `:thinking-tokens 500` increments thinking-tokens field
  - [ ] Test: `tracker-add-usage` WITHOUT `:thinking-tokens` key works (backward compat)
  - [ ] Test: `tracker-add-cost` accumulates correctly: 0.01 + 0.02 = 0.03
  - [ ] Test: `context-window-for-model` returns 200000 for "claude-opus-4-6"
  - [ ] Test: `context-window-for-model` returns 200000 for "claude-sonnet-4-6"
  - [ ] Test: `context-window-for-model` returns 131072 for "gpt-5-mini"
  - [ ] Test: `context-window-for-model` returns 200000 for unknown model (fallback)
  - [ ] `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit` → result T

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Token tracker backward compatibility
    Tool: Bash (sbcl --eval)
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(let ((tr (sibyl.llm::make-token-tracker))) (sibyl.llm::tracker-add-usage tr (list :input-tokens 100 :output-tokens 50)) (format t "in=~a out=~a think=~a cost=~a" (sibyl.llm::token-tracker-input-tokens tr) (sibyl.llm::token-tracker-output-tokens tr) (sibyl.llm::token-tracker-thinking-tokens tr) (sibyl.llm::token-tracker-cost-usd tr)))' --quit
      2. Assert: output is "in=100 out=50 think=0 cost=0.0d0" (or equivalent)
    Expected Result: New fields default to zero, existing usage still works
    Evidence: Command output captured

  Scenario: Context window lookup works for all providers
    Tool: Bash (sbcl --eval)
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(format t "opus=~a sonnet=~a gpt=~a unknown=~a" (sibyl.llm:context-window-for-model "claude-opus-4-6") (sibyl.llm:context-window-for-model "claude-sonnet-4-6") (sibyl.llm:context-window-for-model "gpt-5-mini") (sibyl.llm:context-window-for-model "unknown-model"))' --quit
      2. Assert: opus=200000, sonnet=200000, gpt=131072, unknown=200000
    Expected Result: Correct context window per model with fallback
    Evidence: Command output captured
  ```

  **Commit**: YES
  - Message: `feat(llm): enhance token-tracker with cost accumulator and thinking tokens`
  - Files: `src/llm/token-tracker.lisp`, `src/llm/model-selector.lisp`, `src/packages.lisp`, `tests/repl-display-test.lisp`, `tests/suite.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit`

---

- [x] 4. Tool Display Overhaul + English Strings

  **What to do**:

  Redesign tool execution display with timing and result feedback. Convert all user-visible Japanese UI strings to English. Replace the current `🔧 shell を実行中...` with a richer pattern that shows a spinner during execution and overwrites with a result line on completion.

  **Target display format**:
  ```
  During execution:
    🔧 read-file ...             ← spinner on this line

  After completion (overwrite):
    ✓ read-file (src/repl.lisp) 0.02s 14.9 KB
    ✗ shell (bad-cmd) 1.20s Error
  ```

  **Implementation steps (TDD)**:

  RED: Write tests for:
  - `format-tool-start-line` — produces `"🔧 tool-name ..."` with args
  - `format-tool-result-line` — produces `"✓ tool-name (args) 0.02s 14.9 KB"` for success
  - `format-tool-result-line` — produces `"✗ tool-name (args) 1.20s Error"` for failure
  - Result line uses green ✓ / red ✗ with colors, plain ✓ / ✗ without
  - Spinner message is "Thinking..." (English)

  GREEN: Implement:
  - Create `format-tool-start-line` in `src/repl/display.lisp`
  - Create `format-tool-result-line` in `src/repl/display.lisp`
  - Create `make-tool-call-hook-v2` in `src/repl/display.lisp` (new hook closure that displays start line)
  - Create `make-tool-result-hook` in `src/repl/display.lisp` (hook closure that overwrites with result)
  - Wire new hooks in `start-repl` (repl.lisp):
    - Replace existing `make-tool-call-hook` with `make-tool-call-hook-v2`
    - Add `(cons :on-tool-result (make-tool-result-hook))` to hooks list
  - Replace Japanese strings:
    - `"考え中..."` → `"Thinking..."` (3 locations in repl.lisp: L1143, L1537, spinner defaults)
    - `"🔧 ~a を実行中..."` → `"🔧 ~a ..."` (2 locations in repl.lisp: L1122-1123)
  - Update `rich-repl-test.lisp` tests that assert Japanese strings (search for "考え中")

  REFACTOR: Ensure thread safety via `*spinner-output-lock*`.

  **Must NOT do**:
  - Add result content preview or syntax highlighting
  - Convert Japanese in docstrings, comments, or log messages
  - Move existing functions out of repl.lisp
  - Add new code to repl.lisp except hook wiring changes

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Touches both display module and repl.lisp, requires careful thread-safety coordination
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 5, 6)
  - **Blocks**: Task 7
  - **Blocked By**: Tasks 1, 2

  **References**:

  **Pattern References**:
  - `src/repl.lisp:1105-1144` — Current `make-tool-call-hook` (spinner stop/start, output lock, tool display)
  - `src/repl.lisp:1146-1202` — Current `format-tool-call-summary` (arg extraction per tool name)
  - `src/repl.lisp:1128` — Thread safety: `bt:with-lock-held (*spinner-output-lock*)` pattern
  - `src/repl/spinner.lisp:118` — Spinner clear pattern: `\r\e[2K` (carriage return + clear line)
  - `src/repl.lisp:1415-1418` — Hook wiring in `start-repl` (where to add `:on-tool-result`)

  **API/Type References**:
  - `src/llm/message.lisp` — `tool-call` struct: `tool-call-name`, `tool-call-arguments`
  - Task 2 output — `:on-tool-result` hook signature: `(tool-call result elapsed-seconds)`
  - Task 1 output — `format-bytes-human`, `format-duration` from display module

  **Test References**:
  - `tests/rich-repl-test.lisp:341` — Existing test checking for "考え中" (MUST be updated)
  - `tests/repl-display-test.lisp` — New test file from Task 1

  **Documentation References**:
  - ANSI escape codes: `\r` (carriage return), `\e[2K` (clear line), `\e[A` (cursor up)

  **Acceptance Criteria**:

  **TDD Tests:**
  - [ ] Tests added to `repl-display-tests` suite
  - [ ] `format-tool-start-line` produces correct format for read-file, shell, grep tools
  - [ ] `format-tool-result-line` success: includes ✓, tool name, args, time, size
  - [ ] `format-tool-result-line` failure: includes ✗, tool name, args, time, "Error"
  - [ ] Color mode: ✓ is green, ✗ is red
  - [ ] No-color mode: plain ✓/✗ without ANSI
  - [ ] No Japanese strings in user-visible output (grep for を実行中 and 考え中 in repl.lisp)
  - [ ] Existing `rich-repl-test.lisp` tests pass (updated assertions)
  - [ ] `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit` → result T

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Tool result formatting works correctly
    Tool: Bash (sbcl --eval)
    Steps:
      1. Load sibyl, call format-tool-result-line with mock data (name="read-file", args="src/foo.lisp", time=0.05, size=1500, success=t)
      2. Assert: output contains "✓", "read-file", "src/foo.lisp", "0.05s", "1.5 KB"
    Expected Result: Rich tool result line
    Evidence: Command output captured

  Scenario: No Japanese strings remain in user-visible output
    Tool: Bash (grep)
    Steps:
      1. grep -n "を実行中\|考え中" src/repl.lisp
      2. Assert: zero matches (exit code 1)
    Expected Result: All Japanese UI strings replaced with English
    Evidence: grep output captured

  Scenario: Existing rich-repl tests still pass
    Tool: Bash (sbcl --eval)
    Steps:
      1. Run rich-repl test suite specifically
      2. Assert: all tests pass
    Expected Result: Updated assertions match new English strings
    Evidence: Test output captured
  ```

  **Commit**: YES (groups with Task 4 only)
  - Message: `feat(repl): redesign tool display with timing and English strings`
  - Files: `src/repl/display.lisp`, `src/repl.lisp`, `tests/repl-display-test.lisp`, `tests/rich-repl-test.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit`

---

- [x] 5. Per-Turn Footer Redesign

  **What to do**:

  Replace the existing 1-line `format-elapsed-time` footer with a rich, conditionally-expanding multi-line status display. Base display is 2 lines; extra lines appear when cache or thinking tokens are present.

  **Target display format**:
  ```
  Base (always shown):
  ─────────────────────────────────────────────
   claude-opus-4-6 · 2.3s · $0.012    42% ctx
   Tokens  In 1,234  Out 567

  With cache (conditional):
  ─────────────────────────────────────────────
   claude-opus-4-6 · 2.3s · $0.012    42% ctx
   Tokens  In 1,234  Out 567  Thinking 890
   Cache   Read 450 (saved $0.004)  Write 89
  ```

  **Implementation steps (TDD)**:

  RED: Write tests for:
  - `format-turn-footer` — main function taking a plist of turn data
  - Base case: model, elapsed, cost, context%, tokens only → 2 lines + separator
  - With cache: adds cache line when cache-read-tokens > 0
  - With thinking: adds "Thinking N" to tokens line when thinking-tokens > 0
  - Without cost: omits cost for Ollama (local models)
  - With `*use-colors*` = nil: clean plain text
  - Cost calculated via `estimate-cost-usd` using turn token deltas

  GREEN: Implement:
  - Create `format-turn-footer` in `src/repl/display.lisp`
  - Function signature:
    ```lisp
    (defun format-turn-footer (&key model elapsed-seconds
                                    input-tokens output-tokens
                                    thinking-tokens
                                    cache-read-tokens cache-write-tokens
                                    context-percentage
                                    cost-usd)
      ...)
    ```
  - Wire into REPL loop: Replace `format-elapsed-time` call at repl.lisp:1585-1592 with `format-turn-footer` call
  - Calculate cost using `estimate-cost-usd` with turn delta tokens
  - Calculate context % using `input-tokens / context-window-for-model`
  - Accumulate cost in token-tracker via `tracker-add-cost`

  REFACTOR: Ensure separator line width is reasonable.

  **Must NOT do**:
  - Delete `format-elapsed-time` (keep for backward compat, but stop calling it)
  - Add progress bars (user chose percentage only)
  - Add background colors
  - Handle terminal width specially

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Core user-facing display change, requires careful formatting and cost calculation integration
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 4, 6)
  - **Blocks**: Task 7
  - **Blocked By**: Tasks 1, 3

  **References**:

  **Pattern References**:
  - `src/repl.lisp:1232-1253` — Current `format-elapsed-time` (what to replace)
  - `src/repl.lisp:1569-1592` — REPL loop where footer is displayed (token deltas, cache deltas calculated)
  - `src/repl.lisp:1544-1567` — Streaming setup context (where turn starts, what variables exist)

  **API/Type References**:
  - `src/llm/pricing.lisp:49-68` — `estimate-cost-usd` function signature and return format
  - Task 3 output — `context-window-for-model`, `tracker-add-cost` functions
  - Task 1 output — `format-cost`, `format-tokens-compact`, `format-duration`, `format-context-percentage`, `format-separator-line` helpers
  - `src/llm/client.lisp:17-18` — `client-model` accessor to get model name string

  **Test References**:
  - `tests/repl-display-test.lisp` — Test file from Task 1

  **Acceptance Criteria**:

  **TDD Tests:**
  - [ ] `format-turn-footer` base case: produces separator + 2 content lines
  - [ ] `format-turn-footer` with cache: produces 3 content lines including cache info
  - [ ] `format-turn-footer` with thinking: tokens line includes "Thinking N"
  - [ ] `format-turn-footer` with cost nil: omits cost from first line
  - [ ] `format-turn-footer` no-color: no ANSI escape codes anywhere
  - [ ] Context percentage: 0% for zero input tokens (no division by zero)
  - [ ] Cost calculation: matches `estimate-cost-usd` output for same inputs
  - [ ] `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit` → result T

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Footer formatting with all features
    Tool: Bash (sbcl --eval)
    Steps:
      1. Call format-turn-footer with: model="claude-opus-4-6", elapsed=2.3, input=1234, output=567, thinking=890, cache-read=450, cache-write=89, context-percentage=0.42, cost=0.012
      2. Assert: output contains "claude-opus-4-6", "2.30s", "$0.012", "42%", "1,234", "567", "890", "450", "89"
      3. Assert: output has separator line (contains "─")
    Expected Result: Rich multi-line footer with all data
    Evidence: Command output captured

  Scenario: Footer without cache or thinking (minimal)
    Tool: Bash (sbcl --eval)
    Steps:
      1. Call format-turn-footer with: model="claude-sonnet-4-6", elapsed=0.5, input=100, output=50, thinking=0, cache-read=0, cache-write=0, context-percentage=0.05, cost=0.001
      2. Assert: output does NOT contain "Cache" or "Thinking"
      3. Assert: output has exactly 2 content lines + separator
    Expected Result: Compact 2-line footer for simple turns
    Evidence: Command output captured

  Scenario: Footer for Ollama (no cost)
    Tool: Bash (sbcl --eval)
    Steps:
      1. Call format-turn-footer with cost-usd=nil, model="llama3"
      2. Assert: output does NOT contain "$"
    Expected Result: Cost omitted for local models
    Evidence: Command output captured
  ```

  **Commit**: YES
  - Message: `feat(repl): redesign per-turn footer with cost, cache, and context display`
  - Files: `src/repl/display.lisp`, `src/repl.lisp`, `tests/repl-display-test.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit`

---

- [x] 6. Session Summary Enhancement

  **What to do**:

  Overhaul the `/tokens` command (`format-token-usage` in repl.lisp) with a rich, color-coded session summary including cumulative cost, cache savings, and thinking token totals.

  **Target display format**:
  ```
  ═══════════════════════════════════════════════
   Session Summary (claude-opus-4-6)
  ═══════════════════════════════════════════════
   Requests    12
   Total Cost  $0.145

   Tokens
     Input       12,345
     Output       5,678
     Thinking     2,100
     Cache Read   3,456  (saved $0.023)
     Cache Write    890

   Cache Performance
     Server Hit Rate  28.0%
     Response Cache   8 / 12 (66.7%)
     Entries          15 / 100
  ═══════════════════════════════════════════════
  ```

  **Implementation steps (TDD)**:

  RED: Write tests for:
  - `format-session-summary` — takes agent's token-tracker + model-name, produces formatted string
  - Shows cumulative cost from `token-tracker-cost-usd`
  - Shows thinking tokens from `token-tracker-thinking-tokens`
  - Shows cache savings calculated via `decompose-savings`
  - Shows server cache hit rate from `tracker-cache-hit-rate`
  - Shows response cache stats from client
  - Plain text fallback with `*use-colors*` = nil

  GREEN: Implement:
  - Create `format-session-summary` in `src/repl/display.lisp`
  - Replace `format-token-usage` call in repl.lisp `/tokens` handler with new function
  - Use existing `decompose-savings` for cache savings display
  - Use `token-tracker-cost-usd` for total cost

  REFACTOR: Clean formatting alignment.

  **Must NOT do**:
  - Add persistence, history, or export functionality
  - Delete existing `format-token-usage` (keep for backward compat)
  - Add charts or graphs

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: Formatting function + simple wiring, no complex logic
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Tasks 4, 5)
  - **Blocks**: Task 7
  - **Blocked By**: Tasks 1, 3

  **References**:

  **Pattern References**:
  - `src/repl.lisp:550-578` — Current `format-token-usage` (what to replace/enhance)
  - `src/repl.lisp:458-465` — `/tokens` command handler (call site for format function)
  - `src/repl.lisp:186-198` — `format-colored-text` for color output

  **API/Type References**:
  - `src/llm/token-tracker.lisp` — All tracker fields including new cost-usd and thinking-tokens
  - `src/llm/pricing.lisp:89-145` — `decompose-savings` for cache savings breakdown
  - `src/cache/telemetry.lisp` — Response cache stats access pattern
  - Task 1 output — `format-cost`, `format-tokens-compact` helpers

  **Test References**:
  - `tests/repl-display-test.lisp` — Test file from Task 1

  **Acceptance Criteria**:

  **TDD Tests:**
  - [ ] `format-session-summary` includes total cost from tracker
  - [ ] `format-session-summary` includes thinking tokens when > 0
  - [ ] `format-session-summary` includes cache savings (from decompose-savings)
  - [ ] `format-session-summary` includes server + response cache hit rates
  - [ ] No-color mode: clean aligned text without ANSI
  - [ ] Zero requests: handles empty tracker gracefully (no division by zero)
  - [ ] `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit` → result T

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Session summary with full data
    Tool: Bash (sbcl --eval)
    Steps:
      1. Create a token-tracker with: input=12345, output=5678, thinking=2100, cache-read=3456, cache-write=890, cost=0.145, requests=12
      2. Call format-session-summary with tracker and model "claude-opus-4-6"
      3. Assert: output contains "$0.145", "12,345", "5,678", "2,100", "3,456", "890"
      4. Assert: output contains "Session Summary"
    Expected Result: Rich formatted session summary
    Evidence: Command output captured

  Scenario: Session summary with empty tracker
    Tool: Bash (sbcl --eval)
    Steps:
      1. Create fresh token-tracker (all zeros)
      2. Call format-session-summary
      3. Assert: no errors, output contains "$0.000" and "0"
    Expected Result: Graceful display with zero data
    Evidence: Command output captured
  ```

  **Commit**: YES
  - Message: `feat(repl): enhance /tokens session summary with cost and cache savings`
  - Files: `src/repl/display.lisp`, `src/repl.lisp`, `tests/repl-display-test.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit`

---

- [x] 7. Integration Verification + Context Window

  **What to do**:

  Final integration task: wire all components together, verify the complete display pipeline works end-to-end, ensure context window percentage flows correctly, and run the full test suite.

  **Implementation steps**:

  1. Verify all Wave 1-2 tasks are integrated:
     - Display module loads and exports are accessible
     - Token tracker has cost-usd and thinking-tokens fields
     - `:on-tool-result` hook fires with timing
     - Footer uses new `format-turn-footer`
     - Tool display uses new hooks
     - `/tokens` uses new `format-session-summary`

  2. Wire context window percentage:
     - In REPL loop (repl.lisp:1585-1592 area), calculate:
       ```lisp
       (let* ((ctx-window (context-window-for-model model-name))
              (ctx-pct (if (and ctx-window (> ctx-window 0) (> in-delta 0))
                           (/ (float in-delta) ctx-window)
                           0.0)))
         ...)
       ```
     - Pass `context-percentage` to `format-turn-footer`

  3. Wire cost accumulation:
     - After calculating per-turn cost, call `(tracker-add-cost tracker turn-cost-total)`

  4. Verify thinking token flow:
     - Check if Anthropic SSE events contain separate thinking token count
     - If YES: extract and include in usage plist
     - If NO: estimate from `(message-thinking response)` text length (ceiling len/4)
     - Pass thinking-tokens to footer

  5. Verify provider-specific behavior:
     - Anthropic: full display (cost + cache + thinking + context)
     - OpenAI: cost + context (no thinking, cache only if `cached_tokens` present)
     - Ollama: tokens + context only (no cost, no cache, thinking from `<think>` blocks if present)

  6. Run full test suite and verify zero regressions.

  **Must NOT do**:
  - Add new features beyond wiring
  - Refactor existing code
  - Change any behavior that was already verified in tasks 1-6

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Cross-cutting integration touching all modified files, requires careful verification
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 3 (sequential, final)
  - **Blocks**: None (final task)
  - **Blocked By**: Tasks 4, 5, 6

  **References**:

  **Pattern References**:
  - `src/repl.lisp:1569-1592` — REPL loop footer display area (integration point)
  - `src/repl.lisp:1414-1418` — Hook wiring in start-repl
  - `src/agent/core.lisp:220-262` — agent-step (verify hooks fire)
  - `src/llm/providers.lisp:350-380` — Streaming usage/thinking data flow

  **API/Type References**:
  - All Task 1-6 outputs — display functions, hooks, tracker fields
  - `src/llm/message.lisp:31` — `message-thinking` slot (for thinking token estimation)
  - `src/llm/client.lisp` — `client-model` for model name extraction

  **Acceptance Criteria**:

  **TDD Tests:**
  - [ ] All existing tests pass: `(asdf:test-system :sibyl)` → T
  - [ ] All new display tests pass
  - [ ] Zero compilation warnings from new code

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Full test suite passes
    Tool: Bash (sbcl --eval)
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit
      2. Assert: exit code 0, result T
      3. Count total checks — should be >= baseline (3,214 from previous run) + new tests
    Expected Result: All tests pass with zero regressions
    Evidence: Full test output captured

  Scenario: Display module fully integrated
    Tool: Bash (sbcl --eval)
    Steps:
      1. Load sibyl
      2. Verify all display exports: format-turn-footer, format-session-summary, format-tool-start-line, format-tool-result-line, format-cost, format-tokens-compact, format-bytes-human, format-duration, format-context-percentage
      3. Assert: all are fboundp
    Expected Result: All display functions accessible
    Evidence: Command output captured

  Scenario: Context window calculation works
    Tool: Bash (sbcl --eval)
    Steps:
      1. Load sibyl
      2. Calculate: input-tokens=85000, model="claude-opus-4-6"
      3. context-window = context-window-for-model("claude-opus-4-6") = 200000
      4. percentage = 85000/200000 = 0.425
      5. format-context-percentage(0.425) should produce "42%" in green
    Expected Result: Correct percentage with appropriate color
    Evidence: Command output captured

  Scenario: Cost accumulation across turns
    Tool: Bash (sbcl --eval)
    Steps:
      1. Create token-tracker
      2. Call tracker-add-cost with 0.005, then 0.003, then 0.007
      3. Assert: token-tracker-cost-usd = 0.015
    Expected Result: Cost accumulates correctly
    Evidence: Command output captured

  Scenario: Provider-specific footer behavior
    Tool: Bash (sbcl --eval)
    Steps:
      1. Call format-turn-footer with model="claude-opus-4-6" and cache/thinking data
      2. Assert: output includes cost, cache, thinking
      3. Call format-turn-footer with model="llama3" and cost-usd=nil
      4. Assert: output omits cost line, omits cache line
    Expected Result: Correct conditional display per provider
    Evidence: Command output captured
  ```

  **Commit**: YES
  - Message: `feat(repl): integrate display overhaul with context window and cost tracking`
  - Files: `src/repl.lisp`, `src/repl/display.lisp` (if any final tweaks), `tests/repl-display-test.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit`

---

## Commit Strategy

| After Task | Message | Key Files | Verification |
|------------|---------|-----------|--------------|
| 1 | `feat(repl): add display infrastructure module with formatting helpers` | display.lisp, packages.lisp, sibyl.asd | `(asdf:test-system :sibyl)` → T |
| 2 | `feat(agent): add on-tool-result hook with execution timing` | core.lisp, tests | `(asdf:test-system :sibyl)` → T |
| 3 | `feat(llm): enhance token-tracker with cost accumulator and thinking tokens` | token-tracker.lisp, model-selector.lisp | `(asdf:test-system :sibyl)` → T |
| 4 | `feat(repl): redesign tool display with timing and English strings` | display.lisp, repl.lisp | `(asdf:test-system :sibyl)` → T |
| 5 | `feat(repl): redesign per-turn footer with cost, cache, and context display` | display.lisp, repl.lisp | `(asdf:test-system :sibyl)` → T |
| 6 | `feat(repl): enhance /tokens session summary with cost and cache savings` | display.lisp, repl.lisp | `(asdf:test-system :sibyl)` → T |
| 7 | `feat(repl): integrate display overhaul with context window and cost tracking` | repl.lisp, display.lisp | `(asdf:test-system :sibyl)` → T |

---

## Success Criteria

### Verification Commands
```bash
# All tests pass
sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --quit
# Expected: result T

# No Japanese in user-visible output
grep -rn "を実行中\|考え中" src/repl.lisp
# Expected: zero matches

# Display module loads
sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(format t "~a" (fboundp (quote sibyl.repl.display:format-turn-footer)))' --quit
# Expected: T

# New exports exist
sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(format t "~a" (find-symbol "CONTEXT-WINDOW-FOR-MODEL" :sibyl.llm))' --quit
# Expected: SIBYL.LLM:CONTEXT-WINDOW-FOR-MODEL
```

### Final Checklist
- [x] All "Must Have" present
- [x] All "Must NOT Have" absent
- [x] All tests pass (existing + new)
- [x] No Japanese in user-visible strings
- [x] Plain-text fallback works (*use-colors* nil)
- [x] Cost displayed per-turn and in session summary
- [x] Cache savings visible when cache tokens present
- [x] Tool timing and result feedback visible
- [x] Context window percentage visible
- [x] `/tokens` shows rich summary with cost
