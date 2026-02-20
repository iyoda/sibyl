# Session Persistence for Sibyl REPL

## TL;DR

> **Quick Summary**: Implement session persistence — auto-save conversation history every 5 minutes and on exit, resume from any previous session by specifying its timestamp-based session ID on startup.
> 
> **Deliverables**:
> - `src/repl/session.lisp` — serialization, file I/O, auto-save timer
> - 3 new REPL commands: `/sessions`, `/save`, `/load <id>`
> - `:session-id` keyword on `start-repl` for resume
> - `tests/session-test.lisp` — FiveAM test suite
> - Updated `sibyl.asd`, `src/packages.lisp`, `tests/suite.lisp`
> 
> **Estimated Effort**: Medium
> **Parallel Execution**: YES — 3 waves
 > **Critical Path**: Task 1 (serialization) → Task 3 (file I/O) → Task 5 (REPL integration) → Task 6 (auto-save timer) → Task 7 (tests)

---

## Context

### Original Request
"終了時に会話履歴をファイルへ保存し、次回起動時に引数でセッションIDを指定することで前回の続きから作業を再開できる機能を実装する"

### Interview Summary
**Key Discussions**:
- **Session ID format**: Timestamp-based auto-generation (e.g., `"session-20250220-143022"`)
- **Storage format**: S-expression (following `plan->sexp` pattern in `src/plan/core.lisp`)
- **Restore scope**: Conversation messages + memory summary (summary added after Metis review)
- **Save timing**: Auto-save every 5 minutes (configurable) + save on exit
- **Default behavior**: Always auto-generate a new session; display session-id on exit for resumption
- **New commands**: `/sessions` (list), `/save` (manual), `/load <id>` (switch with auto-save)
- **/reset behavior**: Auto-save current session, then clear within same session-id
- **Test strategy**: Tests-after with FiveAM

**Research Findings**:
- `message` is a defstruct → `#S()` notation is readable, but nested `tool-call` is CLOS → needs explicit serialization
- `conversation-to-list` returns a thread-safe copy under lock → safe for auto-save
- `exit-repl` is a `labels` closure calling `return-from repl-loop` → session save must go here before process exit
- `spinner.lisp` has stop-flag + sleep-loop pattern → reuse for auto-save timer
- `plan->sexp` / `sexp->plan` in `src/plan/core.lisp` → proven S-expression I/O pattern with index file
- `bordeaux-threads` already in dependencies → no new deps needed
- `*repl-commands*` + `*command-handlers*` alist pattern → push new entries

### Metis Review
**Identified Gaps** (addressed):
- **tool-call CLOS serialization**: Implement explicit `tool-call->sexp` / `sexp->tool-call` converters (plist format). Note: `plan->sexp` is NOT a CLOS converter — plans are already plists. Novel code needed.
- **Memory summary persistence**: Added to restore scope — save `memory-summary` alongside messages
- **Timer thread lifecycle**: Use stop-flag pattern from `spinner.lisp`, stop in `exit-repl` BEFORE final save
- **Concurrent save protection**: Use `*session-save-lock*` (bt:lock) for all session file writes
- **/reset with sessions**: Auto-save before clearing, stay in same session-id
- **System messages**: Filter `:system` role on save — regenerated from startup config
- **Auto-save interval**: Make configurable via `:auto-save-interval` keyword (default 300s, tests use 1s)
- **Format version**: `(:session-version 1)` tag for forward-compatible migration (added in Round 2 Metis review)
- **Session ID collision**: Random suffix (`(random 1000000)`) prevents same-second ID clash across REPL instances
- **Content polymorphism**: `message.content` is `(or string list null)` — serializer must handle all three
- **Exit race**: Session save MUST go inside `exit-repl` labels function, BEFORE `return-from repl-loop`. `sb-ext:exit` at line 1517 kills process immediately after the block.
- **command-count**: Persist `*command-count*` for prompt counter continuity on restore
- **Auto-save during compaction**: Timer fires while `memory-compact` clears/rebuilds conversation. Save must use `conversation-to-list` (already locked)

---

## Work Objectives

### Core Objective
Enable users to save and resume REPL conversations via timestamped session files stored as S-expressions in `~/.sibyl/sessions/`.

### Concrete Deliverables
- `src/repl/session.lisp` — all session persistence logic
- 3 REPL commands: `/sessions`, `/save`, `/load <id>`
- `:session-id` and `:auto-save-interval` keywords on `start-repl`
- `tests/session-test.lisp` — round-trip, file I/O, and command handler tests
- Updated: `sibyl.asd`, `src/packages.lisp`, `tests/suite.lisp`

### Definition of Done
- [x] `(sibyl.repl::save-session ...)` writes S-expression file to `~/.sibyl/sessions/<id>/session.lisp`
- [x] `(sibyl.repl::load-session ...)` reads and reconstructs conversation messages + summary
- [x] Tool-call round-trip: CLOS objects survive serialize → deserialize cycle
- [x] `/sessions` lists saved sessions with ID, date, message count
- [x] `/save` writes current session to disk
- [x] `/load <id>` auto-saves current, then loads specified session
- [x] `/reset` auto-saves before clearing
- [x] Auto-save fires every 5 minutes (configurable)
- [x] Session-id displayed on exit with resume instructions
- [x] `start-repl :session-id "..."` loads existing session on startup
- [x] All existing tests still pass (1505 checks)
- [x] New session tests pass

### Must Have
- Timestamp-based session ID auto-generation with random suffix (e.g., `session-20250220-143022-847291`)
- `(:session-version 1)` format tag in all serialized session files
- S-expression serialization with safe read (`*read-eval* nil`)
- Session index file for fast listing (`~/.sibyl/sessions/index.lisp`)
- Thread-safe auto-save timer with configurable interval
- Graceful handling of missing/corrupt session files (warn, don't crash)
- Atomic file writes (write to temp, rename)
- `*command-count*` persisted in session for prompt counter continuity
- Content polymorphism handling: `(or string list null)` in message serialization
- `bt:with-lock-held` on conversation lock during save (prevent compaction race)

### Must NOT Have (Guardrails)
- ❌ Session tagging, naming, or user-provided descriptions
- ❌ Session search, filtering, or grep functionality
- ❌ Session export to JSON/Markdown/other formats
- ❌ Token tracker or cost record serialization
- ❌ Session merge, append, or diff capabilities
- ❌ Session encryption or compression
- ❌ Automatic session rotation/cleanup/garbage collection
- ❌ Storing model/client/tools metadata
- ❌ Session locking for multi-process safety
- ❌ Interactive session browser or TUI selector
- ❌ More than 3 commands (`/sessions`, `/save`, `/load`) — no `/delete`, `/rename`, `/export`
- ❌ Modification of `message` struct or `tool-call` class definitions
- ❌ New external dependencies

---

## Verification Strategy

> **ZERO HUMAN INTERVENTION** — ALL verification is agent-executed. No exceptions.

### Test Decision
- **Infrastructure exists**: YES (FiveAM)
- **Automated tests**: YES (tests-after)
- **Framework**: FiveAM
- **If TDD**: N/A — tests-after strategy

### QA Policy
Every task MUST include agent-executed QA scenarios.
Evidence saved to `.sisyphus/evidence/task-{N}-{scenario-slug}.{ext}`.

- **Serialization**: Use Bash (sbcl --eval) — serialize, deserialize, compare
- **File I/O**: Use Bash (sbcl --eval) — save, load, list, verify file contents
- **REPL Commands**: Use Bash (sbcl --eval) — programmatic invocation of command handlers
- **Auto-save Timer**: Use Bash (sbcl --eval) — start timer with 1s interval, verify file created

---

## Execution Strategy

### Parallel Execution Waves

```
Wave 1 (Start Immediately — foundation):
├── Task 1: Serialization layer (tool-call->sexp, message->sexp, session->sexp + reverses) [deep]
├── Task 2: Package exports + ASDF updates [quick]

Wave 2 (After Wave 1 — core I/O + commands):
├── Task 3: File I/O (save-session, load-session, list-sessions, index) [unspecified-high]
├── Task 4: REPL commands (/sessions, /save, /load) [unspecified-high]
├── Task 5: REPL integration (start-repl :session-id, exit-repl hook, /reset hook) [deep]

Wave 3 (After Wave 2 — timer + tests):
├── Task 6: Auto-save timer (bordeaux-threads, stop-flag, configurable interval) [unspecified-high]
├── Task 7: Tests (session-test.lisp, FiveAM suite, suite classification) [unspecified-high]

Wave FINAL (After ALL tasks — verification):
├── Task F1: Plan compliance audit [oracle]
├── Task F2: Code quality review [unspecified-high]
├── Task F3: Real QA — programmatic verification [unspecified-high]
├── Task F4: Scope fidelity check [deep]

Critical Path: Task 1 → Task 3 → Task 5 → Task 6 → Task 7 → F1-F4
Parallel Speedup: ~50% faster than sequential
Max Concurrent: 3 (Wave 2)
```

### Dependency Matrix

| Task | Depends On | Blocks | Wave |
|------|-----------|--------|------|
| 1 | — | 3, 4, 5, 6, 7 | 1 |
| 2 | — | 3, 4, 5, 6, 7 | 1 |
| 3 | 1, 2 | 4, 5, 6, 7 | 2 |
| 4 | 1, 2, 3 | 7 | 2 |
| 5 | 1, 2, 3 | 6, 7 | 2 |
| 6 | 3, 5 | 7 | 3 |
| 7 | 1, 2, 3, 4, 5, 6 | F1-F4 | 3 |
| F1-F4 | all | — | FINAL |

### Agent Dispatch Summary

- **Wave 1**: **2** — T1 → `deep`, T2 → `quick`
- **Wave 2**: **3** — T3 → `unspecified-high`, T4 → `unspecified-high`, T5 → `deep`
- **Wave 3**: **2** — T6 → `unspecified-high`, T7 → `unspecified-high`
- **FINAL**: **4** — F1 → `oracle`, F2 → `unspecified-high`, F3 → `unspecified-high`, F4 → `deep`

---

## TODOs

- [x] 1. Serialization Layer — tool-call, message, and session S-expression converters

  **What to do**:
  - Create `src/repl/session.lisp` in the `sibyl.repl` package
  - Implement `tool-call->sexp` and `sexp->tool-call`:
    - Convert tool-call CLOS object to plist: `(:id "..." :name "..." :arguments (("key" . "val") ...))`
    - `tool-call-arguments` is already an alist — serialize directly
    - Handle nil arguments gracefully
  - Implement `message->sexp` and `sexp->message`:
    - Convert message struct to plist: `(:role :user :content "..." :timestamp "..." :tool-calls (...) :tool-call-id "...")`
    - For `:assistant` messages with tool-calls, nest `tool-call->sexp` results
    - For `:tool` messages, include `:tool-call-id`
    - Exclude `:thinking` and `:thinking-signature` fields (API-specific, not needed for restore)
    - Handle content that is a list of content-block alists (not just strings)
  - Implement `session->sexp` and `sexp->session`:
    - Session format: `(:repl-session (:session-version 1) :id "..." :created-at "..." :last-modified "..." :message-count N :command-count N :summary "..." :messages (...))`
    - Filter out `:system` role messages (regenerated from startup config)
    - Include `memory-summary` as `:summary` field (string or nil)
  - Implement `generate-session-id`:
    - Format: `"session-YYYYMMDD-HHMMSS-NNNNNN"` using `get-decoded-time` + `(random 1000000)`
    - Random suffix prevents collision when two REPL instances start in the same second
    - Example: `"session-20250220-143022-847291"`
  - **Format version**: All session sexp MUST include `(:session-version 1)` as the first field after `:repl-session` tag, enabling future migration
  - **command-count**: Include `*command-count*` in session data for prompt counter continuity on restore

  **Must NOT do**:
  - Do NOT modify `message` struct or `tool-call` class definitions
  - Do NOT serialize thinking/thinking-signature
  - Do NOT serialize token-tracker, cost-records, hooks
  - Do NOT add new external dependencies

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: Serialization with CLOS objects requires careful handling of edge cases (nil values, nested types, content polymorphism)
  - **Skills**: []
    - No special skills needed — pure Lisp implementation
  - **Skills Evaluated but Omitted**:
    - `playwright`: Not relevant — no browser interaction
    - `git-master`: Not relevant — no git operations in this task

  **Parallelization**:
  - **Can Run In Parallel**: YES (with Task 2)
  - **Parallel Group**: Wave 1 (with Task 2)
  - **Blocks**: Tasks 3, 4, 5, 6, 7
  - **Blocked By**: None (can start immediately)

  **References**:

  **Pattern References** (existing code to follow):
  - `src/plan/core.lisp:127-138` — `plan->sexp` (line 127) / `sexp->plan` (line 132) pattern: tagged plist wrapper, `prin1` with `*print-case* :downcase`, `uiop:safe-read-file-form` with `*read-eval* nil`. Follow this exact pattern for session serialization.
  - `src/plan/core.lisp:127-138` — `plan->sexp` / `sexp->plan` shows how to serialize a plan to plist and restore via `getf`. Use the same `getf` pattern for deserialization of session plists.
  - `src/repl/spinner.lisp:50-80` — Stop-flag + sleep loop pattern for background threads. Reference for Task 6 but good to understand the concurrency pattern now.

  **API/Type References** (contracts to implement against):
  - `src/llm/message.lisp:1-30` — `message` struct definition: slots are `role`, `content`, `tool-calls`, `tool-call-id`, `timestamp`, `thinking`, `thinking-signature`. Constructor is `make-message`.
  - `src/llm/message.lisp:32-60` — `tool-call` class definition: slots are `id`, `name`, `arguments`. Constructor is `make-tool-call`. Accessors: `tool-call-id`, `tool-call-name`, `tool-call-arguments`.
  - `src/llm/message.lisp:62-90` — `conversation` struct: slots are `messages` (list) and `lock` (bt:lock). `conversation-to-list` returns a thread-safe copy. `conversation-messages` is the raw accessor.
  - `src/agent/memory.lisp:1-40` — `memory` class: `memory-conversation` accessor, `memory-summary` accessor (string or nil).
  - `src/llm/message.lisp` — Helper constructors: `user-message`, `assistant-message`, `tool-result-message`. Use these in tests.

  **External References**:
  - Common Lisp HyperSpec: `prin1`, `*print-case*`, `*print-pretty*`, `*read-eval*`, `with-standard-io-syntax`

  **WHY Each Reference Matters**:
  - `plan->sexp` pattern is the proven serialization pattern in this codebase — follow it exactly for consistency
  - `tool-call` class definition tells you the CLOS slot accessors to use (not struct accessors)
  - `message` struct definition tells you ALL fields including optional ones to handle
  - `memory-summary` is the compacted text that must be persisted alongside messages

  **Acceptance Criteria**:

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: Tool-call round-trip with arguments
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl system loaded
    Steps:
      1. Create tool-call: (make-tool-call :id "tc_42" :name "read-file" :arguments '(("path" . "/tmp/foo.lisp")))
      2. Convert to sexp: (sibyl.repl::tool-call->sexp tc)
      3. Convert back: (sibyl.repl::sexp->tool-call sexp)
      4. Assert (string= (tool-call-id restored) "tc_42")
      5. Assert (string= (tool-call-name restored) "read-file")
      6. Assert (string= (cdr (assoc "path" (tool-call-arguments restored) :test #'string=)) "/tmp/foo.lisp")
    Expected Result: All assertions pass, sbcl exits with code 0
    Failure Indicators: Any assertion error, type error, or unbound function
    Evidence: .sisyphus/evidence/task-1-tool-call-roundtrip.txt

  Scenario: Tool-call round-trip with nil arguments
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl system loaded
    Steps:
      1. Create tool-call: (make-tool-call :id "tc_99" :name "no-args" :arguments nil)
      2. Round-trip through tool-call->sexp → sexp->tool-call
      3. Assert arguments is nil
    Expected Result: No error, nil arguments preserved
    Failure Indicators: Error on nil arguments
    Evidence: .sisyphus/evidence/task-1-tool-call-nil-args.txt

  Scenario: Message round-trip with tool-calls (assistant message)
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl system loaded
    Steps:
      1. Create assistant message with tool-calls: (assistant-message "thinking..." :tool-calls (list (make-tool-call :id "tc_1" :name "shell" :arguments '(("command" . "ls")))))
      2. Convert to sexp: (sibyl.repl::message->sexp msg)
      3. Convert back: (sibyl.repl::sexp->message sexp)
      4. Assert role is :assistant
      5. Assert content is "thinking..."
      6. Assert (= 1 (length (message-tool-calls restored)))
      7. Assert tool-call id is "tc_1"
    Expected Result: Full fidelity round-trip
    Failure Indicators: Missing tool-calls, wrong role, nil content
    Evidence: .sisyphus/evidence/task-1-message-roundtrip.txt

  Scenario: Session round-trip with mixed message types and summary
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl system loaded
    Steps:
      1. Create messages list: user, assistant, assistant-with-tools, tool-result
      2. Create session sexp with summary: (sibyl.repl::session->sexp "test-id" messages "old context summary")
      3. Parse back: (sibyl.repl::sexp->session sexp)
      4. Assert message count matches
      5. Assert summary is "old context summary"
      6. Assert :system role messages are NOT present
      7. Assert all message types correctly round-tripped
    Expected Result: All message types survive round-trip, summary preserved
    Failure Indicators: Missing messages, lost summary, system messages included
    Evidence: .sisyphus/evidence/task-1-session-roundtrip.txt

  Scenario: System messages filtered on serialization
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl system loaded
    Steps:
      1. Create messages including (make-message :role :system :content "You are Sibyl...")
      2. Serialize to session sexp
      3. Count messages in sexp — should be N-1 (system excluded)
      4. Deserialize back — no system message present
    Expected Result: System messages excluded from serialized output
    Failure Indicators: System message appears in output
    Evidence: .sisyphus/evidence/task-1-filter-system.txt
  ```

  **Evidence to Capture:**
  - [ ] task-1-tool-call-roundtrip.txt
  - [ ] task-1-tool-call-nil-args.txt
  - [ ] task-1-message-roundtrip.txt
  - [ ] task-1-session-roundtrip.txt
  - [ ] task-1-filter-system.txt

  **Commit**: YES (groups with Task 2)
  - Message: `feat(repl): add session serialization layer`
  - Files: `src/repl/session.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sb-ext:exit)'`

- [x] 2. Package Exports + ASDF Updates

  **What to do**:
  - Add `(:file "session")` to the `:repl-module` in `sibyl.asd`, after `display`:
    ```lisp
    (:module "repl-module"
     :pathname "repl"
     :components ((:file "spinner")
                  (:file "display")
                  (:file "session")))
    ```
  - Add new public symbol exports to `sibyl.repl` package in `src/packages.lisp`:
    - `save-session`, `load-session`, `list-sessions`
    - `generate-session-id`
    - Any other symbols needed by `src/repl.lisp` (the main REPL file uses `sibyl.repl` package, so internal symbols are accessible — only export what users need)
  - Verify system loads cleanly after changes

  **Must NOT do**:
  - Do NOT add new package definitions — use existing `sibyl.repl`
  - Do NOT add new dependencies to `sibyl.asd`
  - Do NOT modify `sibyl` facade package exports yet (wait until API is stable)

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: Simple file edits — add entries to existing lists
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `git-master`: Not relevant — no git operations

  **Parallelization**:
  - **Can Run In Parallel**: YES (with Task 1)
  - **Parallel Group**: Wave 1 (with Task 1)
  - **Blocks**: Tasks 3, 4, 5, 6, 7
  - **Blocked By**: None (can start immediately)

  **References**:

  **Pattern References**:
  - `sibyl.asd:1-50` — ASDF system definition. The `:repl-module` is at the end of the components list. Follow existing component ordering.
  - `src/packages.lisp:421-427` — `sibyl.repl` package definition with current exports (`start-repl`, `repl-command-p`, `handle-repl-command`, `readline-available-p`). Add new exports here.
  - `src/packages.lisp:489-530` — `sibyl` facade package. Shows `:import-from` pattern. May add session symbols later.

  **WHY Each Reference Matters**:
  - `sibyl.asd` must be updated for the new file to be compiled/loaded
  - `packages.lisp` exports determine what's visible to external callers
  - Understanding the facade pattern ensures we don't break the public API

  **Acceptance Criteria**:

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: System loads cleanly with new session module
    Tool: Bash (sbcl --eval)
    Preconditions: Task 1 completed (session.lisp exists)
    Steps:
      1. Run: sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(format t "LOAD-OK~%")' --eval '(sb-ext:exit :code 0)'
      2. Assert output contains "LOAD-OK"
      3. Assert exit code is 0
    Expected Result: Clean load with no warnings or errors
    Failure Indicators: Compilation error, missing file, package conflict
    Evidence: .sisyphus/evidence/task-2-system-load.txt

  Scenario: Exported symbols accessible
    Tool: Bash (sbcl --eval)
    Preconditions: System loaded
    Steps:
      1. Run: (find-symbol "SAVE-SESSION" :sibyl.repl) and check second value is :external
      2. Run: (find-symbol "LOAD-SESSION" :sibyl.repl) and check :external
      3. Run: (find-symbol "LIST-SESSIONS" :sibyl.repl) and check :external
      4. Run: (find-symbol "GENERATE-SESSION-ID" :sibyl.repl) and check :external
    Expected Result: All symbols found as external
    Failure Indicators: Symbol not found or :internal
    Evidence: .sisyphus/evidence/task-2-exports.txt
  ```

  **Evidence to Capture:**
  - [ ] task-2-system-load.txt
  - [ ] task-2-exports.txt

  **Commit**: YES (groups with Task 1)
  - Message: `feat(repl): add session serialization layer and package exports`
  - Files: `sibyl.asd`, `src/packages.lisp`, `src/repl/session.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sb-ext:exit)'`

- [x] 3. File I/O — save-session, load-session, list-sessions with index

  **What to do**:
  - Implement in `src/repl/session.lisp` (same file as Task 1):
  - Define `*default-session-directory*`:
    ```lisp
    (defparameter *default-session-directory*
      (merge-pathnames ".sibyl/sessions/" (user-homedir-pathname)))
    ```
  - Implement `%session-directory` and `%session-file-path`:
    - `%session-file-path` returns `~/.sibyl/sessions/<session-id>/session.lisp`
    - Use `ensure-directories-exist` before writing
  - Implement `save-session (session-id messages summary &key directory)`:
    - Convert to sexp using `session->sexp` from Task 1
    - Write atomically: write to `session.tmp`, then rename to `session.lisp`
    - Use `with-open-file` + `prin1` with `*print-case* :downcase` + `*print-pretty* nil`
    - Update index file after save
    - Protect with a `*session-save-lock*` (bt:lock) for thread safety
  - Implement `load-session (session-id &key directory)`:
    - Read with `uiop:safe-read-file-form` (`*read-eval* nil`)
    - Return multiple values: `(values messages summary)` using `sexp->session` from Task 1
    - Handle missing file gracefully (return nil, print warning)
    - Handle corrupt file gracefully (return nil, print error with condition message)
  - Implement `list-sessions (&key directory)`:
    - Read index file `~/.sibyl/sessions/index.lisp`
    - Return list of plists: `((:id "..." :created-at "..." :last-modified "..." :message-count N) ...)`
    - If index file missing, scan directory and rebuild
  - Implement index file management:
    - `%update-session-index (session-id metadata &key directory)` — update one entry
    - `%load-session-index (&key directory)` — read index
    - `%rebuild-session-index (&key directory)` — scan all session dirs and rebuild
    - Index format: `(:session-index (:entry :id "..." :created-at "..." ...) ...)`

  **Must NOT do**:
  - Do NOT implement session deletion, rotation, or cleanup
  - Do NOT add file locking for multi-process safety
  - Do NOT compress or encrypt session files

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: File I/O with atomic writes, index management, error handling — non-trivial but not deeply complex
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `playwright`: Not relevant

  **Parallelization**:
  - **Can Run In Parallel**: YES (with Tasks 4 and 5, but Task 4 needs Task 3's save/load functions)
  - **Parallel Group**: Wave 2 (start immediately after Wave 1)
  - **Blocks**: Tasks 4, 5, 6, 7
  - **Blocked By**: Tasks 1, 2

  **References**:

  **Pattern References**:
  - `src/plan/core.lisp:198-225` — `save-plan` (line 198) uses `with-open-file` + `prin1` with `*print-case* :downcase`. `load-plan` (line 214) uses `uiop:safe-read-file-form`. Follow this exact pattern.
  - `src/plan/core.lisp:155-194` — Plan index management: `load-plan-index` (line 155), `%save-plan-index` (line 166), `%update-plan-index` (line 180). Follow this pattern for session index.
  - `src/plan/core.lisp:20` — `*default-plan-directory*` definition. Follow naming pattern (singular `plan`, not `plans`).

  **API/Type References**:
  - `src/repl/session.lisp` — `session->sexp`, `sexp->session`, `message->sexp`, `sexp->message` from Task 1. These are the serialization functions to call.

  **WHY Each Reference Matters**:
  - Plan I/O pattern is the battle-tested file persistence approach in this codebase — copy it
  - Index pattern prevents slow directory scanning on every `/sessions` call

  **Acceptance Criteria**:

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: Save and load round-trip with tool-call messages
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl system loaded
    Steps:
      1. Create test directory: (merge-pathnames "tmp-test-sessions/" (uiop:temporary-directory))
      2. Create messages with user, assistant (with tool-calls), and tool-result messages
      3. Call (sibyl.repl::save-session "test-001" messages "old summary" :directory dir)
      4. Verify file exists: (probe-file (merge-pathnames "test-001/session.lisp" dir))
      5. Call (multiple-value-bind (msgs sum) (sibyl.repl::load-session "test-001" :directory dir) ...)
      6. Assert (= (length msgs) (length messages))
      7. Assert (string= sum "old summary")
      8. Assert first message role is :user, content matches
      9. Assert tool-call in assistant message survived round-trip
      10. Clean up: (uiop:delete-directory-tree dir :validate t)
    Expected Result: All assertions pass, clean exit
    Failure Indicators: File not created, deserialization error, mismatched content
    Evidence: .sisyphus/evidence/task-3-save-load-roundtrip.txt

  Scenario: Load non-existent session returns nil gracefully
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl system loaded
    Steps:
      1. Call (sibyl.repl::load-session "nonexistent-session-999" :directory (uiop:temporary-directory))
      2. Assert returns nil (not an error)
    Expected Result: Returns nil without error
    Failure Indicators: Unhandled file-error condition
    Evidence: .sisyphus/evidence/task-3-missing-session.txt

  Scenario: List sessions returns correct metadata
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl system loaded
    Steps:
      1. Create test directory
      2. Save session "session-A" with 3 messages
      3. Save session "session-B" with 5 messages
      4. Call (sibyl.repl::list-sessions :directory dir)
      5. Assert (= 2 (length result))
      6. Assert each entry has :id, :created-at, :message-count
      7. Assert "session-A" has :message-count 3
      8. Assert "session-B" has :message-count 5
      9. Clean up
    Expected Result: Both sessions listed with correct metadata
    Failure Indicators: Wrong count, missing entries, missing metadata
    Evidence: .sisyphus/evidence/task-3-list-sessions.txt

  Scenario: Atomic write — corrupt partial write doesn't destroy existing
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl system loaded
    Steps:
      1. Save session "safe-session" normally (good data)
      2. Verify file exists and loads correctly
      3. Verify no .tmp file remains after successful save
    Expected Result: No temp files left behind, clean save
    Failure Indicators: .tmp file remains, original file corrupted
    Evidence: .sisyphus/evidence/task-3-atomic-write.txt
  ```

  **Evidence to Capture:**
  - [ ] task-3-save-load-roundtrip.txt
  - [ ] task-3-missing-session.txt
  - [ ] task-3-list-sessions.txt
  - [ ] task-3-atomic-write.txt

  **Commit**: YES (groups with Tasks 4, 5)
  - Message: `feat(repl): add session file I/O, REPL commands, and start-repl integration`
  - Files: `src/repl/session.lisp`, `src/repl.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sb-ext:exit)'`

- [x] 4. REPL Commands — /sessions, /save, /load

  **What to do**:
  - Add 3 new entries to `*repl-commands*` in `src/repl.lisp`:
    ```lisp
    ("/sessions" . :sessions)
    ("/save"     . :save-session)
    ("/load"     . :load-session)
    ```
  - Add 3 new entries to `*command-handlers*` in `src/repl.lisp`:
    ```lisp
    (cons :sessions     #'handle-sessions-command)
    (cons :save-session #'handle-save-session-command)
    (cons :load-session #'handle-load-session-command)
    ```
  - Implement `handle-sessions-command (agent input)`:
    - Call `list-sessions` from Task 3
    - Format output as a table: ID | Date | Messages
    - If no sessions, print "No saved sessions."
    - Return nil (continue REPL)
  - Implement `handle-save-session-command (agent input)`:
    - Get current session-id from a new dynamic variable `*current-session-id*` (set during `start-repl`)
    - Get messages from `(conversation-to-list (memory-conversation (agent-memory agent)))`
    - Get summary from `(memory-summary (agent-memory agent))`
    - Call `save-session` from Task 3
    - Print confirmation with session-id
    - Return nil (continue REPL)
  - Implement `handle-load-session-command (agent input)`:
    - Parse session-id from input: `(string-trim '(#\Space) (subseq input 5))` for `/load <id>`
    - If empty, print usage: "Usage: /load <session-id>"
    - Auto-save current session first (call save-session)
    - Call `load-session` from Task 3 to get messages and summary
    - If nil (not found), print error and return nil
    - Replace agent's conversation: call `conversation-clear`, then `conversation-push` for each loaded message
    - Set `memory-summary` on agent's memory
    - Update `*current-session-id*`
    - Print confirmation: "Loaded session: <id> (N messages)"
    - Return nil (continue REPL)
  - Update `/help` command output to include the 3 new commands

  **Must NOT do**:
  - Do NOT add `/delete`, `/rename`, or `/export` commands
  - Do NOT implement interactive session browser
  - Do NOT modify command dispatch mechanism (just add entries)

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Multiple handler functions with argument parsing, state manipulation, and user-facing output
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `playwright`: Not relevant

  **Parallelization**:
  - **Can Run In Parallel**: YES (with Task 5, after Task 3)
  - **Parallel Group**: Wave 2
  - **Blocks**: Task 7
  - **Blocked By**: Tasks 1, 2, 3

  **References**:

  **Pattern References**:
  - `src/repl.lisp:10-24` — `*repl-commands*` alist. Add new entries at end, before closing paren.
  - `src/repl.lisp:680-693` — `*command-handlers*` alist. Add new cons cells.
  - `src/repl.lisp:695-705` — `handle-repl-command` dispatch. Shows how handlers are called: `(funcall handler agent original-input)`. Handlers return `:quit` or `nil`.
  - `src/repl.lisp:338-343` — `handle-reset-command`. Simple handler pattern: `(declare (ignore input))`, do work, `(format t ...)`, return `nil`.
  - `src/repl.lisp:286-336` — `handle-help-command`. Shows how help text is formatted. Add new commands to this list.

  **API/Type References**:
  - `src/repl/session.lisp` — `save-session`, `load-session`, `list-sessions` from Task 3
  - `src/agent/memory.lisp:1-40` — `memory-conversation`, `memory-summary`, `(setf memory-summary)` accessors
  - `src/llm/message.lisp:79-90` — `conversation-push` (line 79, adds message to conversation under lock), `conversation-clear` (line 86, clears all messages), `conversation-to-list` (returns thread-safe copy)

  **WHY Each Reference Matters**:
  - Command registration pattern MUST be followed exactly — adding entries to wrong format breaks dispatch
  - Handler return value convention (`:quit` vs `nil`) determines whether REPL exits
  - Memory accessors needed for save (read) and load (write) operations
  - Use `conversation-push` (NOT `conversation-add-message`) to add messages on restore

  **Acceptance Criteria**:

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: /sessions command lists saved sessions
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl loaded, test sessions saved via save-session
    Steps:
      1. Create test directory, save 2 sessions with known IDs
      2. Let-bind *default-session-directory* to test dir
      3. Create mock agent with empty conversation
      4. Call (handle-sessions-command agent "/sessions")
      5. Capture stdout — assert contains both session IDs
      6. Assert return value is nil (not :quit)
    Expected Result: Both session IDs printed, returns nil
    Failure Indicators: Missing sessions, returns :quit, error
    Evidence: .sisyphus/evidence/task-4-sessions-cmd.txt

  Scenario: /save command saves current session
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl loaded, *current-session-id* set, agent has messages
    Steps:
      1. Create test dir, set *default-session-directory*
      2. Set *current-session-id* to "test-save-001"
      3. Create agent with 2 user messages in conversation
      4. Call (handle-save-session-command agent "/save")
      5. Verify file created: (probe-file ...)
      6. Load file and verify 2 messages present
    Expected Result: Session file created with correct content
    Failure Indicators: File not created, wrong message count
    Evidence: .sisyphus/evidence/task-4-save-cmd.txt

  Scenario: /load command switches sessions
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl loaded, 2 sessions exist
    Steps:
      1. Create test dir, save "session-A" with 2 messages, "session-B" with 3 messages
      2. Set *current-session-id* to "session-A"
      3. Create agent, load session-A messages into it
      4. Call (handle-load-session-command agent "/load session-B")
      5. Assert agent now has 3 messages (from session-B)
      6. Assert *current-session-id* is "session-B"
      7. Verify session-A was auto-saved (file exists, still has 2 messages)
    Expected Result: Session switched, old session preserved
    Failure Indicators: Messages not swapped, old session not saved, wrong session-id
    Evidence: .sisyphus/evidence/task-4-load-cmd.txt

  Scenario: /load with invalid session-id shows error
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl loaded
    Steps:
      1. Call (handle-load-session-command agent "/load nonexistent-999")
      2. Assert returns nil (no crash)
      3. Assert agent's conversation unchanged
    Expected Result: Graceful error message, REPL continues
    Failure Indicators: Crash, conversation cleared
    Evidence: .sisyphus/evidence/task-4-load-missing.txt
  ```

  **Evidence to Capture:**
  - [ ] task-4-sessions-cmd.txt
  - [ ] task-4-save-cmd.txt
  - [ ] task-4-load-cmd.txt
  - [ ] task-4-load-missing.txt

  **Commit**: YES (groups with Tasks 3, 5)
  - Message: `feat(repl): add session file I/O, REPL commands, and start-repl integration`
  - Files: `src/repl/session.lisp`, `src/repl.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sb-ext:exit)'`

- [x] 5. REPL Integration — start-repl :session-id, exit-repl hook, /reset hook, banner

  **What to do**:
  - Modify `start-repl` signature to accept new keywords:
    ```lisp
    (defun start-repl (&key client
                            (system-prompt sibyl.agent::*default-system-prompt*)
                            (name "Sibyl")
                            session-id
                            (auto-save-interval 300))
    ```
  - Add `*current-session-id*` dynamic variable (defvar) at top of `src/repl.lisp`
  - Add `*auto-save-interval*` dynamic variable
  - Session initialization in `start-repl` body (before `block repl-loop`):
    - If `session-id` provided: call `load-session` → if found, populate agent's memory with loaded messages and summary; if not found, print warning and start fresh
    - If `session-id` nil: call `generate-session-id` to create new session
    - Set `*current-session-id*`
    - Print session info in banner: `"Session: <id>"`
  - Modify `exit-repl` labels function to save session before exit:
    - After `save-history` call (line ~1333): add session save
    - Get messages from agent's conversation, summary from memory
    - Call `save-session` with `*current-session-id*`
    - Print: `"Session saved: <id>"` and `"Resume with: (start-repl :client ... :session-id \"<id>\")"` 
  - Modify `handle-reset-command` to auto-save before clearing:
    - Before `agent-reset` call: save current session
    - Print: `"Session saved before reset: <id>"`
    - Then proceed with normal reset
  - Wire the `auto-save-interval` to the timer (Task 6 will implement the timer itself)

  **Must NOT do**:
  - Do NOT implement the auto-save timer logic here — that's Task 6
  - Do NOT modify the interrupt handler — exit-repl already handles all exit paths
  - Do NOT change the existing REPL loop structure
  - Do NOT add session-id to the `sibyl` facade package yet

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: Modifying the core REPL startup/shutdown flow requires careful understanding of the labels/block/catch architecture and all exit paths
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `playwright`: Not relevant

  **Parallelization**:
  - **Can Run In Parallel**: YES (with Tasks 3 and 4 in theory, but depends on Task 3)
  - **Parallel Group**: Wave 2 (after Task 3 completes)
  - **Blocks**: Tasks 6, 7
  - **Blocked By**: Tasks 1, 2, 3

  **References**:

  **Pattern References**:
  - `src/repl.lisp:1241-1260` — `start-repl` function definition. Current signature: `(&key client system-prompt name)`. Add `:session-id` and `:auto-save-interval` here.
  - `src/repl.lisp:1325-1340` — `exit-repl` labels function. Contains `save-history`, MCP disconnect. Add session save AFTER `save-history` but BEFORE `return-from repl-loop`.
  - `src/repl.lisp:1329` — `*last-interrupt-time*` reset. This is inside `start-repl` body, before the REPL loop. Session loading should happen near here.
  - `src/repl.lisp:1343-1509` — `repl-body` labels function. The main loop. Do NOT modify this.
  - `src/repl.lisp:1510-1518` — Post-loop `sb-ext:exit`. Session must be saved BEFORE this point (inside exit-repl).
  - `src/repl.lisp:338-343` — `handle-reset-command`. Add session save before `agent-reset`.
  - `src/repl.lisp:1285-1320` — Banner printing section. Add session-id display here.

  **API/Type References**:
  - `src/repl/session.lisp` — `save-session`, `load-session`, `generate-session-id` from Tasks 1+3
  - `src/agent/memory.lisp` — `memory-conversation`, `memory-summary`, `(setf memory-summary)`, `memory-reset`
  - `src/agent/core.lisp:298-302` — `agent-reset`. Called by `/reset`. Need to save before this clears everything.
  - `src/llm/message.lisp` — `conversation-to-list`, `conversation-clear`, `conversation-push` (line 79 — adds message under lock; use this to restore messages)

  **WHY Each Reference Matters**:
  - `exit-repl` is the ONLY place to save on exit — `sb-ext:exit` happens right after the loop
  - Banner section shows where to add session info display
  - `agent-reset` clears everything including summary — must save BEFORE it's called
  - The labels/block/catch architecture means session state must be accessible from `exit-repl` closure

  **Acceptance Criteria**:

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: start-repl with session-id loads existing session
    Tool: Bash (sbcl --eval)
    Preconditions: A session file exists with known content
    Steps:
      1. Create test session file with 2 messages via save-session
      2. Programmatically verify that load-session returns those messages
      3. Verify *current-session-id* would be set correctly in start-repl flow
      (Note: Can't fully test start-repl interactively — test the loading path programmatically)
    Expected Result: Session loaded, messages available
    Failure Indicators: Messages not loaded, error on missing session
    Evidence: .sisyphus/evidence/task-5-load-on-start.txt

   Scenario: start-repl without session-id generates new session
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl loaded
    Steps:
      1. Call generate-session-id
      2. Assert result matches pattern "session-YYYYMMDD-HHMMSS-NNNNNN" (regex: "^session-\\d{8}-\\d{6}-\\d+$")
      3. Assert calling it twice produces different IDs (random suffix ensures uniqueness even within same second)
    Expected Result: Valid timestamp-based session IDs with random suffix
    Failure Indicators: Wrong format, duplicate IDs
    Evidence: .sisyphus/evidence/task-5-generate-id.txt

  Scenario: /reset saves session before clearing
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl loaded, session with messages
    Steps:
      1. Create test dir, create agent with 3 messages
      2. Set *current-session-id*, *default-session-directory*
      3. Call handle-reset-command
      4. Verify session file was saved with 3 messages
      5. Verify agent's conversation is now empty (reset worked)
    Expected Result: Session preserved on disk, memory cleared
    Failure Indicators: File not saved, messages still in memory, or file saved with 0 messages
    Evidence: .sisyphus/evidence/task-5-reset-saves.txt

  Scenario: exit-repl path includes session save
    Tool: Bash (grep/read)
    Preconditions: Task 5 implementation complete
    Steps:
      1. Read src/repl.lisp, find exit-repl labels function
      2. Verify save-session call exists inside exit-repl, after save-history
      3. Verify session-id and resume instructions are printed
    Expected Result: save-session call present in exit-repl path
    Failure Indicators: No save-session in exit-repl, save after sb-ext:exit
    Evidence: .sisyphus/evidence/task-5-exit-path.txt
  ```

  **Evidence to Capture:**
  - [ ] task-5-load-on-start.txt
  - [ ] task-5-generate-id.txt
  - [ ] task-5-reset-saves.txt
  - [ ] task-5-exit-path.txt

  **Commit**: YES (groups with Tasks 3, 4)
  - Message: `feat(repl): add session file I/O, REPL commands, and start-repl integration`
  - Files: `src/repl/session.lisp`, `src/repl.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sb-ext:exit)'`

- [x] 6. Auto-save Timer — bordeaux-threads periodic save with configurable interval

  **What to do**:
  - Implement in `src/repl/session.lisp`:
  - Define `*auto-save-thread*` dynamic variable (nil when not running)
  - Define `*auto-save-stop-flag*` dynamic variable (nil = running, t = stop)
  - Implement `start-auto-save-timer (get-state-fn interval)`:
    - `get-state-fn` is a closure that returns `(values session-id messages summary)` — passed from start-repl where agent is in scope
    - Start a bordeaux-threads thread that:
      1. Loops: sleep for `interval` seconds (but check stop-flag every 1 second within the sleep)
      2. On each interval: call `get-state-fn`, then `save-session`
      3. Wrap save in handler-case to catch file errors (log warning, don't crash)
      4. Exit loop when `*auto-save-stop-flag*` is T
    - Store thread in `*auto-save-thread*`
    - Use granular sleep (1s increments checking stop-flag) so shutdown is responsive
  - Implement `stop-auto-save-timer ()`:
    - Set `*auto-save-stop-flag*` to T
    - If `*auto-save-thread*` is non-nil and alive, join it (with 5s timeout)
    - Set `*auto-save-thread*` to nil
    - Log debug message on stop
  - Wire into `start-repl`:
    - After session initialization, before REPL loop: call `start-auto-save-timer`
    - The `get-state-fn` closure captures `agent` and `*current-session-id*`
  - Wire into `exit-repl`:
    - Call `stop-auto-save-timer` BEFORE session save (avoid race with final save)
  - Protect `save-session` calls with `*session-save-lock*` — both auto-save and manual save use the same lock

  **Must NOT do**:
  - Do NOT use `sleep` for the full interval (5 min) in a single call — use 1s granular sleep with stop-flag check
  - Do NOT crash the REPL if auto-save fails — catch and log
  - Do NOT save if conversation is empty (skip no-op saves)
  - Do NOT modify the spinner thread pattern — just reference it

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Thread lifecycle management with stop-flag pattern, closure-based state access, error handling
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `playwright`: Not relevant

  **Parallelization**:
  - **Can Run In Parallel**: YES (with Task 7 if Task 7 doesn't test timer)
  - **Parallel Group**: Wave 3 (after Wave 2)
  - **Blocks**: Task 7
  - **Blocked By**: Tasks 3, 5

  **References**:

  **Pattern References**:
  - `src/repl/spinner.lisp:50-80` — Stop-flag + sleep loop pattern for background threads. The spinner uses `(loop ... (sleep 0.1) ... (when stop-flag (return)))`. Adapt this for 1-second granular sleep checking stop-flag.
  - `src/repl/spinner.lisp:20-30` — Thread creation with `bt:make-thread`. Follow naming convention: `:name "session-auto-save"`.

  **API/Type References**:
  - `src/repl/session.lisp` — `save-session` from Task 3, `*session-save-lock*` from Task 3
  - `src/agent/memory.lisp` — `memory-conversation`, `memory-summary` for the state closure
  - `src/llm/message.lisp` — `conversation-to-list` for thread-safe message snapshot

  **External References**:
  - bordeaux-threads API: `bt:make-thread`, `bt:join-thread`, `bt:thread-alive-p`, `bt:with-lock-held`

  **WHY Each Reference Matters**:
  - Spinner pattern is the ONLY existing thread lifecycle pattern in the codebase — follow it
  - `conversation-to-list` already uses locks for thread-safe access — critical for auto-save reading while main thread writes
  - `save-session` must be protected by `*session-save-lock*` to avoid corruption if manual `/save` and auto-save fire simultaneously

  **Acceptance Criteria**:

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: Auto-save timer creates file after interval
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl loaded
    Steps:
      1. Create test dir
      2. Create mock get-state-fn that returns ("test-auto" messages "summary")
      3. Start timer with interval=2 (seconds)
      4. Sleep 3 seconds
      5. Stop timer
      6. Verify session file exists for "test-auto"
      7. Load and verify messages match
    Expected Result: File created automatically within interval
    Failure Indicators: No file created, wrong content, timer didn't stop
    Evidence: .sisyphus/evidence/task-6-auto-save-fires.txt

  Scenario: Timer stops cleanly on stop-auto-save-timer
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl loaded, timer running
    Steps:
      1. Start timer with interval=60 (long, shouldn't fire during test)
      2. Immediately call stop-auto-save-timer
      3. Assert *auto-save-thread* is nil
      4. Assert *auto-save-stop-flag* is T
      5. Assert function returns within 5 seconds (not hung)
    Expected Result: Timer stopped, thread cleaned up
    Failure Indicators: Hung thread, non-nil thread variable, timeout
    Evidence: .sisyphus/evidence/task-6-timer-stop.txt

  Scenario: Auto-save handles file error gracefully
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl loaded
    Steps:
      1. Create get-state-fn pointing to non-writable directory (e.g., "/root/no-access/")
      2. Start timer with interval=1
      3. Sleep 2 seconds
      4. Stop timer
      5. Assert no unhandled error (REPL would still be running)
    Expected Result: Error caught and logged, no crash
    Failure Indicators: Unhandled condition, thread crash
    Evidence: .sisyphus/evidence/task-6-error-handling.txt

  Scenario: Auto-save skips empty conversation
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl loaded
    Steps:
      1. Create get-state-fn that returns ("empty-test" nil nil) — empty messages
      2. Start timer with interval=1
      3. Sleep 2 seconds
      4. Stop timer
      5. Assert no file created for "empty-test" (save was skipped)
    Expected Result: No file written for empty conversation
    Failure Indicators: File created with empty content
    Evidence: .sisyphus/evidence/task-6-skip-empty.txt
  ```

  **Evidence to Capture:**
  - [ ] task-6-auto-save-fires.txt
  - [ ] task-6-timer-stop.txt
  - [ ] task-6-error-handling.txt
  - [ ] task-6-skip-empty.txt

  **Commit**: YES
  - Message: `feat(repl): add auto-save timer with configurable interval`
  - Files: `src/repl/session.lisp`, `src/repl.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sb-ext:exit)'`

- [x] 7. Tests — session-test.lisp with FiveAM suite

  **What to do**:
  - Create `tests/session-test.lisp` with FiveAM test suite `session-tests`
  - Add test suite to `*unsafe-suites*` in `tests/suite.lisp` (file I/O = unsafe)
  - Add `(:file "session-test")` to `:sibyl/tests` system in `sibyl.asd`
  - Test categories:
    1. **Serialization round-trips**:
       - `test-tool-call-roundtrip`: tool-call with arguments → sexp → tool-call, verify all slots
       - `test-tool-call-nil-args`: tool-call with nil arguments round-trips
       - `test-message-user-roundtrip`: user message → sexp → message
       - `test-message-assistant-with-tools`: assistant message with tool-calls → sexp → message
       - `test-message-tool-result`: tool-result message with tool-call-id → sexp → message
       - `test-session-roundtrip`: full session with mixed messages + summary
       - `test-system-messages-filtered`: :system messages excluded from serialization
    2. **File I/O**:
       - `test-save-and-load`: save session, load it back, compare
       - `test-load-nonexistent`: load missing session returns nil
       - `test-list-sessions`: save multiple sessions, list returns correct metadata
       - `test-index-rebuild`: delete index file, list triggers rebuild
    3. **Session ID generation**:
       - `test-generate-session-id`: format matches `session-YYYYMMDD-HHMMSS`
       - `test-unique-session-ids`: two calls produce different IDs (with sleep 1)
    4. **Command handlers** (mock-based):
       - `test-sessions-command`: handle-sessions-command produces output
       - `test-save-command`: handle-save-session-command creates file
       - `test-load-command`: handle-load-session-command switches sessions
       - `test-load-missing-command`: handle-load-session-command with bad ID doesn't crash
    5. **Auto-save timer**:
       - `test-timer-fires`: start with 1s interval, sleep 2s, verify file
       - `test-timer-stops`: start timer, stop immediately, verify stopped
  - Use temp directories for all file I/O tests: `(merge-pathnames (format nil "sibyl-test-~a/" (get-universal-time)) (uiop:temporary-directory))`
  - Clean up temp dirs in test fixtures (handler-case around cleanup for robustness)
  - Run full test suite to verify no regressions

  **Must NOT do**:
  - Do NOT use real `~/.sibyl/sessions/` in tests — always use temp dirs
  - Do NOT make tests depend on other test suites (no cross-suite depends-on)
  - Do NOT use `sleep` longer than 2 seconds in any test
  - Do NOT skip classifying the suite in `*unsafe-suites*`

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Many test cases covering serialization, I/O, commands, and timer — substantial but patterned work
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `playwright`: Not relevant

  **Parallelization**:
  - **Can Run In Parallel**: NO (depends on all implementation tasks)
  - **Parallel Group**: Wave 3 (sequential after Task 6)
  - **Blocks**: F1-F4
  - **Blocked By**: Tasks 1, 2, 3, 4, 5, 6

  **References**:

  **Pattern References**:
  - `tests/rich-repl-test.lisp:1-30` — FiveAM test suite definition pattern. Shows `(def-suite :rich-repl-tests)`, `(in-suite :rich-repl-tests)`, `(test test-name ...)` pattern.
  - `tests/rich-repl-test.lisp:400-478` — `interrupt-handler-tests` subsuite. Shows mock agent creation, let-binding of dynamic variables for testing.
  - `tests/suite.lisp:30-60` — `*unsafe-suites*` list. Add `:session-tests` here.
  - `tests/cache-test.lisp:1-30` — Shows mock CLOS class creation pattern for testing (mock-llm-client).
  - `sibyl.asd` — `:sibyl/tests` system definition. Add `(:file "session-test")` to components.

  **API/Type References**:
  - `src/repl/session.lisp` — All functions from Tasks 1-6: tool-call->sexp, message->sexp, session->sexp, save-session, load-session, list-sessions, generate-session-id, start-auto-save-timer, stop-auto-save-timer
  - `src/llm/message.lisp` — `make-message`, `user-message`, `assistant-message`, `tool-result-message`, `make-tool-call`
  - `src/agent/core.lisp` — `make-agent` or mock agent creation for command handler tests

  **WHY Each Reference Matters**:
  - Test suite patterns must match existing conventions for the parallel runner to classify correctly
  - Mock patterns from cache-test and rich-repl-test show how to create test doubles
  - Adding to `*unsafe-suites*` is MANDATORY — file I/O tests must not run in parallel with each other

  **Acceptance Criteria**:

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: All session tests pass
    Tool: Bash (sbcl --eval)
    Preconditions: All implementation tasks (1-6) complete
    Steps:
      1. Run: sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(5am:run! :session-tests)' --eval '(sb-ext:exit)'
      2. Assert output shows all tests passing
      3. Assert 0 failures, 0 errors
    Expected Result: All tests pass
    Failure Indicators: Any test failure or error
    Evidence: .sisyphus/evidence/task-7-session-tests.txt

  Scenario: Full test suite still passes (no regressions)
    Tool: Bash (sbcl --eval)
    Preconditions: All tasks complete, tests added
    Steps:
      1. Run: sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(sibyl.tests:run-tests-parallel)' --eval '(sb-ext:exit)'
      2. Assert 0 failures, 0 errors
      3. Assert session-tests appears in output
      4. Assert total check count >= 1505 (existing) + new session tests
    Expected Result: Zero regressions, new tests included
    Failure Indicators: Pre-existing tests fail, session-tests not found
    Evidence: .sisyphus/evidence/task-7-full-suite.txt

  Scenario: Suite classified correctly
    Tool: Bash (grep)
    Preconditions: tests/suite.lisp updated
    Steps:
      1. Grep tests/suite.lisp for "session-tests"
      2. Assert it appears in *unsafe-suites* list
    Expected Result: :session-tests in *unsafe-suites*
    Failure Indicators: Missing or in wrong list
    Evidence: .sisyphus/evidence/task-7-suite-class.txt
  ```

  **Evidence to Capture:**
  - [ ] task-7-session-tests.txt
  - [ ] task-7-full-suite.txt
  - [ ] task-7-suite-class.txt

  **Commit**: YES
  - Message: `test(repl): add session persistence test suite`
  - Files: `tests/session-test.lisp`, `tests/suite.lisp`, `sibyl.asd`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(sibyl.tests:run-tests-parallel)' --eval '(sb-ext:exit)'`

---

## Final Verification Wave (MANDATORY — after ALL implementation tasks)

> 4 review agents run in PARALLEL. ALL must APPROVE. Rejection → fix → re-run.

- [x] F1. **Plan Compliance Audit** — `oracle`
  Read the plan end-to-end. For each "Must Have": verify implementation exists (read file, run sbcl --eval, check exports). For each "Must NOT Have": search codebase for forbidden patterns — reject with file:line if found. Check evidence files exist in .sisyphus/evidence/. Compare deliverables against plan.
  Output: `Must Have [N/N] | Must NOT Have [N/N] | Tasks [N/N] | VERDICT: APPROVE/REJECT`

- [x] F2. **Code Quality Review** — `unspecified-high`
  Run `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(sibyl.tests:run-tests-parallel)' --eval '(sb-ext:exit)'`. Review `src/repl/session.lisp` for: missing error handling, thread safety issues, resource leaks (unclosed streams), hardcoded paths. Check AI slop: excessive comments, over-abstraction, generic variable names.
  Output: `Tests [N pass/N fail] | Files [N clean/N issues] | VERDICT`

- [x] F3. **Real QA — Programmatic Verification** — `unspecified-high`
  Execute EVERY QA scenario from EVERY task using `sbcl --eval`. Test cross-task integration: save → load → modify → save → load again. Test edge cases: empty session, corrupt file, missing directory. Save evidence to `.sisyphus/evidence/final-qa/`.
  Output: `Scenarios [N/N pass] | Integration [N/N] | Edge Cases [N tested] | VERDICT`

- [x] F4. **Scope Fidelity Check** — `deep`
  For each task: read "What to do", read actual implementation. Verify 1:1 — everything specified was built, nothing beyond spec was built. Check "Must NOT do" compliance. Check for cross-task contamination. Flag unaccounted changes.
  Output: `Tasks [N/N compliant] | Contamination [CLEAN/N issues] | Unaccounted [CLEAN/N files] | VERDICT`

---

## Commit Strategy

- **Commit 1** (after Task 1+2): `feat(repl): add session serialization layer and package exports`
  - Files: `src/repl/session.lisp`, `src/packages.lisp`, `sibyl.asd`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sb-ext:exit)'`

- **Commit 2** (after Task 3+4+5): `feat(repl): add session file I/O, REPL commands, and start-repl integration`
  - Files: `src/repl/session.lisp`, `src/repl.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sb-ext:exit)'`

- **Commit 3** (after Task 6): `feat(repl): add auto-save timer with configurable interval`
  - Files: `src/repl/session.lisp`, `src/repl.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sb-ext:exit)'`

- **Commit 4** (after Task 7): `test(repl): add session persistence test suite`
  - Files: `tests/session-test.lisp`, `tests/suite.lisp`, `sibyl.asd`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(sibyl.tests:run-tests-parallel)' --eval '(sb-ext:exit)'`

---

## Success Criteria

### Verification Commands
```bash
# Load system cleanly
sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sb-ext:exit :code 0)'
# Expected: exits with code 0, no errors

# Run all tests
sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(sibyl.tests:run-tests-parallel)' --eval '(sb-ext:exit :code 0)'
# Expected: 0 failures, 0 errors, session-tests suite appears in output

# Verify session file creation (programmatic)
sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(let ((id (sibyl.repl::generate-session-id))) (sibyl.repl::save-session id (list (sibyl.llm:user-message "test")) nil) (assert (probe-file (sibyl.repl::%session-file-path id))) (format t "OK: session ~a created~%" id))' --eval '(sb-ext:exit :code 0)'
# Expected: "OK: session session-YYYYMMDD-HHMMSS created"
```

### Final Checklist
- [x] All "Must Have" present
- [x] All "Must NOT Have" absent
- [x] All existing tests pass (≥1505 checks)
- [x] New session tests pass
- [x] Session round-trip works with tool-call messages
- [x] Auto-save timer starts and stops cleanly
- [x] /sessions, /save, /load commands registered and functional
