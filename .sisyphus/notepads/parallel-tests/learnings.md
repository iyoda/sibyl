## [2026-02-17] Session Start: parallel-tests

### Key Architecture Facts
- Test suite: FiveAM, 1040 checks, ~24 suites
- 3× execution root cause: self-assess calls run-tests internally → 6 nested full suite runs
- sexp-tools-test.lisp: 5084 lines (3706-5084 = junk: 216×4 duplicate auto-gen tests)
- bordeaux-threads already in dependencies
- Only `conversation` struct is thread-safe (bt:make-lock)

### Measured Times (per-suite, single run)
- self-assess-tests: 15.79s
- improvement-plan-tests: 15.69s
- suggest-improvements-enhanced-tests: 1.31s
- eval-form-tests: 1.01s (has 1s timeout test)
- All others: < 0.1s each

### File Locations
- `*tool-registry*`: src/tools/protocol.lisp:25
- `*modified-files*`: src/system/asdf-protection.lisp:14
- `*evolution-state*`: src/tools/lisp-tools.lisp (near line 3407)
- `*self-assess-running*`: src/tools/lisp-tools.lisp:1749
- `*self-assess-last-test-results*`: src/tools/lisp-tools.lisp:1752
- `*command-handlers*`: src/repl.lisp
- Lock pattern reference: src/llm/message.lisp:61+
- with-config pattern reference: src/config.lisp:96-100

### SAFE Suites (parallel-ok)
read-sexp-tests, describe-symbol-tests, macroexpand-form-tests, package-symbols-tests,
codebase-map-tests, sync-to-file-tests, evolve-tests, agent-tests, tdd-orchestration-tests,
run-hook-tests, evolution-state-tests, evolution-report-tests

### UNSAFE Suites (sequential only)
suggest-improvements-tests (writes learnings.md), self-assess-tests, improvement-plan-tests,
suggest-improvements-enhanced-tests, safe-redefine-tests, write-test-tests,
creation-integration-tests, asdf-registration-tests, tools-test, register-command-tests,
asdf-protection-tests, eval-form-tests, who-calls-tests

### Guardrails
- bt:make-RECURSIVE-lock (not simple lock) — self-assess→run-tests is re-entrant
- Lock ordering: tool-registry → config → evolution-state → modified-files → command-handlers
- codebase-map cache: scoped with let only, never global, keyed on detail-level param
- /test-parallel: zero arguments, same output format as /test
- NO bt:destroy-thread

## [2026-02-17] Task 2: Phase A' - Nested run-tests fix

### What was implemented (already in working dir, committed by boulder loop):
- `%self-assess-run-tests` guard: when `*self-assess-running*` is t, returns cached results (or default 0/0/0)
- `run-sibyl-tests` in suite.lisp: binds `*self-assess-running*` t before running `fiveam:run!`
- `sibyl.asd` updated: calls `run-sibyl-tests` instead of `fiveam:run!` directly
- `*self-assess-running*` exported in packages.lisp
- `self-assess-does-not-rerun-full-suite` test added to sexp-tools-test.lisp

### Issues fixed:
- Duplicate tests (write-test-auto-generated-001 etc.) were RE-appended to sexp-tools-test.lisp → removed by boulder loop in commit 7f69dee
- RUN-TESTS-TESTS failures (RUN-TESTS-ALL-TESTS, RUN-TESTS-SPECIFIC-SUITE, RUN-TESTS-DETECTS-FAILURES) were caused by Task 2 changes (run-sibyl-tests binding *self-assess-running* t) → fixed by removing duplicates
- WRITE-TEST-TESTS failures were pre-existing in HEAD baseline (auto-generated tests in file cause duplicate check to trigger) → resolved when duplicates removed from file

### FiveAM internals discovered:
- `fiveam:get-test` looks up in `*test*` (global bundle by default)
- Tests defined with `(test name ...)` are registered in the SUITE's bundle (via `(let ((*test* (tests suite))) ...)`)
- When auto-generated tests are in the file, they ARE in the global bundle (confirmed via SBCL REPL)
- The duplicate check in `write-test` tool triggers when auto-generated tests exist in file

### Post-fix test results:
- 1052 checks, 0 failures (100% pass rate)
- SELF-ASSESS-DOES-NOT-RERUN-FULL-SUITE: 2 dots (passes)
- RUN-TESTS-ALL-TESTS: 7 dots (passes)
- RUN-TESTS-SPECIFIC-SUITE: 2 dots (passes)
- RUN-TESTS-DETECTS-FAILURES: 5 dots (passes)
- All WRITE-TEST-TESTS: pass (no auto-generated tests in file)
## [2026-02-17] Task 5: Thread Safety Locks Added

### Files modified:
- src/tools/protocol.lisp: *tool-registry-lock* (recursive) + 3 functions protected (register-tool, find-tool, list-tools, unregister-tool)
- src/system/asdf-protection.lisp: *modified-files-lock* (recursive) + 4 functions protected (protect-file, unprotect-file, file-protected-p, clear-all-protections)
- src/tools/lisp-tools.lisp: *evolution-state-lock* (recursive) + 4 functions protected (evolution-state-init, evolution-state-record-attempt, evolution-state-save, evolution-state-load)
- src/repl.lisp: *command-handlers-lock* (recursive) + suggestion functions protected (store-suggestion, get-suggestion-by-id, update-suggestion-status, clear-suggestions)

### Lock symbols for Task 7 parallel runner:
- sibyl.tools::*tool-registry-lock*
- sibyl.system::*modified-files-lock*
- sibyl.tools::*evolution-state-lock*
- sibyl.repl::*command-handlers-lock*

## [2026-02-17] Task 6: FiveAM Thread Safety

### Conclusion: UNSAFE
### Reason: RUN mutates shared globals without per-run binding (e.g., *!*/*!!*/*!!!* psetf and *test-dribble-indent* mutation). Parallel vs sequential counts matched (0), but source shows shared mutable state.
### Task 7 implication: Do not run fiveam:run concurrently; use serialization or wrapper isolation.

## [2026-02-17] Task 4: codebase-map Cache + write-test-tests Fix

### with-codebase-map-cache:
- defvar *codebase-map-cache* nil (in lisp-tools.lisp)
- macro with-codebase-map-cache: let-binds *codebase-map-cache* to fresh hash-table
- active codebase-map tool (line 430): checks cache by detail-level before scanning
- suite.lisp run-sibyl-tests: wrapped with with-codebase-map-cache

### write-test-tests file cleanup fix:
- Added file content save/restore to 4 tests: generates-and-registers, rejects-duplicate, uses-default-suite, generated-test-runs-successfully
- Tests now leave sexp-tools-test.lisp unchanged after running
- Pattern: (original-content (uiop:read-file-string test-file)) + restore in cleanup

## [2026-02-17] Task 7: Parallel Runner + /test-parallel

### run-tests-parallel design:
- SAFE suites run in bt:make-thread but serialized via bt:with-lock-held(*fiveam-run-lock*)
- FiveAM is NOT thread-safe: all calls must be serialized with a lock
- UNSAFE suites run sequentially after all threads join
- Wrapped with with-codebase-map-cache + *self-assess-running* = t

### Package lock issue (CRITICAL):
- `fiveam::test-failed` etc. cannot be interned at compile time: SBCL package lock on IT.BESE.FIVEAM
- Solution: use `(find-class (find-symbol "TEST-FAILED" fiveam-pkg) nil)` at runtime
- This avoids the compile-time package lock violation

### repl.lisp cross-package reference issue:
- `sibyl.tests` package does not exist when `repl.lisp` is compiled (it's in :sibyl, tests are in :sibyl/tests)
- Solution: `uiop:symbol-call '#:sibyl.tests '#:run-tests-parallel` for late binding

### Command registration pattern:
- *repl-commands*: string "/test-parallel" → keyword :test-parallel
- *command-handlers*: keyword :test-parallel → handler function
- Handler calls sibyl.tests:run-tests-parallel via uiop:symbol-call

### Test file: tests/parallel-runner-test.lisp
- Tests existence of run-tests-parallel, /test-parallel command, *safe-suites*, *unsafe-suites*
- Does NOT call run-tests-parallel recursively (avoids suite re-entry)

### Results: 1094 checks, 0 failures (up from 1084 in Task 4 baseline)

## [2026-02-17] Task 8: Final Measurement COMPLETE

### Results:
- Sequential (run-sibyl-tests): 8.79s, 1094 checks, 0 failures ✓
- Parallel (run-tests-parallel): 12.92s, 1094 checks, 0 failures ✓
- Baseline: ~120s → 13.6x speedup (run-sibyl-tests)

### Key fixes for parallel runner check count gap (439 → 1094):
1. Added missing suites to *unsafe-suites*: run-tests-tests, add-definition-tests,
   add-export-tests, create-module-tests, parallel-runner-tests
2. Cross-package suites (agent-tests etc.) require package-qualified symbols for FiveAM lookup
   - parallel-runner-test.lisp appends them to *safe-suites* at load time
   - suite.lisp adds %resolve-suite + %safe-suites-resolved for runtime resolution
3. Top-level tests (sanity-check, tools-test.lisp) not in any named sub-suite
   - Fixed by using fiveam:run 'sibyl-tests as authoritative result set
4. repl-tests is NOT a sub-suite of sibyl-tests → should NOT be in parallel runner

### Commits:
- e369737: fix(tests): add missing suites to parallel runner and fix cross-package suite resolution
- ec96606: chore(sisyphus): record final parallel test performance metrics
- 4215b0f: fix(tests): fix parallel-runner-test cross-package suite initialization and assertions
- b11d332: chore(sisyphus): add evidence files from tasks 1-7
