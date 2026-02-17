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
