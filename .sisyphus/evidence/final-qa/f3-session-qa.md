# F3: Real QA - Programmatic Verification (session-persistence)

## Test Run
All scenarios executed via `sbcl --load .sisyphus/f3-qa.lisp`

## Results

| # | Scenario | Result |
|---|----------|--------|
| 1 | Full save-load round-trip with tool-calls (3 msgs, summary, command-count 42) | PASS |
| 2 | List sessions with correct metadata (2 sessions, message counts verified) | PASS |
| 3 | Load non-existent session returns nil gracefully | PASS |
| 4 | Session ID format (prefix, length >20, uniqueness) | PASS |
| 5 | System messages filtered on save (3 in -> 2 out, no :system) | PASS |
| 6 | Cross-task integration: save -> load -> modify -> save -> load again | PASS |
| 7 | Timer starts and stops cleanly (*auto-save-thread* nil, *auto-save-stop-flag* T) | PASS |
| 8 | Index rebuild after deletion (delete index.lisp, list rebuilds) | PASS |

## Summary
Scenarios [8/8 pass] | Integration [1/1] | Edge Cases [3 tested] | VERDICT: APPROVE
