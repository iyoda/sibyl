# Rich REPL — Issues / Gotchas

## [2026-02-17] Session ses_3954c162bffeQlfSjVgjT9e4lr

### KNOWN BLOCKERS (Both Resolved in Plan)

1. **`make-agent` doesn't accept `:hooks`** — Use `setf (agent-hooks agent)` post-construction
2. **Streaming has no call path without core changes** — Resolved via `*streaming-text-callback*` dynamic variable

### POTENTIAL GOTCHAS

- `*use-colors*` defaults to NIL currently (unbound). After fix, defaults to T. Tests use `search` (tolerates ANSI) — should be safe. But new tests that check exact output MUST use `(let ((*use-colors* nil)) ...)`.

- `repl-tests` suite in `tests/repl-test.lisp` is NOT a child of `sibyl-tests`. This is an existing issue. Don't try to fix it in this plan — it's out of scope.

- `src/repl/` directory doesn't exist yet. Task 2 creates it. The ASDF module restructuring changes `(:file "repl")` at line 41 to `(:module "repl" :components ((:file "spinner") (:file "repl")))` — this may require moving/renaming repl.lisp OR using `:pathname "repl"` on an individual file component.

- cl-readline CFFI: On macOS, may need `brew install readline && brew link readline --force`. The library is at `/opt/homebrew/opt/readline/lib/libreadline.dylib` (Apple Silicon) or `/usr/local/opt/readline/lib/libreadline.dylib` (Intel).

- Spinner must capture `*standard-output*` at thread creation time: `(let ((out *standard-output*)) (bt:make-thread (lambda () (write-string ... out)) ...))`. Do NOT use `*standard-output*` directly inside thread body — its binding is thread-local.

- Double Ctrl+C detection: use `get-internal-real-time` for the 2-second window. SBCL's `sb-sys:interactive-interrupt` doesn't guarantee delivery timing — be generous with the window (maybe 2.5s).

- `unwind-protect` is IMPORTANT in spinner stop logic. If an error occurs while spinner is running, the terminal must be cleaned up. Always wrap `agent-run` + spinner in `unwind-protect`.
