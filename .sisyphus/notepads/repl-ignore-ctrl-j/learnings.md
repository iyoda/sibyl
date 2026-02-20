
## 2026-02-19: Indentation fix + parenthesis structure

### Key discovery
The `(let ((*ignore-ctrl-j* ...)))` at L1267 in `start-repl` originally spanned the ENTIRE rest of the function body. The "indentation bug" was purely aesthetic (4-space body instead of 6-space), but the parens were balanced with the closing `)` at the very end of the function (line 1511: `(repl-body))))))`).

When adding a `)` to close the `let` early (after `(%ensure-utf8-locale)`), you MUST also remove one `)` from the end of the function — otherwise you get "unmatched close parenthesis" at EOF.

### Pattern: restructuring `let` scope
- Before: `let` wraps entire function tail → closes at very end (6 parens)  
- After: `let` wraps only readline setup forms → closes immediately, then function continues at `let*` scope (5 parens at end)
- Always: verify paren count with `git diff HEAD~1 HEAD -- src/repl.lisp | tail -5` first

### Comment separation convention
Two semantically distinct comment blocks should be separated by `;;` (empty comment line), not just concatenated.

### Test replacement approach
Tautological tests (testing `(eq x x)` or `(null nil)`) were replaced with behavioral tests that:
1. Mock `cl-readline:unbind-key` via `setf (symbol-function ...)` 
2. Capture arguments to verify the guard logic
3. Use `unwind-protect` to restore original function
4. Skip gracefully with `when (readline-available-p)` if cl-readline not loaded

- 2026-02-20 F4 scope audit: commit 3909588 modified out-of-scope file `src/tools/builtin.lisp`; plan scope fidelity fails even though config default and `%strip-ctrl-j`/`read-user-input` guardrails remained intact.
