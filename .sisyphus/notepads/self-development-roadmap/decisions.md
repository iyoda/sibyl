## [SESSION START] Architectural Decisions

### Phase 0 Strategy
- **S-expression reading**: Using `CL:READ` (not Eclector in Phase 0)
  - Rationale: Simpler, sufficient for initial implementation
  - Trade-off: Loses comments, but file editing will be text-based anyway
  
- **Compilation strategy**: Auto-compile after every `eval`
  - Rationale: Required for `sb-introspect:who-calls` to work
  - Implementation: `eval-form` tool will handle this automatically
  
- **Rollback mechanism**: `fdefinition` object capture
  - Rationale: `function-lambda-expression` can return NIL
  - Pattern: Save function object before redefinition

### Parallelization Decisions
- Wave 0-A: read-sexp, describe-symbol, eval-form (independent implementations)
- Wave 0-B: macroexpand-form, package-symbols (independent implementations)
- Task 0-6 sequential: Must wait for all tools before ASDF integration

### Testing Approach
- TDD for all tasks (test-first)
- FiveAM suite: `sibyl-tests`
- Programmatic API: `fiveam:run` + `results-status` (not `run!`)
