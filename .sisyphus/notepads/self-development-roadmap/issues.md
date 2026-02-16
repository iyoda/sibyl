## [SESSION START] Known Issues & Gotchas

### SBCL/Lisp Specifics
- **sb-introspect:who-calls**: Only works on COMPILED code
  - Must `compile` all evaluated functions
  - Test functions must be compiled too for introspection tests
  
- **Closure capture semantics**: 
  - `#'function` captures function object (won't see redefinition)
  - `(function-call)` uses symbol (will see redefinition)
  - Sibyl's `deftool` uses symbol references → safe for redefinition
  
- **FiveAM output**: 
  - `fiveam:run!` prints to stdout (not what we want)
  - `fiveam:run` returns data structure (correct for programmatic use)

### Phase 0 Constraints
- **No Eclector yet**: Using basic `CL:READ`, which loses:
  - Source position annotations
  - Comments
  - Reader macros (beyond standard)
  - Solution: Will track line numbers manually via text parsing

### Potential Blockers
- None identified yet for Phase 0
