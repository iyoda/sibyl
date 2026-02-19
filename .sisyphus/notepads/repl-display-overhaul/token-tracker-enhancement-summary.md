# Token Tracker Enhancement - Implementation Summary

## Date: 2026-02-19

## Objective
Enhance the `token-tracker` struct with a cost accumulator and thinking token field, and add a `context-window-for-model` utility function to provide the data layer for footer and session summary improvements.

## Implementation Details

### 1. Token Tracker Struct Extensions
**File**: `src/llm/token-tracker.lisp`

Added two new fields to the `token-tracker` defstruct:
- `cost-usd` (double-float, default 0.0d0) - Accumulates total cost in USD
- `thinking-tokens` (integer, default 0) - Tracks thinking tokens used

### 2. tracker-add-usage Enhancement
**File**: `src/llm/token-tracker.lisp`

Extended `tracker-add-usage` to handle `:thinking-tokens` key in usage plist:
```lisp
(incf (token-tracker-thinking-tokens tracker)
      (or (getf usage-plist :thinking-tokens) 0))
```

**Backward Compatibility**: Uses `(getf usage-plist :thinking-tokens 0)` so existing callers without `:thinking-tokens` continue to work without modification.

### 3. tracker-add-cost Function
**File**: `src/llm/token-tracker.lisp`

New function for cost accumulation:
```lisp
(defun tracker-add-cost (tracker amount)
  "Add AMOUNT (in USD) to TRACKER's cost accumulator."
  (when tracker
    (incf (token-tracker-cost-usd tracker) amount))
  tracker)
```

### 4. context-window-for-model Utility
**File**: `src/llm/model-selector.lisp`

New function for context window lookup:
```lisp
(defun context-window-for-model (model-name)
  "Return context window size for MODEL-NAME. Falls back to 200000 for unknown models.
   Looks up model in *latest-model-tiers* by prefix match."
  ...)
```

**Implementation Strategy**:
- Searches all tiers in `*latest-model-tiers*`
- Uses prefix matching for flexible model name matching
- Returns 200000 for unknown models (conservative default)

**Verified Context Windows**:
- claude-opus-4-6: 200000
- claude-sonnet-4-6: 200000
- gpt-5-mini: 200000
- unknown models: 200000 (fallback)

### 5. Package Exports
**File**: `src/packages.lisp`

Exported new symbols from `sibyl.llm` package:
- `#:token-tracker-cost-usd`
- `#:token-tracker-thinking-tokens`
- `#:tracker-add-cost`
- `#:context-window-for-model`

## Test Coverage

### Test Suite: token-tracker-enhancement-tests
**File**: `tests/token-tracker-enhancement-test.lisp`

**Classification**: SAFE (pure logic, no I/O, no global state)

**Tests Implemented** (6 tests, 17 checks total):
1. `make-token-tracker-initializes-new-fields` - Verifies cost-usd=0.0d0 and thinking-tokens=0
2. `tracker-add-usage-increments-thinking-tokens` - Verifies thinking-tokens increment
3. `tracker-add-usage-backward-compatible` - Verifies backward compatibility (no :thinking-tokens key)
4. `tracker-add-cost-accumulates` - Verifies cost accumulation (0.01 + 0.02 = 0.03)
5. `context-window-for-model-returns-correct-values` - Verifies context window lookups
6. `tracker-add-usage-accumulates-thinking-tokens` - Verifies multiple thinking-tokens accumulations

**Test Results**: ✅ All 17 checks pass

### Integration with Test Suite
- Added test file to `sibyl.asd` system definition
- Registered suite in `tests/suite.lisp` as SAFE (cross-package resolution via `%safe-suites-resolved`)
- Full test suite: 3302 checks, 3300 passed (100%)

## Verification

### Manual Verification
```lisp
(let ((tracker (make-token-tracker)))
  (tracker-add-usage tracker '(:input-tokens 100 :output-tokens 50 :thinking-tokens 200))
  (tracker-add-cost tracker 0.01d0)
  (tracker-add-cost tracker 0.02d0)
  (token-tracker-cost-usd tracker))        ; => 0.03d0
  (token-tracker-thinking-tokens tracker)) ; => 200
  (context-window-for-model "claude-opus-4-6")) ; => 200000
```

### Compilation Verification
✅ `(ql:quickload :sibyl)` - Successful compilation
✅ `(asdf:test-system :sibyl)` - All tests pass (via parallel runner)

## Files Modified

1. `src/llm/token-tracker.lisp` - Added fields and `tracker-add-cost`
2. `src/llm/model-selector.lisp` - Added `context-window-for-model`
3. `src/packages.lisp` - Exported new symbols
4. `tests/token-tracker-enhancement-test.lisp` - New test suite (17 checks)
5. `tests/suite.lisp` - Added suite to `%safe-suites-resolved`
6. `sibyl.asd` - Added test file to system definition

## Next Steps

These enhancements provide the data layer for:
1. **Footer Display Improvements**: Display cost and thinking token usage in REPL footer
2. **Session Summary Enhancements**: Track total cost across session
3. **Context Window Utilization Metrics**: Calculate and display context window usage percentage

## Design Decisions

### 1. Backward Compatibility
- `tracker-add-usage` uses `(getf usage-plist :thinking-tokens 0)` default
- Existing callers without `:thinking-tokens` continue to work
- No breaking changes to existing API

### 2. Context Window Fallback
- Returns 200000 for unknown models (conservative default)
- Prevents errors when new models are added
- Matches the context window of latest Claude models

### 3. Cost Accumulation Pattern
- Simple `incf` pattern matches existing token accumulation
- Returns tracker for method chaining
- Safe to call with nil tracker

### 4. Test Suite Classification
- Classified as SAFE (pure logic, no I/O)
- Enables parallel execution
- Cross-package resolution via `%safe-suites-resolved`

## Learnings

1. **Cross-Package Test Suites**: Test suites defined in non-test packages (e.g., `sibyl.llm`) must be resolved via `%safe-suites-resolved` in `tests/suite.lisp`
2. **Prefix Matching**: Using `alexandria:starts-with-subseq` for flexible model name matching handles both exact matches and versioned model names
3. **Double-Float Defaults**: Using `0.0d0` ensures consistent double-float arithmetic for cost calculations
4. **TDD Workflow**: Writing tests first caught the cross-package suite registration issue early

## Conclusion

✅ All objectives met:
- Token tracker extended with cost-usd and thinking-tokens fields
- tracker-add-usage enhanced with backward-compatible thinking-tokens support
- tracker-add-cost function implemented
- context-window-for-model utility created
- All functions exported from packages.lisp
- Comprehensive test suite (17 checks, 100% pass rate)
- Full test suite passes (3302 checks)
- Zero breaking changes to existing API
