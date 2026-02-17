# テスト並列実行高速化（2分→10秒）

## TL;DR

> **Quick Summary**: Sibylのテストスイート実行を2分超→10秒以下に高速化する。根本原因は`self-assess`がテスト中に`run-tests`を呼び出してスイート全体を6回再実行していること（×codebase-mapの重い処理）。4段階で修正: 重複テスト削除→ネスト実行防止→キャッシュ→並列化。
>
> **Deliverables**:
> - `tests/sexp-tools-test.lisp` のクリーンアップ（5084行→~3700行）
> - テスト中の`self-assess`ネスト実行防止
> - `with-codebase-map-cache`マクロによるテスト時キャッシュ
> - `run-tests-parallel`関数 + `/test-parallel` REPLコマンド
> - 全グローバル状態へのスレッドセーフ化（recursive locks）
>
> **Estimated Effort**: Medium
> **Parallel Execution**: YES - 4 waves
> **Critical Path**: Task 1 → Task 2 → 計測 → Task 3 → Task 4 → Task 5 → Task 6 → Task 7 → 計測 → Task 8

---

## Context

### Original Request
テストの実行を並列で行えるようにできないか。現行のものは待ち時間が長くなってきている。

### Interview Summary
**Key Discussions**:
- テスト実行時間の実測: `self-assess-tests` (15.79s) と `improvement-plan-tests` (15.69s) が全体の90%を占める
- FiveAMは並列実行をサポートしていない → 独自ランナーが必要
- `bordeaux-threads`は既に依存に含まれる
- アプローチ: A+B+C全部やる（ユーザー選択）
- テスト戦略: TDD
- ランナーAPI: 関数 + REPLコマンド両方
- キャッシュ: テスト時のみ

**Research Findings**:
- グローバル変数は`*tool-registry*`, `*config*`, `*evolution-state*`, `*command-handlers*`, `*modified-files*` — 全てロックなし
- `conversation`構造体のみ`bt:make-lock`で保護済み
- テストのファイルI/O: 全て`unwind-protect`でクリーンアップ済み
- `/tmp`パスはハードコード多い → 並列時に衝突リスク

### Metis Review
**Identified Gaps (addressed)**:
- **3重実行の真の原因**: 重複テスト登録ではなく、`self-assess`が内部で`run-tests`を呼びスイート全体を再実行。6回のネスト実行 × codebase-map = 126回のスキャン
- **フェーズ順序修正**: A'（ネスト防止）を最優先に。これだけで70-80%の高速化が見込める
- **FiveAMスレッド安全性**: 未検証。並列化の前に必須
- **ロック種別**: `bt:make-recursive-lock`が必要（`self-assess → run-tests`チェーンで再入する）
- **ロック順序定義**: デッドロック防止のため文書化が必要
- **キャッシュキー**: `detail-level`パラメータを含める必要
- **suggest-improvements-tests**: `learnings.md`を書き換える → UNSAFE分類

---

## Work Objectives

### Core Objective
テストスイートの実行時間を2分超から10秒以下に短縮する。テスト結果の正確性を保ちつつ、不要な重複実行を排除し、独立したスイートを並列実行する。

### Concrete Deliverables
- `tests/sexp-tools-test.lisp` — 重複コード削除（5084行→~3700行）
- `src/tools/lisp-tools.lisp` — `with-codebase-map-cache`マクロ追加
- `tests/suite.lisp` — `run-tests-parallel`関数追加
- `src/repl.lisp` — `/test-parallel` REPLコマンド追加
- `src/tools/protocol.lisp` — `*tool-registry*`へのrecursive lock追加
- `src/system/asdf-protection.lisp` — `*modified-files*`へのrecursive lock追加
- `src/tools/lisp-tools.lisp` — `*evolution-state*`へのrecursive lock追加
- `src/repl.lisp` — `*command-handlers*`, `*pending-suggestions*`へのrecursive lock追加
- `tests/parallel-runner-test.lisp` — 並列ランナーのテスト（TDD）
- `sibyl.asd` — 新テストファイル登録

### Definition of Done
- [ ] `(time (fiveam:run! 'sibyl.tests:sibyl-tests))` → 15秒以下
- [ ] `(sibyl.tests:run-tests-parallel)` → 全テスト合格、15秒以下
- [ ] 全既存テストがパス（1040 checks, 0 failures）
- [ ] `wc -l tests/sexp-tools-test.lisp` → 3710行以下

### Must Have
- テスト結果が順次実行と完全に一致すること
- 全フェーズでTDD（RED→GREEN→REFACTOR）
- 各フェーズ後に`(time (fiveam:run! 'sibyl.tests:sibyl-tests))`で計測
- スレッドエラーのハンドリング（`handler-case` + timeout）
- `/test-parallel` REPLコマンド

### Must NOT Have (Guardrails)
- テストの意味・動作の変更（実行戦略のみ変更）
- `codebase-map`以外のキャッシュ（scope creep防止）
- スレッドプール、ワークスティーリング、スケジューラ抽象化
- `/test-parallel`への引数・オプション追加（ゼロ引数）
- ロックフリー構造、STM、アクターモデル
- キャッシュのTTL、LRU、無効化ロジック
- プロダクションコードでのcodebase-mapキャッシュ
- `bt:destroy-thread`によるスレッド強制停止

---

## Verification Strategy

> **UNIVERSAL RULE: ZERO HUMAN INTERVENTION**
>
> ALL tasks in this plan MUST be verifiable WITHOUT any human action.

### Test Decision
- **Infrastructure exists**: YES
- **Automated tests**: TDD (RED-GREEN-REFACTOR)
- **Framework**: FiveAM (existing)

### Agent-Executed QA Scenarios (MANDATORY — ALL tasks)

**Verification Tool by Deliverable Type:**

| Type | Tool | How Agent Verifies |
|------|------|-------------------|
| Lisp code changes | Bash (sbcl --eval) | Load system, run tests, compare counts/timing |
| File cleanup | Bash (wc -l, grep) | Line count, content verification |
| Parallel runner | Bash (sbcl --eval) | Run parallel + sequential, compare results |

---

## Execution Strategy

### Parallel Execution Waves

```
Wave 1 (Start Immediately):
└── Task 1: Baseline measurement + duplicate cleanup

Wave 2 (After Wave 1):
├── Task 2: Fix nested run-tests (Phase A')
└── Task 3: Measure Phase A' impact

Wave 3 (After Wave 2):
├── Task 4: codebase-map cache (Phase B) + measure
├── Task 5: Global state thread-safety (locks)
└── Task 6: FiveAM thread-safety validation

Wave 4 (After Wave 3):
├── Task 7: Parallel runner + /test-parallel command (Phase C)
└── Task 8: Final measurement + regression verification

Critical Path: Task 1 → Task 2 → Task 4 → Task 7 → Task 8
Parallel Speedup: Tasks 4,5,6 can run in parallel
```

### Dependency Matrix

| Task | Depends On | Blocks | Can Parallelize With |
|------|------------|--------|---------------------|
| 1 | None | 2 | None |
| 2 | 1 | 3, 4 | None |
| 3 | 2 | 4 | None |
| 4 | 3 | 7 | 5, 6 |
| 5 | 2 | 7 | 4, 6 |
| 6 | 2 | 7 | 4, 5 |
| 7 | 4, 5, 6 | 8 | None |
| 8 | 7 | None | None |

### Agent Dispatch Summary

| Wave | Tasks | Recommended Agents |
|------|-------|-------------------|
| 1 | 1 | task(category="quick", load_skills=[], run_in_background=false) |
| 2 | 2, 3 | task(category="unspecified-low", ...), then task(category="quick", ...) |
| 3 | 4, 5, 6 | dispatch 3 parallel tasks |
| 4 | 7, 8 | task(category="unspecified-high", ...), then task(category="quick", ...) |

---

## TODOs

- [x] 1. Baseline Measurement + Duplicate Cleanup (Phase A)

  **What to do**:
  - 現在のテスト実行時間のベースラインを計測: `(time (fiveam:run! 'sibyl.tests:sibyl-tests))`
  - テスト数（checks）、パス数、フェイル数を記録
  - `tests/sexp-tools-test.lisp` から重複コードを削除:
    - 行994-1009: 4つのauto-generated tests（最初の出現）を削除
      - `write-test-auto-generated-001`, `write-test-duplicate-check`, `write-test-default-suite-check`, `write-test-runnable-check`
    - 行3706-5084付近: 216×4個の重複auto-generated testsを削除
    - 注意: 行番号ではなく**内容でマッチ**して削除すること（行番号はズレる可能性あり）
  - 削除後の検証:
    - `(compile-file (asdf:system-relative-pathname :sibyl "tests/sexp-tools-test.lisp"))` → エラーなし
    - `(asdf:test-system :sibyl)` → 全テストパス
    - `wc -l tests/sexp-tools-test.lisp` → 3710行以下

  **Must NOT do**:
  - write-test-tests スイート内のオリジナルテスト（行859-993）を削除しない
  - creation-integration-tests（行1010-3140）を変更しない
  - Phase 7追加スイート（行3141-3705）を変更しない
  - テストの意味を変更しない

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: ファイルの特定範囲を削除するだけの単純作業
  - **Skills**: []
    - No special skills needed

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 1 (solo)
  - **Blocks**: Tasks 2, 3
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `tests/sexp-tools-test.lisp:994-1009` — 最初のauto-generated test block（削除対象）
  - `tests/sexp-tools-test.lisp:1010` — creation-integration-testsの開始（ここは残す）
  - `tests/sexp-tools-test.lisp:3141-3705` — Phase 7追加スイート（ここは残す）
  - `tests/sexp-tools-test.lisp:3706+` — junkブロック開始（ここから末尾まで削除）

  **WHY Each Reference Matters**:
  - 行994-1009: `write-test-tests`スイートが`write-test`ツールで自動生成したテスト。FiveAMでは同名テストは上書きされるため、最後の定義だけが有効。最初の出現を削除しても影響なし。
  - 行3706+: サブエージェントが`write-test`ツールを繰り返し呼んで追記した216セットの重複。同名テストの再定義であり、全て不要。

  **Acceptance Criteria**:

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Baseline measurement before cleanup
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl/tests loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(time (fiveam:run! (quote sibyl.tests:sibyl-tests)))' --eval '(uiop:quit)'
      2. Record: wall time, total checks, pass count, fail count
      3. wc -l tests/sexp-tools-test.lisp → record line count
    Expected Result: Baseline numbers recorded (expect ~120s, 1040 checks, 0 failures, 5084 lines)
    Evidence: Terminal output captured to .sisyphus/evidence/task-1-baseline.txt

  Scenario: Post-cleanup verification
    Tool: Bash (sbcl --eval)
    Preconditions: Duplicate lines removed
    Steps:
      1. wc -l tests/sexp-tools-test.lisp
      2. Assert: line count < 3710
      3. grep -c 'write-test-auto-generated-001' tests/sexp-tools-test.lisp
      4. Assert: count = 1 (only the reference in write-test-generates-and-registers-test)
      5. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --eval '(uiop:quit)' (timeout: 300s)
      6. Assert: "Fail: 0" in output
    Expected Result: File cleaned up, all tests still pass
    Evidence: .sisyphus/evidence/task-1-cleanup.txt
  ```

  **Commit**: YES
  - Message: `fix(tests): remove 1380+ duplicate auto-generated test lines from sexp-tools-test`
  - Files: `tests/sexp-tools-test.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --eval '(uiop:quit)'`

---

- [x] 2. Fix Nested run-tests in self-assess (Phase A')

  **What to do**:
  - **RED**: テストを書く — `tests/sexp-tools-test.lisp`の`self-assess-tests`スイートに追加:
    ```lisp
    (test self-assess-does-not-rerun-full-suite
      "self-assess should not trigger full test suite re-execution during test runs."
      (let ((run-tests-call-count 0))
        ;; self-assess内部のrun-testsが呼ばれないことを確認
        ;; *self-assess-running*がtの時、run-testsをスキップすべき
        (let ((sibyl.tools::*self-assess-running* t))
          (let ((result (sibyl.tools:execute-tool "self-assess" nil)))
            (is (stringp result))
            ;; 結果がrun-testsの結果を含まないことを確認（キャッシュまたはスキップ）
            ))))
    ```
  - **GREEN**: `src/tools/lisp-tools.lisp`の`self-assess`ツール内で、`*self-assess-running*`が`t`の場合にrun-testsをスキップし、前回のキャッシュ結果(`*self-assess-last-test-results*`)を使うように修正
  - **追加修正**: `tests/suite.lisp`のテスト実行エントリポイントで`*self-assess-running*`を`t`にバインド:
    ```lisp
    ;; suite.lisp の test-op perform メソッド、または新しいラッパー関数
    (defun run-all-tests ()
      (let ((sibyl.tools::*self-assess-running* t))
        (fiveam:run! 'sibyl-tests)))
    ```
  - **REFACTOR**: 不要になったネスト防止ロジックがあれば整理

  **Must NOT do**:
  - `self-assess`のプロダクション動作を変更しない（REPLから呼んだ時は`run-tests`を実行すべき）
  - テストスイート外での`*self-assess-running*`の束縛を変更しない

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: 単一変数のスコーピング変更 + テスト追加。ロジックは明確。
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 2 (sequential after Task 1)
  - **Blocks**: Tasks 3, 4, 5, 6
  - **Blocked By**: Task 1

  **References**:

  **Pattern References**:
  - `src/tools/lisp-tools.lisp:1749` — `*self-assess-running*`のdefvar定義
  - `src/tools/lisp-tools.lisp:1752` — `*self-assess-last-test-results*`のdefvar定義
  - `src/tools/lisp-tools.lisp` — `%self-assess-run-tests`関数（`*self-assess-running*`ガード）

  **API/Type References**:
  - `tests/suite.lisp` — テストスイートのルート定義（`sibyl-tests`）
  - `sibyl.asd` — `test-op` perform メソッド定義

  **WHY Each Reference Matters**:
  - `*self-assess-running*`は現在self-assess内でのみ`t`に束縛される。テストランナーレベルで束縛することで、全テスト実行中はself-assessがrun-testsをスキップし、前回キャッシュを使う。
  - `*self-assess-last-test-results*`はself-assessの前回テスト結果キャッシュ。テスト実行開始時に1回だけrun-testsを実行してキャッシュし、以降はキャッシュを返す戦略。

  **Acceptance Criteria**:

  **TDD:**
  - [ ] テスト: `self-assess-does-not-rerun-full-suite` → RED → GREEN → PASS
  - [ ] `(asdf:test-system :sibyl)` → 全テストパス（0 failures）

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Nested run-tests eliminated
    Tool: Bash (sbcl --eval)
    Preconditions: Task 1 completed, sibyl/tests loaded
    Steps:
      1. Load system: (ql:quickload :sibyl/tests :silent t)
      2. Run: (time (fiveam:run! 'sibyl.tests:sibyl-tests))
      3. Assert: wall time < 60 seconds (was ~120s)
      4. Assert: "Fail: 0" in output
    Expected Result: 50%+ reduction in execution time
    Evidence: .sisyphus/evidence/task-2-nested-fix.txt

  Scenario: self-assess still works in production mode
    Tool: Bash (sbcl --eval)
    Preconditions: System loaded, *self-assess-running* is NIL (default)
    Steps:
      1. (ql:quickload :sibyl :silent t)
      2. (sibyl.config:load-config)
      3. (let ((result (sibyl.tools:execute-tool "self-assess" nil))) (format t "~a~%" (subseq result 0 (min 200 (length result)))))
      4. Assert: result contains "test_coverage" (proving run-tests was called)
    Expected Result: self-assess runs normally outside test context
    Evidence: .sisyphus/evidence/task-2-production-mode.txt
  ```

  **Commit**: YES
  - Message: `fix(tests): prevent self-assess from re-executing full suite during test runs`
  - Files: `src/tools/lisp-tools.lisp`, `tests/suite.lisp` or `tests/sexp-tools-test.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --eval '(uiop:quit)'`

---

- [x] 3. Measure Phase A' Impact

  **What to do**:
  - `(time (fiveam:run! 'sibyl.tests:sibyl-tests))`を実行
  - 結果を記録: wall time, total checks, pass, fail
  - Task 1のベースラインと比較
  - 結果を`.sisyphus/evidence/task-3-measurement.txt`に保存

  **Must NOT do**:
  - コードを変更しない（計測のみ）

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: 計測のみ
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 2 (after Task 2)
  - **Blocks**: Task 4
  - **Blocked By**: Task 2

  **References**: None needed (measurement only)

  **Acceptance Criteria**:

  ```
  Scenario: Measure post-A' performance
    Tool: Bash (sbcl --eval)
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(time (fiveam:run! (quote sibyl.tests:sibyl-tests)))' --eval '(uiop:quit)'
      2. Record wall time
      3. Assert: wall time < 60s (expect ~40s based on eliminating 6 nested runs)
      4. Assert: 0 failures
    Expected Result: Significant speedup documented
    Evidence: .sisyphus/evidence/task-3-measurement.txt
  ```

  **Commit**: NO (measurement only)

---

- [x] 4. Add codebase-map Cache for Tests (Phase B)

  **What to do**:
  - **RED**: テストを書く — `tests/sexp-tools-test.lisp`に追加:
    ```lisp
    (test codebase-map-cache-prevents-redundant-scans
      "with-codebase-map-cache should prevent redundant file system scans."
      (let ((call-count 0))
        (sibyl.tools::with-codebase-map-cache ()
          ;; 1回目: 実際にスキャン
          (sibyl.tools:execute-tool "codebase-map" '(("detail-level" . "summary")))
          ;; 2回目: キャッシュから返る
          (sibyl.tools:execute-tool "codebase-map" '(("detail-level" . "summary")))
          ;; call-countが1であることを確認する方法が必要
          )))
    ```
  - **GREEN**: `src/tools/lisp-tools.lisp`に実装:
    - `*codebase-map-cache*` 動的変数（デフォルト`nil`）
    - `with-codebase-map-cache` マクロ: 新しいハッシュテーブルを`*codebase-map-cache*`にlet束縛
    - `codebase-map`ツール内: `*codebase-map-cache*`が非nilならキャッシュを確認。キーは`detail-level`パラメータ。ヒットすればキャッシュを返す、ミスすれば実行してキャッシュに格納。
  - **REFACTOR**: テストランナー（`run-all-tests`）を`with-codebase-map-cache`で包む
  - `src/packages.lisp`に`with-codebase-map-cache`をexport

  **Must NOT do**:
  - `codebase-map`以外のツールをキャッシュしない
  - キャッシュにTTL、LRU、無効化ロジックを追加しない
  - プロダクション（テスト外）でキャッシュを有効にしない
  - グローバル変数としてキャッシュを永続化しない（letスコープのみ）

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: マクロ定義 + ツール内のキャッシュチェック追加。明確なパターン。
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 3 (with Tasks 5, 6)
  - **Blocks**: Task 7
  - **Blocked By**: Task 3

  **References**:

  **Pattern References**:
  - `src/tools/lisp-tools.lisp` — `deftool "codebase-map"` の定義（`detail-level`パラメータの受け取り方）
  - `src/config.lisp:96-100` — `with-config`マクロ（let束縛パターンの参考）

  **WHY Each Reference Matters**:
  - `codebase-map`ツールの引数処理を理解してキャッシュキーを正しく設定するため
  - `with-config`は`*config*`をlet束縛するマクロ。同じパターンで`with-codebase-map-cache`を実装する。

  **Acceptance Criteria**:

  **TDD:**
  - [ ] テスト: `codebase-map-cache-prevents-redundant-scans` → RED → GREEN → PASS
  - [ ] `(asdf:test-system :sibyl)` → 全テストパス

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Cache reduces codebase-map calls
    Tool: Bash (sbcl --eval)
    Steps:
      1. Load system
      2. Run with cache: (sibyl.tools::with-codebase-map-cache () (time (fiveam:run! 'sibyl.tests:sibyl-tests)))
      3. Assert: wall time < 30s (was ~40s after Phase A')
      4. Assert: 0 failures
    Expected Result: Further speedup from cached codebase-map
    Evidence: .sisyphus/evidence/task-4-cache.txt

  Scenario: Cache is scoped (not persistent)
    Tool: Bash (sbcl --eval)
    Steps:
      1. (sibyl.tools::with-codebase-map-cache () (sibyl.tools:execute-tool "codebase-map" '(("detail-level" . "summary"))))
      2. Assert: sibyl.tools::*codebase-map-cache* is NIL after macro exits
    Expected Result: Cache cleaned up after scope exit
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `feat(tools): add with-codebase-map-cache macro for test-time scan caching`
  - Files: `src/tools/lisp-tools.lisp`, `src/packages.lisp`, `tests/sexp-tools-test.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --eval '(uiop:quit)'`

---

- [x] 5. Add Thread-Safety Locks to Global State

  **What to do**:
  - 全グローバル可変状態に`bt:make-recursive-lock`を追加
  - 各変数のアクセス関数を`bt:with-recursive-lock-held`で保護
  - **ロック順序（デッドロック防止、文書化必須）**:
    1. `*tool-registry-lock*`
    2. `*config-lock*` (※ `with-config`がlet束縛するため通常不要)
    3. `*evolution-state-lock*`
    4. `*modified-files-lock*`
    5. `*command-handlers-lock*`
  - 対象変数と場所:
    - `*tool-registry*` in `src/tools/protocol.lisp` → `register-tool`, `find-tool`, `all-tools`をロック
    - `*modified-files*` in `src/system/asdf-protection.lisp` → `protect-file`, `unprotect-file`, `clear-all-protections`をロック
    - `*evolution-state*` in `src/tools/lisp-tools.lisp` → init, record-attempt, save, loadをロック
    - `*command-handlers*` in `src/repl.lisp` → push/assocアクセスをロック
    - `*pending-suggestions*` + `*next-suggestion-id*` in `src/repl.lisp` → store-suggestionをロック
  - ロックパターンは`src/llm/message.lisp:61,69,76,81,86`を参考にする

  **Must NOT do**:
  - ロックフリー構造、STM、アクターモデルを使わない
  - `bt:make-lock`ではなく`bt:make-recursive-lock`を使う（self-assess→run-testsチェーンで再入するため）
  - 既存のAPIシグネチャを変更しない

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: 各ファイルにロック定義 + with-lock-heldを追加する定型作業
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 3 (with Tasks 4, 6)
  - **Blocks**: Task 7
  - **Blocked By**: Task 2

  **References**:

  **Pattern References**:
  - `src/llm/message.lisp:61` — `(lock :initform (bt:make-lock "conversation-lock"))` — ロック定義パターン
  - `src/llm/message.lisp:69` — `(bt:with-lock-held ((slot-value conv 'lock)) ...)` — ロック使用パターン
  - `src/tools/protocol.lisp:25` — `*tool-registry*`定義
  - `src/system/asdf-protection.lisp:14` — `*modified-files*`定義
  - `src/repl.lisp` — `*command-handlers*`, `*pending-suggestions*`, `*next-suggestion-id*`定義

  **WHY Each Reference Matters**:
  - message.lispのロックパターンはSibylで唯一のスレッドセーフ実装。同じパターンを他の変数に適用する。
  - recursive-lockが必要な理由: self-assessがrun-testsを呼び、その中でtool-registryにアクセスする再帰パスがある。

  **Acceptance Criteria**:

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Locks don't break sequential execution
    Tool: Bash (sbcl --eval)
    Steps:
      1. Load system: (ql:quickload :sibyl/tests :silent t)
      2. Run: (asdf:test-system :sibyl)
      3. Assert: "Fail: 0"
      4. Assert: check count unchanged (1040 or adjusted after cleanup)
    Expected Result: All tests pass with locks added
    Evidence: .sisyphus/evidence/task-5-locks.txt

  Scenario: Lock variables exist
    Tool: Bash (sbcl --eval)
    Steps:
      1. Load system
      2. Assert: (boundp 'sibyl.tools::*tool-registry-lock*) → T
      3. Assert: (typep sibyl.tools::*tool-registry-lock* 'bt:lock) → T or similar
      4. Repeat for *modified-files-lock*, *evolution-state-lock*, *command-handlers-lock*
    Expected Result: All locks defined and bound
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `feat(core): add recursive locks to all global mutable state for thread safety`
  - Files: `src/tools/protocol.lisp`, `src/system/asdf-protection.lisp`, `src/tools/lisp-tools.lisp`, `src/repl.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --eval '(uiop:quit)'`

---

- [x] 6. Validate FiveAM Thread Safety

  **What to do**:
  - FiveAMの`run`関数がスレッドセーフかを検証
  - FiveAMソースコードを確認: `(run ...)` が内部状態を`let`で束縛しているか、グローバルに変更しているか
  - 経験的テスト: 2つの軽量スイートを同時に`bt:make-thread`で実行し、結果を比較
    ```lisp
    (let* ((results-a nil)
           (results-b nil)
           (thread-a (bt:make-thread
                       (lambda ()
                         (setf results-a (fiveam:run 'evolution-report-tests)))))
           (thread-b (bt:make-thread
                       (lambda ()
                         (setf results-b (fiveam:run 'evolution-state-tests))))))
      (bt:join-thread thread-a)
      (bt:join-thread thread-b)
      ;; 結果を検証
      )
    ```
  - 結果を`.sisyphus/evidence/task-6-fiveam-safety.txt`に記録
  - **もしUNSAFEなら**: 並列ランナーで`(fiveam:run ...)`を直接呼ばず、テスト関数を直接実行してチェック結果をスレッドローカルに収集する方式に変更

  **Must NOT do**:
  - FiveAMのソースコードを変更しない
  - この調査でコードベースを変更しない

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: FiveAM内部の調査 + 経験的テストの実行が必要。不確実性が高い。
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 3 (with Tasks 4, 5)
  - **Blocks**: Task 7
  - **Blocked By**: Task 2

  **References**:

  **External References**:
  - FiveAM source: `~/quicklisp/dists/quicklisp/software/fiveam-*/src/run.lisp` — `run`関数の実装
  - FiveAM docs: `https://fiveam.common-lisp.dev/docs/Running_0020Tests.html` — `*run-queue*`, `*debug-on-error*`等の特殊変数

  **WHY Each Reference Matters**:
  - `run`関数が`*run-queue*`等をlet束縛していればスレッドセーフ、グローバルに変更していればUNSAFE
  - 結果次第でTask 7の実装方式が変わる（FiveAM直接呼び出し vs ラッパー）

  **Acceptance Criteria**:

  ```
  Scenario: FiveAM concurrent run test
    Tool: Bash (sbcl --eval)
    Steps:
      1. Load sibyl/tests
      2. Run 2 lightweight suites concurrently via bt:make-thread
      3. Join both threads
      4. Assert: both result sets have correct pass/fail counts
      5. Assert: no errors, no corrupted results
    Expected Result: Either SAFE (proceed with fiveam:run in threads) or UNSAFE (need wrapper)
    Evidence: .sisyphus/evidence/task-6-fiveam-safety.txt
  ```

  **Commit**: NO (investigation only, results inform Task 7 design)

---

- [x] 7. Build Parallel Test Runner + /test-parallel Command (Phase C)

  **What to do**:
  - **RED**: `tests/parallel-runner-test.lisp`を新規作成（TDD）:
    - `run-tests-parallel`が全テストを実行して結果を返す
    - 結果が順次実行と一致する（同じcheck数、pass数）
    - 安全なスイートが並列で実行される
    - 危険なスイートが順次実行される
    - スレッドエラーが適切にハンドリングされる
  - **GREEN**: `tests/suite.lisp`に`run-tests-parallel`関数を実装:
    - スイート分類（ハードコードリスト）:
      - **SAFE（並列可能）**: `read-sexp-tests`, `describe-symbol-tests`, `macroexpand-form-tests`, `package-symbols-tests`, `codebase-map-tests`, `sync-to-file-tests`, `evolve-tests`, `agent-tests`, `tdd-orchestration-tests`, `run-hook-tests`, `evolution-state-tests`, `evolution-report-tests`
      - **UNSAFE（順次実行）**: `suggest-improvements-tests`, `self-assess-tests`, `improvement-plan-tests`, `suggest-improvements-enhanced-tests`, `safe-redefine-tests`, `write-test-tests`, `creation-integration-tests`, `asdf-registration-tests`, `tools-test`(top-level), `register-command-tests`, `asdf-protection-tests`, `eval-form-tests`, `who-calls-tests`
    - 実行フロー:
      1. `with-codebase-map-cache`で全体をラップ
      2. `*self-assess-running*`を`t`にlet束縛
      3. SAFEスイートを`bt:make-thread`で並列起動（各スレッドに`handler-case`）
      4. `bt:join-thread`で全スレッド待機（タイムアウト付き）
      5. UNSAFEスイートを順次実行
      6. 結果をマージして返す
    - エラーハンドリング:
      ```lisp
      (bt:make-thread
        (lambda ()
          (handler-case
              (setf (aref results i) (fiveam:run suite))
            (error (e)
              (setf (aref results i) (list (make-instance 'fiveam::unexpected-test-failure
                                             :test-case suite :reason e)))))))
      ```
    - 出力制御: 各スレッドの出力を`with-output-to-string`でキャプチャ
  - `/test-parallel` REPLコマンドを`src/repl.lisp`に追加（`register-command`パターン使用）
  - `sibyl.asd`に`tests/parallel-runner-test.lisp`を追加
  - `src/packages.lisp`に`run-tests-parallel`をexport

  **Must NOT do**:
  - スレッドプール、ワークスティーリング、スケジューラ抽象化
  - `/test-parallel`に引数やオプション追加
  - `bt:destroy-thread`でスレッド強制停止
  - スイート分類の自動検出（ハードコードリスト）
  - Task 6でUNSAFEと判定された場合のFiveAM直接呼び出し

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: マルチスレッド実装 + TDD + エラーハンドリング。最も複雑なタスク。
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 4 (sequential)
  - **Blocks**: Task 8
  - **Blocked By**: Tasks 4, 5, 6

  **References**:

  **Pattern References**:
  - `src/llm/message.lisp:61-86` — bordeaux-threadsのロック使用パターン
  - `src/repl.lisp` — `*command-handlers*`への登録パターン（`register-command`参考）
  - `tests/repl-test.lisp` — `register-command-tests`スイート（コマンド登録テストパターン）
  - `tests/suite.lisp` — 既存のテストスイート定義とrun!呼び出し

  **API/Type References**:
  - `fiveam:run` — スイートを実行して結果リストを返す（出力なし）
  - `fiveam:run!` — スイートを実行して結果を表示（出力あり）
  - `bt:make-thread` — スレッド作成
  - `bt:join-thread` — スレッド待機
  - `bt:make-recursive-lock` — 再帰ロック作成
  - `bt:with-recursive-lock-held` — ロック取得

  **WHY Each Reference Matters**:
  - message.lispはSibylで唯一のマルチスレッドパターン。同じスタイルで実装する。
  - register-commandパターンで`/test-parallel`を登録する。

  **Acceptance Criteria**:

  **TDD:**
  - [ ] `tests/parallel-runner-test.lisp` 作成
  - [ ] テスト: parallel-runner結果が順次実行と一致
  - [ ] テスト: エラーハンドリング（スレッド内例外でハングしない）
  - [ ] テスト: `/test-parallel`コマンドが登録されている
  - [ ] `(asdf:test-system :sibyl)` → 全テストパス

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Parallel runner produces correct results
    Tool: Bash (sbcl --eval)
    Preconditions: All previous tasks completed
    Steps:
      1. Load system
      2. Sequential run: (let ((seq (fiveam:run 'sibyl.tests:sibyl-tests))) (format t "SEQ: ~a checks~%" (length seq)))
      3. Parallel run: (let ((par (sibyl.tests:run-tests-parallel))) (format t "PAR: ~a total~%" (length par)))
      4. Assert: parallel check count = sequential check count
      5. Assert: parallel 0 failures
    Expected Result: Identical test results
    Evidence: .sisyphus/evidence/task-7-parallel-results.txt

  Scenario: Parallel runner is faster
    Tool: Bash (sbcl --eval)
    Steps:
      1. (time (fiveam:run! 'sibyl.tests:sibyl-tests)) → record sequential time
      2. (time (sibyl.tests:run-tests-parallel)) → record parallel time
      3. Assert: parallel time < sequential time
    Expected Result: Measurable speedup
    Evidence: .sisyphus/evidence/task-7-parallel-timing.txt

  Scenario: /test-parallel REPL command exists
    Tool: Bash (sbcl --eval)
    Steps:
      1. Load system
      2. (assoc "/test-parallel" sibyl.repl::*command-handlers* :test #'string=)
      3. Assert: result is non-nil
    Expected Result: Command registered
    Evidence: Terminal output
  ```

  **Commit**: YES
  - Message: `feat(tests): add parallel test runner with /test-parallel REPL command`
  - Files: `tests/parallel-runner-test.lisp`, `tests/suite.lisp`, `src/repl.lisp`, `src/packages.lisp`, `sibyl.asd`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --eval '(uiop:quit)'`

---

- [x] 8. Final Measurement + Regression Verification

  **What to do**:
  - 全フェーズ完了後の最終計測:
    1. `(time (fiveam:run! 'sibyl.tests:sibyl-tests))` — 順次実行タイム
    2. `(time (sibyl.tests:run-tests-parallel))` — 並列実行タイム
  - ベースライン（Task 1）との比較表を作成
  - 全テストがパスすることを確認（0 failures）
  - テスト数が変わっていないことを確認
  - 結果を`.sisyphus/evidence/task-8-final.txt`に保存

  **Must NOT do**:
  - コードを変更しない（計測のみ）

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: 計測と記録のみ
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 4 (after Task 7)
  - **Blocks**: None (final)
  - **Blocked By**: Task 7

  **References**: None needed

  **Acceptance Criteria**:

  ```
  Scenario: Final performance verification
    Tool: Bash (sbcl --eval)
    Steps:
      1. Sequential: (time (fiveam:run! 'sibyl.tests:sibyl-tests))
      2. Record: wall time, checks, pass, fail
      3. Parallel: (time (sibyl.tests:run-tests-parallel))
      4. Record: wall time, checks, pass, fail
      5. Assert: sequential wall time < 30s
      6. Assert: parallel wall time < 15s (target: 10s)
      7. Assert: both have 0 failures
      8. Assert: check counts match
    Expected Result: 
      - Sequential: ~20-30s (down from 120s+)
      - Parallel: ~10-15s (target achieved)
    Evidence: .sisyphus/evidence/task-8-final.txt
  ```

  **Commit**: YES (groups with Task 7 if applicable)
  - Message: `chore(sisyphus): record final parallel test performance metrics`
  - Files: `.sisyphus/evidence/task-8-final.txt`

---

## Commit Strategy

| After Task | Message | Files | Verification |
|------------|---------|-------|--------------|
| 1 | `fix(tests): remove 1380+ duplicate auto-generated test lines` | sexp-tools-test.lisp | asdf:test-system |
| 2 | `fix(tests): prevent self-assess suite re-execution during test runs` | lisp-tools.lisp, suite.lisp | asdf:test-system |
| 4 | `feat(tools): add with-codebase-map-cache for test-time scan caching` | lisp-tools.lisp, packages.lisp, sexp-tools-test.lisp | asdf:test-system |
| 5 | `feat(core): add recursive locks to global mutable state` | protocol.lisp, asdf-protection.lisp, lisp-tools.lisp, repl.lisp | asdf:test-system |
| 7 | `feat(tests): add parallel test runner with /test-parallel command` | parallel-runner-test.lisp, suite.lisp, repl.lisp, packages.lisp, sibyl.asd | asdf:test-system |
| 8 | `chore(sisyphus): record parallel test performance metrics` | .sisyphus/evidence/ | N/A |

---

## Success Criteria

### Verification Commands
```lisp
;; Sequential execution
(time (fiveam:run! 'sibyl.tests:sibyl-tests))
;; Expected: < 30 seconds, 0 failures

;; Parallel execution
(time (sibyl.tests:run-tests-parallel))
;; Expected: < 15 seconds, 0 failures, same check count as sequential

;; REPL command exists
(assoc "/test-parallel" sibyl.repl::*command-handlers* :test #'string=)
;; Expected: non-nil

;; File cleanup verified
;; wc -l tests/sexp-tools-test.lisp → < 3710
```

### Final Checklist
- [x] All "Must Have" present
- [x] All "Must NOT Have" absent
- [x] All existing tests pass (0 failures, 1094 checks)
- [x] run-sibyl-tests < 15 seconds (8.79s) ✓
- [x] run-tests-parallel < 15 seconds (12.92s) ✓
- [x] /test-parallel REPL command works
- [x] sexp-tools-test.lisp < 3710 lines
- [x] All global state has recursive locks
- [x] Lock ordering documented in source code comments
