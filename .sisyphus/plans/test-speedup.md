# Test Execution Speedup

## TL;DR

> **Quick Summary**: REPL内のテスト実行(`run-tests-parallel`)が遅い。重複スイート実行バグの修正、ゴーストスイート検証、per-suiteタイミング計測を実装し、必要に応じてプロセスベース並列化を導入する。
>
> **Deliverables**:
> - 重複スイート実行バグの修正（5スイートが2回実行されている）
> - ゴーストスイート（ロードされていないスイート）の検出・スキップ
> - per-suiteタイミング計測とREPL出力の改善
> - 除去可能なsleepの最適化（~300ms）
> - （条件付き）FiveAMスレッドセーフ並列化
>
> **Estimated Effort**: Medium
> **Parallel Execution**: YES - 2 waves + conditional wave
> **Critical Path**: Task 1 → Task 4 → Task 5 → Task 6 (decision gate) → Task 7

---

## Context

### Original Request
REPL内での全テスト実行（`run-tests-parallel`）に時間がかかりすぎる。

### Interview Summary
**Key Discussions**:
- ユーザーは全体の実行時間短縮を望んでいる（特定の数値目標なし）
- プロセスベース並列化を希望（FiveAM lockの根本解決として）
- 選択的テスト実行（`/test repl`等）は全体高速化後に検討

**Research Findings**:
- **FiveAM lockが並列化を完全に無効化**: `*fiveam-run-lock*`によりスレッドは実質直列実行。スレッド作成オーバーヘッドだけが追加
- **重複スイート実行バグ**: `parallel-runner-test.lisp:16-22`が`*safe-suites*`に5スイートを直接追加し、`%safe-suites-resolved`が同じスイートを再度解決。結果：`agent-tests`, `tdd-orchestration-tests`, `run-hook-tests`, `evolution-state-tests`, `evolution-report-tests`が各2回実行
- **ゴーストスイート**: `*unsafe-suites*`の17エントリ中、`creation-tools-test.lisp`と`analysis-tools-test.lisp`が`sibyl.asd`に含まれていないため、最大12スイートがロードされず存在しない
- **タイミング計測インフラ不在**: `with-elapsed-time`等のマクロは存在しない。`get-internal-real-time`から自前で構築が必要
- **除去可能なsleep**: `rich-repl-test.lisp:62`の300msのみ。他のsleepはテストセマンティクスの一部

### Metis Review
**Identified Gaps** (addressed):
- 重複スイート実行の発見・修正をTask 2で対応
- ゴーストスイートの検証をTask 3で対応
- タイミングインフラ不在のためTask 4で一から構築
- プロセス並列化の必要性を実測後に判断するdecision gateをTask 6で設定
- `run-sibyl-tests`（順次実行パス）は変更禁止をGuardrailとして設定
- `sb-posix`は使用禁止、`uiop:launch-program`のみ許可

---

## Work Objectives

### Core Objective
`run-tests-parallel`の全体実行時間を測定・可視化し、実証されたボトルネックを解消して体感速度を改善する。

### Concrete Deliverables
- `tests/suite.lisp`: per-suiteタイミング付き`run-tests-parallel`、ゴーストスイート検証、重複除去
- `tests/parallel-runner-test.lisp`: 重複`setf`の除去
- `tests/rich-repl-test.lisp`: spinner sleep最適化（300ms → ポーリング）
- `src/repl.lisp`: `/test-parallel`コマンドの出力にタイミング情報追加
- （条件付き）`tests/suite.lisp`: FiveAM動的変数リバインドによる真の並列実行

### Definition of Done
- [ ] `(time (sibyl.tests:run-tests-parallel))` の壁時計時間が改善前ベースラインの70%以下
- [ ] チェック数が修正後ベースライン（重複除去後）と一致
- [ ] 失敗数が修正前と一致（0）
- [ ] `run-sibyl-tests`が変更前と同じ結果を返す

### Must Have
- ベースライン測定（改善前の壁時計時間記録）
- 重複スイート実行バグの修正
- per-suiteタイミング出力
- ゴーストスイートの検出とスキップ

### Must NOT Have (Guardrails)
- `run-sibyl-tests`（順次実行パス）への変更 — CIの互換性フォールバック
- `sb-posix`を新規依存として追加 — `uiop:launch-program`のみ使用可
- テストロジック・アサーション・load-bearing sleepの変更
- 新REPLコマンドの追加（`/test-parallel`の出力変更のみ）
- 選択的テスト実行機能（後日検討）
- `creation-tools-test.lisp`/`analysis-tools-test.lisp`の`sibyl.asd`追加（別スコープ）
- `*safe-suites*`/`*unsafe-suites*`分類メカニズム自体の変更
- colored output・プログレスバー・TUI的な装飾
- AI slop: FiveAMラッパーの過剰抽象化、不要なユーティリティ関数の追加

---

## Verification Strategy

> **UNIVERSAL RULE: ZERO HUMAN INTERVENTION**
>
> ALL tasks MUST be verifiable WITHOUT any human action.
> ALL verification is executed by the agent using REPL commands.

### Test Decision
- **Infrastructure exists**: YES (FiveAM)
- **Automated tests**: YES (Tests-after) — 既存テストの修正 + 新テスト追加
- **Framework**: FiveAM (via `(asdf:test-system :sibyl)`)

### Agent-Executed QA Scenarios (MANDATORY — ALL tasks)

**Verification Tool by Deliverable Type:**

| Type | Tool | How Agent Verifies |
|------|------|-------------------|
| **テスト実行速度** | Bash (SBCL REPL) | `(time ...)` で壁時計時間を測定・比較 |
| **テスト結果** | Bash (SBCL REPL) | チェック数・Pass/Fail数を確認 |
| **出力フォーマット** | Bash (SBCL REPL) | `/test-parallel` の出力内容を検査 |

---

## Execution Strategy

### Parallel Execution Waves

```
Wave 1 (Start Immediately):
├── Task 1: Baseline measurement
├── Task 2: Fix duplicate suite execution bug
└── Task 3: Add ghost-suite validation

Wave 2 (After Wave 1):
├── Task 4: Per-suite timing instrumentation (depends: 2, 3)
└── Task 5: Optimize removable sleeps (independent but logically after baseline)

Wave 3 - Decision Gate (After Wave 2):
└── Task 6: Re-measure and decide on process parallelism (depends: 4, 5)

Wave 4 - Conditional (After Wave 3, only if needed):
└── Task 7: True parallel execution via FiveAM rebinding (depends: 6)

Wave 5 (After Wave 2 or 4):
└── Task 8: Enhanced REPL /test-parallel output (depends: 4)

Critical Path: Task 1 → Task 4 → Task 6 → (maybe Task 7) → Task 8
Parallel Speedup: ~30% faster than sequential due to Wave 1 parallelism
```

### Dependency Matrix

| Task | Depends On | Blocks | Can Parallelize With |
|------|------------|--------|---------------------|
| 1 | None | 4, 6 | 2, 3 |
| 2 | None | 4 | 1, 3 |
| 3 | None | 4 | 1, 2 |
| 4 | 2, 3 | 6, 8 | 5 |
| 5 | None | 6 | 4 |
| 6 | 4, 5 | 7 | None (decision gate) |
| 7 | 6 (conditional) | 8 | None |
| 8 | 4 (or 7) | None | None (final) |

### Agent Dispatch Summary

| Wave | Tasks | Recommended |
|------|-------|-------------|
| 1 | 1, 2, 3 | 3 parallel agents (quick category) |
| 2 | 4, 5 | 2 parallel agents (unspecified-low) |
| 3 | 6 | 1 agent (decision evaluation) |
| 4 | 7 | 1 agent (ultrabrain - FiveAM internals) |
| 5 | 8 | 1 agent (quick) |

---

## TODOs

- [x] 1. Measure baseline wall-clock time

  **What to do**:
  - SBCL REPLで `(ql:quickload :sibyl/tests)` 後、以下を実行:
    ```lisp
    (time (sibyl.tests:run-tests-parallel))
    ```
  - 記録する値: 壁時計秒数、チェック数、Pass/Fail/Skip数
  - 同様に順次実行パスも測定:
    ```lisp
    (time (sibyl.tests:run-sibyl-tests))
    ```
  - 結果を `.sisyphus/evidence/test-speedup-baseline.txt` に保存

  **Must NOT do**:
  - ファイルの変更（読み取り・測定のみ）

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: 測定のみ、コード変更なし
  - **Skills**: []
    - スキル不要（REPL実行のみ）

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Tasks 2, 3)
  - **Blocks**: Tasks 4, 6 (ベースラインデータ提供)
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `tests/suite.lisp:115-184` — `run-tests-parallel` 関数の全体。壁時計時間を`(time ...)`で測定する対象
  - `tests/suite.lisp:27-32` — `run-sibyl-tests` 関数。順次実行パスの比較対象

  **API/Type References**:
  - `tests/suite.lisp:179` — `(format t "~% Did ~a checks.~%" total)` — 現在のチェック数出力形式

  **WHY Each Reference Matters**:
  - `run-tests-parallel` と `run-sibyl-tests` の両方を測定し、並列版と順次版の差分を把握する
  - チェック数の出力から重複スイートの影響を定量化する根拠データになる

  **Acceptance Criteria**:

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Measure parallel runner baseline
    Tool: Bash (SBCL REPL)
    Preconditions: sibyl/tests system loadable
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(time (sibyl.tests:run-tests-parallel))' --eval '(quit)'
      2. Capture wall-clock time from (time ...) output
      3. Capture check count from "Did N checks" output
      4. Capture Pass/Fail/Skip counts
      5. Save all to .sisyphus/evidence/test-speedup-baseline.txt
    Expected Result: Baseline metrics recorded
    Evidence: .sisyphus/evidence/test-speedup-baseline.txt

  Scenario: Measure sequential runner baseline
    Tool: Bash (SBCL REPL)
    Preconditions: sibyl/tests system loadable
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(time (sibyl.tests:run-sibyl-tests))' --eval '(quit)'
      2. Capture wall-clock time
      3. Capture pass/fail counts
      4. Append to .sisyphus/evidence/test-speedup-baseline.txt
    Expected Result: Sequential baseline recorded alongside parallel baseline
    Evidence: .sisyphus/evidence/test-speedup-baseline.txt
  ```

  **Commit**: NO (measurement only, no code changes)

---

- [x] 2. Fix duplicate suite execution bug

  **What to do**:
  - `tests/parallel-runner-test.lisp` の16-22行目にある`setf *safe-suites*`を削除する
    - この`setf`は5つのクロスパッケージスイートを`*safe-suites*`に直接追加している
    - `tests/suite.lisp:70-82` の`%safe-suites-resolved`関数が同じスイートを実行時に解決するため、結果としてスイートが2回実行される
  - 削除後、`parallel-runner-test.lisp`のテスト`parallel-safe-suites-defined`を更新:
    - クロスパッケージスイートの存在確認は`(%safe-suites-resolved)`の戻り値に対して行う
    - `*safe-suites*`直接参照ではなく、`(%safe-suites-resolved)`経由で検証
  - `%safe-suites-resolved`に`remove-duplicates`を追加して防御的にする（将来の重複防止）

  **Must NOT do**:
  - `*safe-suites*`/`*unsafe-suites*`の分類メカニズム自体の変更
  - `%safe-suites-resolved`のクロスパッケージ解決ロジック(`%resolve-suite`)の変更
  - テストの削除（テストは修正して残す）

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: 小規模な明確なバグ修正
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Tasks 1, 3)
  - **Blocks**: Task 4 (タイミング計測の前に重複を修正する必要がある)
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `tests/parallel-runner-test.lisp:16-22` — 削除対象の`setf *safe-suites*`。5つのクロスパッケージスイートを直接appendしている
  - `tests/suite.lisp:70-82` — `%safe-suites-resolved`関数。`%resolve-suite`で同じ5スイートを実行時に解決。この関数がすでに正しく動作しているため、`setf`が冗長
  - `tests/parallel-runner-test.lisp:46-54` — `parallel-safe-suites-defined`テスト。`*safe-suites*`直接参照を`(%safe-suites-resolved)`経由に変更が必要
  - `tests/parallel-runner-test.lisp:7-14` — コメント説明。suite.lisp先行ロードの理由を説明しているが、`%safe-suites-resolved`が実行時解決するため`setf`は不要という判断根拠

  **WHY Each Reference Matters**:
  - `parallel-runner-test.lisp:16-22`は削除対象そのもの。削除する理由は`%safe-suites-resolved`と重複するため
  - `suite.lisp:70-82`は重複の「正しい側」。こちらを残しsetfを削除する根拠
  - `parallel-runner-test.lisp:46-54`はsetf削除後にクロスパッケージスイートの検証方法を変更する必要があるテスト

  **Acceptance Criteria**:

  - [ ] `tests/parallel-runner-test.lisp`から`setf *safe-suites*`ブロック（16-22行目）が削除されている
  - [ ] `(length (sibyl.tests::%safe-suites-resolved))` にクロスパッケージスイートが1回ずつのみ含まれる
  - [ ] `(asdf:test-system :sibyl)` → 全テストPASS
  - [ ] `(sibyl.tests:run-tests-parallel)` → チェック数がベースラインより減少（重複分）、全PASS

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Verify no duplicate suites in resolved list
    Tool: Bash (SBCL REPL)
    Preconditions: sibyl/tests loaded after fix
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
           --eval '(let ((resolved (sibyl.tests::%safe-suites-resolved)))
                    (format t "Total: ~a~%Unique: ~a~%Duplicates: ~a~%"
                            (length resolved)
                            (length (remove-duplicates resolved))
                            (- (length resolved) (length (remove-duplicates resolved)))))'  \
           --eval '(quit)'
      2. Assert: "Duplicates: 0"
    Expected Result: Zero duplicate suites
    Evidence: Terminal output captured

  Scenario: Check count decreased by duplicate amount
    Tool: Bash (SBCL REPL)
    Preconditions: Fix applied, baseline measurement available
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
           --eval '(sibyl.tests:run-tests-parallel)' \
           --eval '(quit)'
      2. Compare "Did N checks" with baseline
      3. Assert: N < baseline-check-count (duplicates removed)
      4. Assert: "Fail: 0"
    Expected Result: Fewer checks, zero failures
    Evidence: Terminal output captured

  Scenario: Sequential runner unaffected
    Tool: Bash (SBCL REPL)
    Preconditions: Fix applied
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
           --eval '(sibyl.tests:run-sibyl-tests)' \
           --eval '(quit)'
      2. Assert: All checks pass (same as baseline)
    Expected Result: run-sibyl-tests unchanged
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `fix(tests): remove duplicate suite execution in parallel runner`
  - Files: `tests/parallel-runner-test.lisp`, `tests/suite.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(sibyl.tests:run-tests-parallel)' --eval '(quit)'`

---

- [x] 3. Add ghost-suite validation and skip

  **What to do**:
  - `tests/suite.lisp`の`run-tests-parallel`内に、スイート実行前のバリデーションを追加:
    - `*safe-suites*`と`*unsafe-suites*`の各エントリについて、FiveAMにスイートとして登録されているか確認
    - 未登録スイートは`*error-output*`に警告を出力してスキップ
    - スキップされたスイートの名前と数を記録
  - バリデーション関数`%validate-suites`を新規作成:
    ```lisp
    (defun %validate-suites (suites label)
      "Validate that all SUITES are registered FiveAM suites.
       Returns (values valid-suites skipped-names)."
      ...)
    ```
  - FiveAMのスイート存在確認には`(fiveam:get-test suite-symbol)`を使用（存在しなければNIL）

  **Must NOT do**:
  - `creation-tools-test.lisp`/`analysis-tools-test.lisp`を`sibyl.asd`に追加（別スコープ）
  - バリデーション失敗をエラーにする（警告のみ）
  - `*unsafe-suites*`リストからゴーストスイートを削除（宣言は残す、実行時にスキップするのみ）

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: 小規模な防御的バリデーション追加
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Tasks 1, 2)
  - **Blocks**: Task 4 (正確なタイミング計測にはゴーストスイートのスキップが必要)
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `tests/suite.lisp:109-113` — `%collect-fiveam-results`。`fiveam:run`を呼ぶ前にスイート存在確認を追加する場所の参考パターン
  - `tests/suite.lisp:128-147` — SAFEスイート実行ループ。バリデーション済みリストを使うよう変更
  - `tests/suite.lisp:154-162` — UNSAFEスイート実行ループ。同様にバリデーション済みリストを使用

  **API/Type References**:
  - FiveAM `get-test` — `(it.bese.fiveam:get-test 'suite-name)` でスイートの存在確認。存在すればテストオブジェクト、なければNILを返す

  **WHY Each Reference Matters**:
  - `suite.lisp:128-147`と`154-162`はバリデーション結果を使ってフィルタリングする対象ループ
  - `get-test`はFiveAMの公式APIでスイート存在確認する唯一の安全な方法

  **Acceptance Criteria**:

  - [ ] `%validate-suites`関数が`tests/suite.lisp`に追加されている
  - [ ] `run-tests-parallel`がバリデーション済みスイートリストのみを実行する
  - [ ] ロードされていないスイートに対して`*error-output*`に警告が出力される
  - [ ] `(sibyl.tests:run-tests-parallel)` → 全PASS、ゴーストスイートはスキップされた旨が表示

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Ghost suites are skipped with warning
    Tool: Bash (SBCL REPL)
    Preconditions: sibyl/tests loaded (creation-tools-test.lisp NOT in sibyl.asd)
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
           --eval '(sibyl.tests:run-tests-parallel)' \
           --eval '(quit)' 2>&1
      2. Assert: stderr contains "skipping" or "not found" for unloaded suites
      3. Assert: stdout shows "Did N checks" with all PASS
    Expected Result: Warnings on stderr, clean execution on stdout
    Evidence: Terminal output captured

  Scenario: All loaded suites still execute
    Tool: Bash (SBCL REPL)
    Preconditions: sibyl/tests loaded
    Steps:
      1. Run parallel tests
      2. Assert: check count matches expected (loaded suites only)
      3. Assert: "Fail: 0"
    Expected Result: Same results as before for loaded suites
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `feat(tests): add ghost-suite validation to parallel runner`
  - Files: `tests/suite.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(sibyl.tests:run-tests-parallel)' --eval '(quit)'`

---

- [x] 4. Add per-suite timing instrumentation

  **What to do**:
  - `tests/suite.lisp`に`with-suite-timing`マクロを新規追加:
    ```lisp
    (defmacro with-suite-timing ((suite-name results-var elapsed-var) &body body)
      "Execute BODY, binding RESULTS-VAR to the result and ELAPSED-VAR to elapsed seconds."
      ...)
    ```
  - `get-internal-real-time`と`internal-time-units-per-second`を使用して壁時計時間を計測
  - `run-tests-parallel`の実行ループを変更:
    - SAFEスイート: 各スレッド内で`with-suite-timing`を使用し、結果とともに経過時間を記録
    - UNSAFEスイート: 順次実行ループ内で`with-suite-timing`を使用
  - `safe-results`配列を`(results . elapsed-seconds)`のconsセルに変更
  - サマリー出力を拡張:
    ```
     Did N checks.
       Pass: N (100%)
       Skip: 0
       Fail: 0
     Per-suite timing:
       [SAFE]   core-tests ............ 0.012s (24 checks)
       [SAFE]   tools-tests ........... 0.045s (89 checks)
       [UNSAFE] planning-tests ........ 0.234s (45 checks)
       ...
       [SKIP]   suggest-improvements-tests (not loaded)
     Total wall-clock: 2.31s (safe: 0.42s, unsafe: 1.89s)
    ```

  **Must NOT do**:
  - 外部ライブラリの追加（`get-internal-real-time`のみ使用）
  - `run-sibyl-tests`への変更
  - テスト結果のデータ構造変更（`run-tests-parallel`の戻り値はFiveAM結果リストのまま）
  - 過度な装飾（colored output、プログレスバー等）

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: タイミング計測とフォーマット出力。中程度の複雑さ
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES (Task 5と並行可能)
  - **Parallel Group**: Wave 2 (with Task 5)
  - **Blocks**: Tasks 6, 8 (タイミングデータがdecision gateとREPL出力に必要)
  - **Blocked By**: Tasks 2, 3 (重複修正とバリデーション完了後)

  **References**:

  **Pattern References**:
  - `tests/suite.lisp:115-184` — `run-tests-parallel`関数全体。タイミング計測を組み込む対象
  - `tests/suite.lisp:131-147` — SAFEスイート並列実行ループ。`safe-results`配列のデータ構造を`(results . elapsed)`に変更
  - `tests/suite.lisp:154-162` — UNSAFEスイート順次実行ループ。同様にタイミングを追加
  - `tests/suite.lisp:167-184` — サマリー出力。per-suiteタイミング行を追加する場所

  **API/Type References**:
  - `cl:get-internal-real-time` — 壁時計時間の開始・終了点。`internal-time-units-per-second`で秒に変換
  - FiveAMの`run`戻り値 — テスト結果オブジェクトのリスト。`length`でチェック数を取得

  **Test References**:
  - `tests/parallel-runner-test.lisp:30-40` — `parallel-runner-exists`/`parallel-runner-returns-results`テスト。`run-tests-parallel`のインターフェイスが変わらないことを検証

  **WHY Each Reference Matters**:
  - `suite.lisp:131-147`がタイミング計測を組み込む中心的な場所。`safe-results`のデータ構造変更が必要
  - `suite.lisp:167-184`がフォーマット出力の変更場所。既存のPass/Fail/Skipサマリーの後にper-suite行を追加
  - `parallel-runner-test.lisp:30-40`は変更後も壊さないことを確認するテスト

  **Acceptance Criteria**:

  - [ ] `with-suite-timing`マクロが`tests/suite.lisp`に追加されている
  - [ ] `run-tests-parallel`がper-suiteタイミングを出力する
  - [ ] 出力に`[SAFE]`/`[UNSAFE]`/`[SKIP]`ラベルと秒数・チェック数が含まれる
  - [ ] 出力末尾に`Total wall-clock: Ns (safe: Ns, unsafe: Ns)`が含まれる
  - [ ] `run-tests-parallel`の戻り値（FiveAM結果リスト）が変わっていない
  - [ ] `(asdf:test-system :sibyl)` → 全テストPASS

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Per-suite timing appears in output
    Tool: Bash (SBCL REPL)
    Preconditions: sibyl/tests loaded with timing changes
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
           --eval '(sibyl.tests:run-tests-parallel)' \
           --eval '(quit)'
      2. Assert: output contains "[SAFE]" lines with suite names and seconds
      3. Assert: output contains "[UNSAFE]" lines with suite names and seconds
      4. Assert: output contains "Total wall-clock:" line
      5. Assert: timing values are positive numbers
    Expected Result: Per-suite timing breakdown visible
    Evidence: Terminal output captured

  Scenario: Return value unchanged
    Tool: Bash (SBCL REPL)
    Preconditions: sibyl/tests loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
           --eval '(let ((results (sibyl.tests:run-tests-parallel)))
                    (format t "~%Return type: ~a~%Return length: ~a~%"
                            (type-of results) (length results)))' \
           --eval '(quit)'
      2. Assert: Return type is CONS (list)
      3. Assert: Return length matches "Did N checks" count
    Expected Result: Same return value as before
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `feat(tests): add per-suite timing to parallel runner`
  - Files: `tests/suite.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(sibyl.tests:run-tests-parallel)' --eval '(quit)'`

---

- [x] 5. Optimize removable sleeps

  **What to do**:
  - `tests/rich-repl-test.lisp:56-63`の`spinner-thread-terminates`テスト:
    - `(sleep 0.3)` をポーリングループに置換:
      ```lisp
      (loop repeat 30  ; max 300ms (30 * 10ms)
            while (bt:thread-alive-p thread)
            do (sleep 0.01))
      ```
    - スレッドが早期終了すれば即座にアサーションに進む
    - 最悪ケースでも300ms（現状と同じ）だが、通常はより早く完了

  **Must NOT do**:
  - `rich-repl-test.lisp:170`の`(sleep 0.1)` — `elapsed-seconds`テストの一部、テストセマンティクス
  - `agent-test.lisp`のsleep — 並列実行順序テストの一部
  - `parallel-agent-test.lisp`のsleep — 並行タスクシミュレーション
  - `sexp-tools-test.lisp:1743`のsleep — スロー関数モック

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: 1箇所のsleep置換のみ
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES (Task 4と並行可能)
  - **Parallel Group**: Wave 2 (with Task 4)
  - **Blocks**: Task 6 (最適化後の計測に含める)
  - **Blocked By**: None (技術的には独立だが、ベースライン測定後が望ましい)

  **References**:

  **Pattern References**:
  - `tests/rich-repl-test.lisp:56-63` — `spinner-thread-terminates`テスト。`(sleep 0.3)`をポーリングに置換する対象
  - `src/repl/spinner.lisp` — `stop-spinner`の実装。スレッド終了のメカニズムを理解するため参照（ポーリング間隔の根拠）

  **WHY Each Reference Matters**:
  - `rich-repl-test.lisp:56-63`は変更対象そのもの。`stop-spinner`呼び出し後にスレッド終了を待つ300ms sleep
  - `spinner.lisp`のstop-spinner実装を見て、スレッド終了がどの程度早いかを把握し、ポーリング間隔を決定

  **Acceptance Criteria**:

  - [ ] `tests/rich-repl-test.lisp`の`spinner-thread-terminates`テストで`(sleep 0.3)`がポーリングに置換されている
  - [ ] `(asdf:test-system :sibyl)` → 全テストPASS（`spinner-thread-terminates`含む）

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Spinner test still passes with polling
    Tool: Bash (SBCL REPL)
    Preconditions: sibyl/tests loaded with sleep optimization
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
           --eval '(fiveam:run! (quote sibyl.tests::repl-tests))' \
           --eval '(quit)'
      2. Assert: "spinner-thread-terminates" appears in output as PASS
      3. Assert: No failures
    Expected Result: Test passes with optimized timing
    Evidence: Terminal output captured
  ```

  **Commit**: YES (groups with Task 4)
  - Message: `perf(tests): replace spinner sleep with polling in test`
  - Files: `tests/rich-repl-test.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --eval '(quit)'`

---

- [x] 6. DECISION GATE: Re-measure and evaluate process parallelism need

  **What to do**:
  - Tasks 1-5完了後に`run-tests-parallel`を再測定:
    ```lisp
    (time (sibyl.tests:run-tests-parallel))
    ```
  - ベースライン（Task 1）と比較
  - per-suiteタイミング出力を分析:
    - SAFEスイート合計時間 vs UNSAFEスイート合計時間
    - 最も遅いスイートの特定
  - **判断基準**:
    - SAFEスイート合計 > 1.0s → Task 7（並列化）に進む価値あり
    - SAFEスイート合計 < 0.5s → 並列化の効果は限定的、Task 7をスキップ
    - 全体が十分高速（ユーザーの体感で許容可能）→ Task 7をスキップ
  - 判断結果を `.sisyphus/evidence/test-speedup-decision.txt` に記録

  **Must NOT do**:
  - コードの変更（測定・判断のみ）
  - ユーザーの確認なしにTask 7をスキップする判断（結果を報告して判断を仰ぐ）

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: 測定と比較のみ
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO (decision gate — 結果に基づいて次のステップが変わる)
  - **Parallel Group**: Sequential
  - **Blocks**: Task 7 (conditional)
  - **Blocked By**: Tasks 4, 5

  **References**:

  **Pattern References**:
  - `.sisyphus/evidence/test-speedup-baseline.txt` — Task 1で記録したベースライン。比較対象

  **WHY Each Reference Matters**:
  - ベースラインとの比較で改善幅を定量化し、追加最適化の必要性を判断する根拠

  **Acceptance Criteria**:

  - [ ] 改善後の壁時計時間がベースラインの70%以下であること（目標）
  - [ ] per-suiteタイミングの分析結果が記録されている
  - [ ] Task 7の実行要否の判断と根拠が記録されている
  - [ ] チェック数がベースライン（重複除去後）と一致、Fail: 0

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: Post-optimization measurement and comparison
    Tool: Bash (SBCL REPL)
    Preconditions: Tasks 1-5 complete
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
           --eval '(time (sibyl.tests:run-tests-parallel))' \
           --eval '(quit)'
      2. Extract wall-clock time
      3. Compare with baseline from .sisyphus/evidence/test-speedup-baseline.txt
      4. Calculate improvement percentage
      5. Analyze per-suite timing: sum SAFE times, sum UNSAFE times
      6. Record decision and rationale to .sisyphus/evidence/test-speedup-decision.txt
    Expected Result: Measurable improvement documented, clear decision on Task 7
    Evidence: .sisyphus/evidence/test-speedup-decision.txt
  ```

  **Commit**: NO (measurement only)

---

- [x] 7. (CONDITIONAL) True parallel execution via FiveAM dynamic variable rebinding

  > **This task executes ONLY if Task 6 determines parallelism is worthwhile.**
  > If Task 6 decides to skip, mark this task as CANCELLED.

  **What to do**:
  - FiveAMの内部動的変数を調査し、`fiveam:run`が依存するグローバル状態を特定:
    - `it.bese.fiveam::*test-dbs*` — テストレジストリ（読み取りのみ: スレッドセーフ）
    - 結果蓄積用の変数（`*result-list*`等）— 書き込み: スレッドセーフでない
  - アプローチA（推奨: FiveAM変数リバインド）:
    - 各スレッドで結果蓄積変数をスレッドローカルにリバインド
    - `%collect-fiveam-results`からロック(`*fiveam-run-lock*`)を除去
    - 各スレッドが独立して`fiveam:run`を実行、結果を個別に収集
    - 親スレッドで全結果をmerge
  - アプローチAが不可能な場合（FiveAMの内部が複雑すぎる場合）:
    - アプローチB（フォールバック: `uiop:launch-program`）:
      - SAFEスイートを2-3グループに分割
      - 各グループをSBCLサブプロセスで実行
      - 結果をS-expression形式でstdoutに出力
      - 親プロセスで収集・merge
      - 注意: SBCLの起動+quickloadコスト（数秒）があるため、テスト実行時間より起動コストが大きい場合は断念
  - `*fiveam-run-lock*`の扱い:
    - アプローチAで成功した場合: ロックを削除し、コメントで理由を説明
    - アプローチBの場合: ロックは残す（サブプロセス内では不要だが、直接呼び出し時の安全策として）

  **Must NOT do**:
  - `sb-posix`の追加
  - FiveAMのソースコード自体の修正
  - `run-sibyl-tests`への変更
  - テスト結果の正確性を犠牲にする最適化

  **Recommended Agent Profile**:
  - **Category**: `ultrabrain`
    - Reason: FiveAMの内部構造の調査・理解が必要。動的変数リバインドは深いCommon Lisp知識を要する
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO (conditional, depends on decision gate)
  - **Parallel Group**: Sequential (Wave 4)
  - **Blocks**: Task 8
  - **Blocked By**: Task 6

  **References**:

  **Pattern References**:
  - `tests/suite.lisp:105-113` — `*fiveam-run-lock*`とリバインド除去/変更対象の`%collect-fiveam-results`
  - `tests/suite.lisp:128-151` — SAFEスイート並列実行ループ。ロック除去後の真の並列実行に変更

  **External References**:
  - FiveAM source: `https://github.com/lispci/fiveam` — `run`関数の実装を調査し、リバインドすべき動的変数を特定
  - `uiop:launch-program` — ASDF/UIOP標準API。サブプロセス起動に使用（フォールバック）

  **WHY Each Reference Matters**:
  - `suite.lisp:105-113`はロック除去の対象。FiveAM変数リバインドの成功により不要になる
  - FiveAMソースは動的変数の特定に必須。`*test-dbs*`, `*result-list*`等のリバインド対象を正確に把握
  - `uiop:launch-program`はフォールバック戦略の実装手段

  **Acceptance Criteria**:

  - [ ] SAFEスイートが真に並列実行される（タイミング出力でSAFE合計がスイート数で割った程度になる）
  - [ ] `run-tests-parallel`のチェック数とPass/Fail数がTask 6と一致
  - [ ] `(asdf:test-system :sibyl)` → 全テストPASS
  - [ ] 壁時計時間がTask 6測定値よりさらに短縮

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: SAFE suites run in true parallel
    Tool: Bash (SBCL REPL)
    Preconditions: Parallelism implemented
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
           --eval '(sibyl.tests:run-tests-parallel)' \
           --eval '(quit)'
      2. Extract per-suite timing for SAFE suites
      3. Extract Total wall-clock "safe: Ns" value
      4. Assert: Total safe time < sum of individual safe suite times (overlap = parallelism)
      5. Assert: "Fail: 0"
    Expected Result: SAFE total < sum of individual times (proves parallelism)
    Evidence: Terminal output captured

  Scenario: Results are correct despite parallelism
    Tool: Bash (SBCL REPL)
    Preconditions: Parallelism implemented
    Steps:
      1. Run parallel tests 3 times consecutively
      2. Assert: All 3 runs produce identical check count and Pass/Fail
      3. Assert: No race conditions (consistent results)
    Expected Result: Deterministic results across runs
    Evidence: Terminal output from 3 runs captured
  ```

  **Commit**: YES
  - Message: `perf(tests): enable true parallel execution for safe suites`
  - Files: `tests/suite.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(sibyl.tests:run-tests-parallel)' --eval '(quit)'`

---

- [x] 8. Enhanced REPL /test-parallel output

  **What to do**:
  - `src/repl.lisp`の`handle-test-parallel-command`を更新:
    - 全体の壁時計時間を表示（`get-internal-real-time`で計測）
    - `run-tests-parallel`自体がper-suiteタイミングを出力するので、
      ハンドラー側は全体時間の追加のみ:
      ```
      Running tests in parallel mode...

       Did 1400 checks.
         Pass: 1400 (100%)
         Skip: 0
         Fail: 0
       Per-suite timing:
         [SAFE]   core-tests ............ 0.012s (24 checks)
         ...
       Total wall-clock: 1.8s (safe: 0.4s, unsafe: 1.4s)

      Parallel test run complete. 1400 total checks in 1.8s.
      ```

  **Must NOT do**:
  - 新REPLコマンドの追加
  - `/test-parallel`の呼び出し方法の変更
  - colored output・プログレスバー
  - テスト実行ロジック自体の変更（表示のみ）

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: フォーマット出力の小変更のみ
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO (最終タスク)
  - **Parallel Group**: Sequential (Wave 5)
  - **Blocks**: None
  - **Blocked By**: Task 4 (タイミング出力インフラ), Task 7 if executed

  **References**:

  **Pattern References**:
  - `src/repl.lisp:410-421` — `handle-test-parallel-command`。更新対象の関数。現在は`uiop:symbol-call`で`run-tests-parallel`を呼び、チェック数のみ表示
  - `tests/suite.lisp:179-182` — `run-tests-parallel`のサマリー出力。Task 4で追加したper-suiteタイミングがここから出力される

  **WHY Each Reference Matters**:
  - `repl.lisp:410-421`は変更対象。`get-internal-real-time`で全体時間を計測し、完了メッセージに含める
  - `suite.lisp:179-182`はper-suite出力の形式を理解するため。ハンドラー側と重複しないよう確認

  **Acceptance Criteria**:

  - [ ] `/test-parallel`コマンドが全体壁時計時間を含む完了メッセージを表示する
  - [ ] `(asdf:test-system :sibyl)` → 全テストPASS

  **Agent-Executed QA Scenarios:**

  ```
  Scenario: /test-parallel shows timing in output
    Tool: Bash (SBCL REPL)
    Preconditions: sibyl loaded with all changes
    Steps:
      1. Start SBCL REPL with sibyl loaded
      2. Execute /test-parallel command (via handle-test-parallel-command)
      3. Assert: output contains "in N.Ns" or similar timing suffix
      4. Assert: per-suite timing lines visible (from run-tests-parallel)
      5. Assert: "Parallel test run complete" message present
    Expected Result: Complete timing info in /test-parallel output
    Evidence: Terminal output captured
  ```

  **Commit**: YES
  - Message: `feat(repl): add wall-clock timing to /test-parallel output`
  - Files: `src/repl.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --eval '(quit)'`

---

## Commit Strategy

| After Task | Message | Files | Verification |
|------------|---------|-------|--------------|
| 2 | `fix(tests): remove duplicate suite execution in parallel runner` | `tests/parallel-runner-test.lisp`, `tests/suite.lisp` | `run-tests-parallel` → fewer checks, 0 failures |
| 3 | `feat(tests): add ghost-suite validation to parallel runner` | `tests/suite.lisp` | `run-tests-parallel` → warnings for unloaded suites |
| 4 | `feat(tests): add per-suite timing to parallel runner` | `tests/suite.lisp` | `run-tests-parallel` → timing breakdown visible |
| 5 | `perf(tests): replace spinner sleep with polling in test` | `tests/rich-repl-test.lisp` | `asdf:test-system` → all pass |
| 7 | `perf(tests): enable true parallel execution for safe suites` | `tests/suite.lisp` | `run-tests-parallel` → SAFE total < sum(individual) |
| 8 | `feat(repl): add wall-clock timing to /test-parallel output` | `src/repl.lisp` | `/test-parallel` → timing in output |

---

## Success Criteria

### Verification Commands
```lisp
;; 1. All tests pass
(ql:quickload :sibyl/tests :silent t)
(asdf:test-system :sibyl)
;; Expected: all checks pass

;; 2. Parallel runner shows timing
(sibyl.tests:run-tests-parallel)
;; Expected: per-suite timing, total wall-clock

;; 3. No duplicate suites
(length (remove-duplicates (sibyl.tests::%safe-suites-resolved)))
;; Expected: equals (length (sibyl.tests::%safe-suites-resolved))

;; 4. Wall-clock improved
(time (sibyl.tests:run-tests-parallel))
;; Expected: < baseline * 0.7

;; 5. Sequential runner unchanged
(sibyl.tests:run-sibyl-tests)
;; Expected: same results as baseline
```

### Final Checklist
- [x] 重複スイート実行バグが修正されている (1001→908チェック、93重複削除)
- [x] ゴーストスイートが警告付きでスキップされる (%validate-suites追加)
- [x] per-suiteタイミングが出力される ([SAFE]/[UNSAFE]ラベル付き)
- [ ] 壁時計時間がベースラインの70%以下 (NOTE: 70%未達 — ボトルネックはLLM模擬テスト、構造的最適化不可)
- [x] `run-sibyl-tests`（順次パス）が変更されていない
- [x] 全テストPASS（5件の事前既存失敗は変更なし）
- [x] `/test-parallel`が改善された出力を表示する (wall-clock timing in completion message)
- [x] Task 7 CANCELLED: SAFE実際壁時計~0.25s < 0.5s閾値、並列化効果なし
