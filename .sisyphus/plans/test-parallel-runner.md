# True Parallel Test Runner & Degradation Prevention

## TL;DR

> **Quick Summary**: FiveAM の `run-tests-parallel` は `*fiveam-run-lock*` により実質シリアル実行になっている。FiveAM の内部グローバル変数をスレッドごとに rebind するカスタムランナーに置き換え、Safe スイートの真の並列実行を実現する。併せて、スイート分類の強制・実行時間の回帰検出・テスト規約のガイドラインを導入し、今後の速度劣化を防止する。
>
> **Deliverables**:
> - `tests/suite.lisp` の並列ランナー全面改修
> - 未分類10スイートの分類追加
> - スイート分類バリデーション機構
> - 実行時間の回帰検出機構（JSON 履歴）
> - `AGENTS.md` テスト規約セクション更新
>
> **Estimated Effort**: Medium
> **Parallel Execution**: YES - 2 waves
> **Critical Path**: Task 1 → Task 2 → Task 3 → Task 5 → Task 6

---

## Context

### Original Request
テスト速度の改善と、今後追加されるテストが速度性能劣化をおこさないための設計。

### Interview Summary
**Key Discussions**:
- ユーザーは「並列実行の本質的な改善」を最優先に選択
- 劣化防止は A（分類強制）+ C（回帰検出）+ D（ガイドライン）。B（スイート時間予算）は除外

**Research Findings**:
- FiveAM ソースの直接解析により、スレッド非安全の原因を6変数に特定
- `result-list`/`current-test` は `bind-run-state` でスレッドセーフ（FiveAM の設計上）
- `%run` 直接利用時は rebind 対象がたった3変数に絞れる
- 既存 `*safe-suites*` / `*unsafe-suites*` 分類がそのまま並列/直列のマッピングに使える
- **10個のスイートが分類漏れ**で無言スキップされている
- cross-suite `depends-on` はゼロ（全20ファイルで確認済み）

### Metis Review
**Identified Gaps** (addressed):
- **`(status test)` リセット漏れ**: `%run` はステータスリセットしない → REPL 再実行で全テスト無視の致命的バグ → リセットステップを追加
- **rebind 対象の精緻化**: `*run-queue*` は FiveAM 内で未使用（vestigial）、`*!*`/`*!!*`/`*!!!*` は `%run` 経由なら不変 → rebind 対象を3変数に削減
- **`*print-names*` の並列出力**: 複数スレッドが同時出力すると garbled → `nil` にバインドして抑制
- **SBCL パッケージロック**: `IT.BESE.FIVEAM` に `:lock t` → `find-symbol` パターンまたは `without-package-locks` が必要
- **`with-codebase-map-cache` のスレッド安全性**: ハッシュテーブルが `:synchronized nil` → safe スイートが codebase-map を呼ぶか確認必要
- **10個の未分類スイート**: 分類して追加する必要あり

---

## Work Objectives

### Core Objective
FiveAM の Safe スイートを真に並列実行できるカスタムランナーを構築し、テスト速度を改善する。
同時に、分類強制・回帰検出・ガイドラインにより今後の速度劣化を防止する。

### Concrete Deliverables
- `tests/suite.lisp`: `%run-suite-isolated`, 改修された `run-tests-parallel`, `%validate-suite-classification`, `%record-timing-history`, `%detect-regression`
- `*safe-suites*` / `*unsafe-suites*`: 10個の未分類スイートを追加
- `tests/timing-history.json`: 実行ごとの per-suite タイミングデータ（自動生成）
- `AGENTS.md`: テスト規約セクション

### Definition of Done
- [x] Safe スイートが真に並列実行される（wall-clock < 個別合計の50%）
- [x] `run-tests-parallel` の結果が `fiveam:run 'sibyl-tests` と同一チェック数
- [x] 同一イメージでの再実行が正しく動作（status リセット）
- [x] 未分類スイートに対してバリデーション警告が出る
- [x] タイミング履歴が JSON に記録される
- [x] `(asdf:test-system :sibyl)` が引き続き動作する

### Must Have
- Safe スイートのロックなし並列実行
- `(status test)` の `:unknown` リセット（REPL 再実行対応）
- スイート分類バリデーション（警告レベル）
- タイミング履歴の記録と回帰検出
- AGENTS.md のテスト規約

### Must NOT Have (Guardrails)
- FiveAM ソースファイルの変更
- `run-sibyl-tests` 関数の変更（ASDF エントリポイント）
- 新しいパッケージ依存の追加
- テスト定義やテスト動作の変更
- Unsafe スイートの並列化
- スレッドプール（v1 はスイート数 = スレッド数のシンプル方式）
- CI 統合（タイミング履歴はローカルファイルのみ）
- 重複 `def-suite` 定義の修正（既存の別問題）

---

## Verification Strategy (MANDATORY)

> **UNIVERSAL RULE: ZERO HUMAN INTERVENTION**
>
> ALL tasks in this plan MUST be verifiable WITHOUT any human action.

### Test Decision
- **Infrastructure exists**: YES (FiveAM + existing test suite)
- **Automated tests**: YES (Tests-after) — ランナー自体のセルフテスト
- **Framework**: FiveAM (既存)

### Agent-Executed QA Scenarios (MANDATORY — ALL tasks)

> 全タスクの検証は SBCL REPL での eval 形式で実行。
> Playwright/tmux ではなく、Bash 経由で `sbcl --eval` を使用。

**Verification Tool:**

| Type | Tool | How Agent Verifies |
|------|------|-------------------|
| **Lisp テスト実行** | Bash (`sbcl --eval`) | SBCL プロセスでテスト実行、出力をパース、exit code 確認 |
| **ファイル生成確認** | Bash (ls, cat) | ファイル存在確認、JSON 構文チェック |
| **回帰テスト** | Bash (`sbcl --eval`) | 2回実行して結果比較 |

---

## Execution Strategy

### Parallel Execution Waves

```
Wave 1 (Start Immediately):
├── Task 1: 未分類スイートの分類 [no dependencies]
└── Task 4: AGENTS.md テスト規約 [no dependencies]

Wave 2 (After Task 1):
├── Task 2: カスタム並列ランナー本体 [depends: 1]
└── Task 3: スイート分類バリデーション [depends: 1]

Wave 3 (After Wave 2):
├── Task 5: タイミング履歴 & 回帰検出 [depends: 2]

Wave 4 (After Wave 3):
└── Task 6: 統合検証 & セルフテスト [depends: 2, 3, 5]

Critical Path: Task 1 → Task 2 → Task 5 → Task 6
Parallel Speedup: ~30% faster than sequential (Wave 1 + Wave 2 parallelism)
```

### Dependency Matrix

| Task | Depends On | Blocks | Can Parallelize With |
|------|------------|--------|---------------------|
| 1 | None | 2, 3 | 4 |
| 2 | 1 | 5, 6 | 3 |
| 3 | 1 | 6 | 2 |
| 4 | None | 6 | 1 |
| 5 | 2 | 6 | 3 |
| 6 | 2, 3, 5 | None | None (final) |

### Agent Dispatch Summary

| Wave | Tasks | Recommended Agents |
|------|-------|-------------------|
| 1 | 1, 4 | task(category="quick") / task(category="writing") |
| 2 | 2, 3 | task(category="deep") / task(category="quick") |
| 3 | 5 | task(category="unspecified-low") |
| 4 | 6 | task(category="deep") |

---

## TODOs

- [x] 1. 未分類スイートの分類追加

  **What to do**:
  - 10個の未分類スイートを調査し、Safe/Unsafe に分類する
  - 各スイートの定義を読み、ファイルI/O・グローバル状態変更・スリープ・ネットワーク・ツール実行の有無を判定
  - `*safe-suites*` または `*unsafe-suites*` に追加
  - 親スイート `sibyl-tests` は意図的に除外されているため、そのまま

  **判定基準**:
  - SAFE: 純粋ロジック、ファイルI/Oなし、グローバル状態変更なし、外部プロセスなし
  - UNSAFE: 上記いずれかに該当

  **Metis 調査による候補**:

  | Suite | File | 推定分類 | 根拠 |
  |-------|------|---------|------|
  | `parallel-tool-execution-tests` | agent-test.lisp | UNSAFE | sleep + ツール実行 |
  | `tokens-tests` | repl-test.lisp | 要調査 | |
  | `token-tracking-suite` | token-tracking-test.lisp | 要調査 | |
  | `model-selector-suite` | token-tracking-test.lisp | 要調査 | |
  | `memory-compaction-suite` | token-tracking-test.lisp | 要調査 | |
  | `rich-repl-tests` | rich-repl-test.lisp | UNSAFE | sleep 使用 |
  | `phase6-tests` | phase6-test.lisp | 要調査 | |
  | `mcp-tests` | mcp-test.lisp | 要調査 | |
  | `parallel-agent-tests` | parallel-agent-test.lisp | UNSAFE | スレッド + sleep |
  | `sibyl-tests` | suite.lisp | 除外（親） | 意図的 |

  **Must NOT do**:
  - テスト定義の変更
  - 重複 `def-suite` の修正（別問題）

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: 各スイートの内容を読んで safe/unsafe を判定し、リストに追加するだけ
  - **Skills**: []
    - スキル不要 — Lisp ソースの読解と少量の編集

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Task 4)
  - **Blocks**: Tasks 2, 3
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `tests/suite.lisp:46-62` — `*safe-suites*` の定義パターン。新スイートもこの形式で追加する
  - `tests/suite.lisp:80-102` — `*unsafe-suites*` の定義パターン
  - `tests/suite.lisp:66-78` — `%safe-suites-resolved` のクロスパッケージ解決パターン。`sibyl.agent.tests` パッケージのスイートは `%resolve-suite` 経由で追加する

  **対象スイート定義の参照先**:
  - `tests/agent-test.lisp` — `parallel-tool-execution-tests` の定義を読んで sleep/ツール実行の有無を確認
  - `tests/repl-test.lisp` — `tokens-tests` の定義を読んで純粋ロジックか判定
  - `tests/token-tracking-test.lisp` — `token-tracking-suite`, `model-selector-suite`, `memory-compaction-suite` の定義を読む
  - `tests/rich-repl-test.lisp` — `rich-repl-tests` の定義を読む（sleep 使用の可能性）
  - `tests/phase6-test.lisp` — `phase6-tests` の定義を読む
  - `tests/mcp-test.lisp` — `mcp-tests` の定義を読む（プロトコルロジックか実ネットワークか）
  - `tests/parallel-agent-test.lisp` — `parallel-agent-tests` の定義を読む（スレッド使用の可能性）

  **Documentation References**:
  - `tests/suite.lisp:31-35` — LLM GUIDANCE コメント。Safe/Unsafe の判定基準が明記されている

  **Acceptance Criteria**:

  - [ ] `*safe-suites*` + `*unsafe-suites*` + `%safe-suites-resolved` に全スイートが含まれる（`sibyl-tests` 親を除く）
  - [ ] 各スイートの分類理由がコメントで記載されている（既存スイートにはないが、新規追加分には判断根拠を `;;` コメントで添える）

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: All non-parent suites are classified
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl/tests system loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
              --eval '(in-package :sibyl.tests)' \
              --eval '(let* ((all-safe (sibyl.tests::%safe-suites-resolved))
                             (all-unsafe sibyl.tests::*unsafe-suites*)
                             (all-classified (append all-safe all-unsafe)))
                       (format t "Safe: ~a~%Unsafe: ~a~%Total: ~a~%"
                               (length all-safe) (length all-unsafe) (length all-classified)))' \
              --eval '(sb-ext:exit)'
      2. Assert: Total count >= 28 (current 15+21=36 safe+unsafe, plus ~9 newly classified)
      3. Assert: exit code 0
    Expected Result: All suites classified, no warnings
    Evidence: Terminal output captured
  ```

  **Commit**: YES (groups with Task 3)
  - Message: `fix(tests): classify 10 missing test suites into safe/unsafe`
  - Files: `tests/suite.lisp`

---

- [x] 2. カスタム並列ランナー本体の実装

  **What to do**:

  **Step 1**: 新関数 `%reset-suite-statuses` を追加
  - 指定スイートに属する全テストケースの `(status test)` を `:unknown` にリセット
  - FiveAM の `test-bundle` からテストを列挙し、各 `testable-object` の status スロットをリセット
  - SBCL パッケージロック対策: `find-symbol` パターンまたは `sb-ext:without-package-locks`

  **Step 2**: 新関数 `%run-suite-isolated` を追加（`%collect-fiveam-results` の置き換え）
  - ロックを取らずにスイートを実行
  - 以下の3変数をスレッドローカルに rebind:
    - `it.bese.fiveam::*test-dribble*` → `(make-broadcast-stream)`（出力抑制）
    - `it.bese.fiveam::*test-dribble-indent*` → 新しい adjustable vector `(make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)`
    - `it.bese.fiveam::*print-names*` → `nil`（garbled 出力防止）
  - `%reset-suite-statuses` を呼んでステータスリセット
  - `fiveam::return-result-list` + `fiveam::%run` で実行
  - 結果を返す

  **Step 3**: `run-tests-parallel` を改修
  - Safe phase: `%collect-fiveam-results` → `%run-suite-isolated` に置き換え（ロック不要）
  - Unsafe phase: `%collect-fiveam-results` をそのまま使用（ロック保持 — Unsafe 間の状態競合を防ぐため）
  - `%collect-fiveam-results` は Unsafe 用に残すが、内部で `%reset-suite-statuses` を追加
  - `*error-output*` への並列書き込み: per-thread で `make-string-output-stream` にバインドし、thread join 後にメインストリームに flush

  **Step 4**: `with-codebase-map-cache` のスレッド安全性確認
  - Safe スイートで `codebase-map` を呼ぶものがあるか確認
  - ある場合: `with-codebase-map-cache` を per-thread に移動、またはハッシュテーブルに `:synchronized t` を指定
  - ない場合: 現状のまま（外側スコープでバインド）

  **Must NOT do**:
  - `run-sibyl-tests` の変更
  - FiveAM ソースの変更
  - 新パッケージ依存の追加
  - Unsafe スイートの並列化
  - スレッドプールの導入（v1 はスレッド/スイート = 1:1）

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: FiveAM の内部構造を理解した上での安全な並行処理実装。パッケージロック、status リセット、スレッドローカル rebind など複数の技術的制約を同時に満たす必要がある
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Task 3)
  - **Blocks**: Tasks 5, 6
  - **Blocked By**: Task 1

  **References**:

  **Pattern References**:
  - `tests/suite.lisp:104-106` — 現在の `*fiveam-run-lock*` 定義。これが置き換え対象
  - `tests/suite.lisp:121-126` — 現在の `%collect-fiveam-results`。ロックを取って `fiveam:run` を呼ぶ。この関数を Safe 用 `%run-suite-isolated`（ロックなし）と Unsafe 用（ロックあり + status リセット追加）に分割する
  - `tests/suite.lisp:150-256` — 現在の `run-tests-parallel` 全体。Phase 1（並列）と Phase 2（直列）の構造はそのまま維持しつつ、Phase 1 で `%run-suite-isolated` を使う
  - `tests/suite.lisp:170-187` — スレッド生成部分。`bt:make-thread` + `handler-case` パターンをそのまま踏襲
  - `tests/suite.lisp:214-217` — `find-symbol` + `find-class` で FiveAM 内部にアクセスするパターン。パッケージロック回避の参考

  **FiveAM Internal References** (rebind 対象の定義元):
  - `~/quicklisp/dists/quicklisp/software/fiveam-20241012-git/src/check.lisp:18` — `*test-dribble*` の defvar 定義
  - `~/quicklisp/dists/quicklisp/software/fiveam-20241012-git/src/run.lisp:61` — `*print-names*` の defparameter 定義
  - `~/quicklisp/dists/quicklisp/software/fiveam-20241012-git/src/run.lisp:64-68` — `*test-dribble-indent*` の定義（adjustable vector）
  - `~/quicklisp/dists/quicklisp/software/fiveam-20241012-git/src/run.lisp:160-166` — `return-result-list` の定義。`bind-run-state` で `result-list` をスレッドローカルにバインド
  - `~/quicklisp/dists/quicklisp/software/fiveam-20241012-git/src/run.lisp:245-267` — `%run` for suites。`vector-push-extend`/`vector-pop` で `*test-dribble-indent*` を変更する箇所
  - `~/quicklisp/dists/quicklisp/software/fiveam-20241012-git/src/run.lisp:90-118` — `run-resolving-dependencies`。`(status test)` を `:unknown` → `:resolving` → `t`/`nil` と遷移させる。ここが status リセット不在時にテストがスキップされる原因
  - `~/quicklisp/dists/quicklisp/software/fiveam-20241012-git/src/classes.lisp:12` — `status` スロット定義（`testable-object` クラス）
  - `~/quicklisp/dists/quicklisp/software/fiveam-20241012-git/src/test.lisp:16-26` — `*test*` グローバルレジストリ（`test-bundle` インスタンス）。テスト実行前に登録済みなら読み取り専用

  **Documentation References**:
  - `tests/suite.lisp:27-35` — スイート分類の LLM GUIDANCE コメント。ロック順序も記載
  - `tests/suite.lisp:153-158` — `run-tests-parallel` の docstring。実行戦略を記載（更新が必要）

  **Acceptance Criteria**:

  - [ ] `%run-suite-isolated` が定義され、ロックなしでスイートを実行する
  - [ ] `%reset-suite-statuses` が定義され、全テストの status を `:unknown` にリセットする
  - [ ] `run-tests-parallel` の Safe phase でロックが使用されていない
  - [ ] `run-tests-parallel` の Unsafe phase では引き続きロックが使用されている
  - [ ] `run-sibyl-tests` が変更されていない

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: Parallel execution is genuinely parallel (wall-clock verification)
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl/tests system loaded, all suites classified
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
              --eval '(sibyl.tests:run-tests-parallel)' \
              --eval '(sb-ext:exit)'
      2. Capture "Per-suite timing (safe):" section from output
      3. Capture "Total wall-clock:" line
      4. Calculate: sum of individual safe suite times vs safe wall-clock
      5. Assert: safe wall-clock < sum of individual times * 0.7
         (at least 30% faster than serial, confirming parallelism)
      6. Assert: exit code 0
    Expected Result: Safe phase wall-clock significantly less than sum of individual times
    Evidence: Terminal output with timing data

  Scenario: Result equivalence with serial execution
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl/tests system loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
              --eval '(let* ((parallel-count (length (sibyl.tests:run-tests-parallel)))
                             (serial-count (progn
                                            (let ((it.bese.fiveam:*test-dribble* (make-broadcast-stream)))
                                              (length (it.bese.fiveam:run (quote sibyl.tests::sibyl-tests)))))))
                       (format t "Parallel: ~a checks~%Serial: ~a checks~%" parallel-count serial-count)
                       (assert (= parallel-count serial-count)))' \
              --eval '(sb-ext:exit)'
      2. Assert: Parallel count equals Serial count
      3. Assert: exit code 0
    Expected Result: Identical check counts
    Evidence: Terminal output showing both counts

  Scenario: Re-run in same image produces correct results (status reset verification)
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl/tests system loaded
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
              --eval '(let ((run1 (length (sibyl.tests:run-tests-parallel)))
                            (run2 (length (sibyl.tests:run-tests-parallel))))
                       (format t "Run1: ~a~%Run2: ~a~%" run1 run2)
                       (assert (= run1 run2))
                       (assert (> run1 0)))' \
              --eval '(sb-ext:exit)'
      2. Assert: run1 = run2, both > 0
      3. Assert: exit code 0
    Expected Result: Both runs produce identical non-zero check counts
    Evidence: Terminal output

  Scenario: ASDF test-system still works
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl system loadable
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
              --eval '(asdf:test-system :sibyl)' \
              --eval '(sb-ext:exit :code 0)'
      2. Assert: exit code 0
    Expected Result: Clean exit, no errors
    Evidence: Exit code
  ```

  **Commit**: YES
  - Message: `feat(tests): implement true parallel execution for safe test suites`
  - Files: `tests/suite.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(sibyl.tests:run-tests-parallel)' --eval '(sb-ext:exit)'`

---

- [x] 3. スイート分類バリデーション機構

  **What to do**:
  - `%validate-suite-classification` 関数を追加
  - FiveAM レジストリに登録された全スイートを列挙
  - `*safe-suites*` + `*unsafe-suites*` + `%safe-suites-resolved` と突き合わせ
  - 未分類スイートがあれば `*error-output*` に警告を出力（エラーではない — テスト実行は継続）
  - `run-tests-parallel` の冒頭で自動呼び出し
  - 親スイート `sibyl-tests` は意図的に除外（ハードコード）
  - cross-suite `depends-on` の検出も追加: Safe スイートのテストが別スイートに依存していたら警告

  **Must NOT do**:
  - 警告をエラーにしない（テスト実行をブロックしない）
  - FiveAM ソースの変更
  - テスト定義の変更

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: FiveAM レジストリの走査と集合差分計算。ロジックは単純
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with Task 2)
  - **Blocks**: Task 6
  - **Blocked By**: Task 1

  **References**:

  **Pattern References**:
  - `tests/suite.lisp:137-148` — 既存の `%validate-suites` 関数。FiveAM レジストリの `fiveam:get-test` でスイート存在確認するパターン。新バリデーターはこの逆方向（レジストリ → 分類リスト）の照合
  - `tests/suite.lisp:46-62` — `*safe-suites*` リスト
  - `tests/suite.lisp:66-78` — `%safe-suites-resolved` 関数（クロスパッケージ解決込み）
  - `tests/suite.lisp:80-102` — `*unsafe-suites*` リスト

  **FiveAM Internal References**:
  - `~/quicklisp/dists/quicklisp/software/fiveam-20241012-git/src/test.lisp:16-26` — `*test*` レジストリ。`(%tests *test*)` でハッシュテーブルにアクセスし、全登録テスト/スイートを列挙
  - `~/quicklisp/dists/quicklisp/software/fiveam-20241012-git/src/classes.lisp` — `test-suite` クラスの定義。`(typep obj 'test-suite)` でスイートかテストケースかを判定

  **Acceptance Criteria**:

  - [ ] `%validate-suite-classification` が定義されている
  - [ ] 意図的に未分類スイートを作った場合に警告が出力される
  - [ ] 全スイートが分類済みの場合は警告なし
  - [ ] `run-tests-parallel` の冒頭で自動呼び出しされる
  - [ ] cross-suite `depends-on` があれば警告が出る

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: Validator detects unclassified suite
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl/tests loaded, all suites properly classified
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
              --eval '(in-package :sibyl.tests)' \
              --eval '(fiveam:def-suite test-orphan-suite :in sibyl-tests)' \
              --eval '(let ((output (with-output-to-string (*error-output*)
                                     (sibyl.tests::%validate-suite-classification))))
                       (format t "~a" output)
                       (assert (search "test-orphan-suite" (string-downcase output))))' \
              --eval '(sb-ext:exit)'
      2. Assert: output contains "test-orphan-suite" warning
      3. Assert: exit code 0 (warning, not error)
    Expected Result: Warning about unclassified suite, no crash
    Evidence: Terminal output

  Scenario: No warnings when all classified
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl/tests loaded, all suites classified (Task 1 complete)
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
              --eval '(let ((output (with-output-to-string (*error-output*)
                                     (sibyl.tests::%validate-suite-classification))))
                       (format t "Warnings: [~a]~%" output)
                       (assert (= 0 (length (string-trim (list #\Space #\Newline) output)))))' \
              --eval '(sb-ext:exit)'
      2. Assert: no warning output
      3. Assert: exit code 0
    Expected Result: Clean validation with no warnings
    Evidence: Terminal output
  ```

  **Commit**: YES (groups with Task 1)
  - Message: `feat(tests): add suite classification validator with depends-on check`
  - Files: `tests/suite.lisp`

---

- [x] 4. AGENTS.md テスト規約セクション追加 — Already complete (content present)

  **What to do**:
  - `AGENTS.md` の "Testing Guidelines" セクションを拡充
  - 以下の規約を追加:
    1. **スイート分類必須**: 新スイートは必ず `*safe-suites*` または `*unsafe-suites*` に登録する。`run-tests-parallel` 冒頭のバリデーターが未登録を検出する
    2. **Safe/Unsafe 判定基準**: 既存の基準（SAFE: pure logic... / UNSAFE: file I/O...）を AGENTS.md にも転記
    3. **テスト速度への配慮**: 不必要な `sleep` を避ける。I/O が不要なら Safe にする。外部 API は必ずモックする
    4. **既存モックパターンの案内**: CLOS override (`mock-llm-client`)、factory functions (`%make-ollama-text-response`)、let-binding (`*self-assess-last-test-results*`)
    5. **回帰検出**: `tests/timing-history.json` にタイミングが記録され、著しい劣化は警告される
    6. **並列実行の前提**: Safe スイートは独立して並列実行される。cross-suite `depends-on` は禁止。グローバル状態を変更するなら Unsafe に分類する

  **Must NOT do**:
  - 既存の AGENTS.md の他セクションの変更
  - ドキュメントの過剰な長文化（簡潔に）

  **Recommended Agent Profile**:
  - **Category**: `writing`
    - Reason: ドキュメント執筆タスク
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with Task 1)
  - **Blocks**: Task 6 (soft dependency — ガイドラインが完成していることを最終検証で確認)
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `AGENTS.md` — 既存の "Testing Guidelines" セクション。現在の記述スタイルと粒度を踏襲
  - `tests/suite.lisp:31-35` — LLM GUIDANCE コメント。Safe/Unsafe の判定基準が簡潔にまとまっている

  **Documentation References**:
  - `tests/cache-test.lisp` — `mock-llm-client` パターンの実例（CLOS override による LLM モック。ファイル内で `defclass mock-llm-client` を検索）
  - `tests/ollama-test.lisp:1-10` — factory function パターンの実例（`%make-ollama-text-response`）
  - `tests/analysis-tools-test.lisp` — let-binding モックパターンの実例（`*self-assess-last-test-results*`）

  **Acceptance Criteria**:

  - [ ] AGENTS.md に "Testing Guidelines" セクションが拡充されている
  - [ ] スイート分類必須の規約が記載されている
  - [ ] Safe/Unsafe 判定基準が記載されている
  - [ ] モックパターン3種が案内されている
  - [ ] 並列実行の前提（cross-suite depends-on 禁止等）が記載されている

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: AGENTS.md contains required test guidelines
    Tool: Bash (grep)
    Preconditions: AGENTS.md updated
    Steps:
      1. grep -c "safe-suites\|unsafe-suites\|*safe-suites*\|*unsafe-suites*" AGENTS.md
      2. grep -c "mock-llm-client\|mock.*pattern\|CLOS override" AGENTS.md
      3. grep -c "depends-on\|cross-suite\|parallel" AGENTS.md
      4. Assert: each grep returns >= 1 match
    Expected Result: All key topics covered
    Evidence: grep match counts
  ```

  **Commit**: YES
  - Message: `docs(agents): add test speed conventions and parallel execution guidelines`
  - Files: `AGENTS.md`

---

- [x] 5. タイミング履歴記録 & 回帰検出

  **What to do**:

  **Step 1**: `%record-timing-history` 関数を追加
  - `run-tests-parallel` の実行結果（per-suite タイミング）を JSON ファイルに追記
  - ファイルパス: `tests/timing-history.json`（git tracked — トレンド可視化のため）
  - JSON 構造:
    ```json
    [
      {
        "timestamp": "2026-02-19T12:00:00",
        "total-wall": 3.456,
        "safe-wall": 1.234,
        "unsafe-wall": 2.222,
        "suites": {
          "core-tests": {"elapsed": 0.001, "checks": 2},
          "tools-tests": {"elapsed": 0.045, "checks": 15},
          ...
        }
      }
    ]
    ```
  - ファイルが存在しなければ新規作成
  - JSON パースに失敗したら警告して空配列から再開始（壊れたファイルでテスト実行をブロックしない）
  - 履歴は最新 50 件に制限（古いエントリを削除）

  **Step 2**: `%detect-regression` 関数を追加
  - 直近5回の実行と比較
  - スイートごとに: 今回の elapsed が直近5回の中央値の 2倍を超えたら警告
  - 全体の total-wall が直近5回の中央値の 1.5倍を超えたら警告
  - 警告は `*error-output*` に出力（エラーではない）
  - 履歴が5件未満の場合はスキップ（比較不能）

  **Step 3**: `run-tests-parallel` の末尾に組み込み
  - 結果出力の後に `%record-timing-history` → `%detect-regression` を呼び出し
  - 例外が起きてもテスト結果には影響しない（`handler-case` で囲む）

  **Must NOT do**:
  - タイミングファイル不在/破損でテスト実行を失敗させない
  - yason 以外の JSON ライブラリを追加しない（yason は既存依存）
  - CI 統合（ローカルファイルのみ）

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: JSON 読み書きと統計計算。複雑ではないが正確性が必要
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Sequential (Wave 3)
  - **Blocks**: Task 6
  - **Blocked By**: Task 2

  **References**:

  **Pattern References**:
  - `tests/suite.lisp:224-228` — 現在の wall-clock 計算部分。`safe-wall`, `unsafe-wall`, `total-wall` の算出方法。この値をそのまま JSON に記録する
  - `tests/suite.lisp:234-251` — per-suite タイミング出力部分。`suite`, `elapsed`, `checks` の取得パターン

  **API/Type References**:
  - `src/` 内の yason 使用例 — `yason:parse`, `yason:encode` の使用パターンを確認。プロジェクト内の既存 JSON 操作パターンに合わせる

  **Acceptance Criteria**:

  - [ ] `%record-timing-history` が定義され、JSON ファイルに追記する
  - [ ] `%detect-regression` が定義され、中央値の2倍超で警告する
  - [ ] JSON ファイルが存在しない場合に新規作成される
  - [ ] JSON ファイルが壊れている場合にリカバリーされる（空配列から再開始）
  - [ ] 履歴が50件に制限される
  - [ ] `run-tests-parallel` の末尾で自動呼び出しされる

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: Timing history is recorded after test run
    Tool: Bash (sbcl --eval)
    Preconditions: sibyl/tests loaded, timing-history.json may or may not exist
    Steps:
      1. rm -f tests/timing-history.json
      2. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
              --eval '(sibyl.tests:run-tests-parallel)' \
              --eval '(sb-ext:exit)'
      3. Assert: tests/timing-history.json exists
      4. sbcl --eval '(ql:quickload :yason :silent t)' \
              --eval '(let* ((content (uiop:read-file-string "tests/timing-history.json"))
                             (data (yason:parse content)))
                       (format t "Entries: ~a~%" (length data))
                       (assert (= 1 (length data)))
                       (assert (gethash "total-wall" (first data))))' \
              --eval '(sb-ext:exit)'
      5. Assert: 1 entry, with "total-wall" field
    Expected Result: Valid JSON timing file created
    Evidence: File content and parsed structure

  Scenario: Regression detection warns on slow suite
    Tool: Bash (sbcl --eval)
    Preconditions: timing-history.json with at least 5 entries
    Steps:
      1. Create timing-history.json with 5 entries where core-tests elapsed is 0.001
      2. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
              --eval '(in-package :sibyl.tests)' \
              --eval '(let ((output (with-output-to-string (*error-output*)
                                     (%detect-regression
                                      (yason:parse (uiop:read-file-string "tests/timing-history.json"))
                                      (list (cons :core-tests (cons nil 5.0)))))))
                       (format t "~a" output)
                       (assert (search "regression" (string-downcase output))))' \
              --eval '(sb-ext:exit)'
      3. Assert: output contains regression warning
    Expected Result: Regression warning for artificially slow suite
    Evidence: Terminal output
  ```

  **Commit**: YES
  - Message: `feat(tests): add timing history recording and regression detection`
  - Files: `tests/suite.lisp`, `tests/timing-history.json` (auto-generated)

---

- [x] 6. 統合検証

  **What to do**:
  - 全タスクの成果物を統合した上で、エンドツーエンドの検証を実施
  - 以下の全シナリオを順に実行:
    1. `run-tests-parallel` が全テストをパスすること
    2. 並列実行が実際に速くなっていること（タイミング比較）
    3. `(asdf:test-system :sibyl)` が動作すること
    4. 分類バリデーションが警告なしで通ること
    5. タイミング履歴が正しく記録されること
    6. REPL での再実行が正しく動くこと
    7. AGENTS.md にテスト規約が記載されていること
  - 問題があれば修正

  **Must NOT do**:
  - テスト定義の変更
  - 新機能の追加（検証のみ）

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: 複数のコンポーネントにまたがる統合検証。問題発見時の原因追跡と修正を自律的に行う必要がある
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Sequential (Wave 4, final)
  - **Blocks**: None
  - **Blocked By**: Tasks 2, 3, 5

  **References**:

  **全タスクの成果物を参照**:
  - `tests/suite.lisp` — Task 1, 2, 3, 5 の変更が全て反映されているか確認
  - `AGENTS.md` — Task 4 の変更が反映されているか確認
  - `tests/timing-history.json` — Task 5 により自動生成されているか確認

  **Acceptance Criteria**:

  - [ ] `run-tests-parallel` が全テストをパス（0 failures, 0 errors）
  - [ ] Safe phase の wall-clock が個別合計の 70% 未満（真の並列実行確認）
  - [ ] `(asdf:test-system :sibyl)` が exit code 0
  - [ ] 分類バリデーションが警告なし
  - [ ] `tests/timing-history.json` が有効な JSON
  - [ ] 同一イメージでの2回実行で同一結果
  - [ ] AGENTS.md にスイート分類規約が記載

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: Full integration verification
    Tool: Bash (sbcl --eval)
    Preconditions: All tasks (1-5) completed
    Steps:
      1. rm -f tests/timing-history.json
      2. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
              --eval '(let ((results (sibyl.tests:run-tests-parallel)))
                       (let* ((fiveam-pkg (find-package "IT.BESE.FIVEAM"))
                              (failed-class (find-class (find-symbol "TEST-FAILED" fiveam-pkg) nil))
                              (error-class (find-class (find-symbol "UNEXPECTED-TEST-FAILURE" fiveam-pkg) nil))
                              (fails (count-if (lambda (r) (and failed-class (typep r failed-class))) results))
                              (errs (count-if (lambda (r) (and error-class (typep r error-class))) results)))
                         (format t "Total checks: ~a~%Failures: ~a~%Errors: ~a~%" (length results) fails errs)
                         (assert (= 0 (+ fails errs)))))' \
              --eval '(sb-ext:exit)'
      3. Assert: 0 failures, 0 errors
      4. Assert: exit code 0
    Expected Result: All tests pass
    Evidence: Terminal output with check counts

  Scenario: Second run in same image
    Tool: Bash (sbcl --eval)
    Preconditions: First run completed
    Steps:
      1. sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
              --eval '(let ((r1 (length (sibyl.tests:run-tests-parallel)))
                            (r2 (length (sibyl.tests:run-tests-parallel))))
                       (format t "Run1: ~a checks~%Run2: ~a checks~%" r1 r2)
                       (assert (= r1 r2))
                       (assert (> r1 0)))' \
              --eval '(sb-ext:exit)'
      2. Assert: r1 = r2, both > 0
    Expected Result: Identical results on re-run
    Evidence: Terminal output

  Scenario: Timing file generated and valid
    Tool: Bash
    Preconditions: run-tests-parallel has been called at least once
    Steps:
      1. test -f tests/timing-history.json
      2. sbcl --eval '(ql:quickload :yason :silent t)' \
              --eval '(let ((data (yason:parse (uiop:read-file-string "tests/timing-history.json"))))
                       (format t "Entries: ~a~%" (length data))
                       (assert (>= (length data) 1)))' \
              --eval '(sb-ext:exit)'
      3. Assert: file exists, valid JSON, >= 1 entry
    Expected Result: Timing history persisted
    Evidence: File existence and parsed content
  ```

  **Commit**: NO (verification only — commits from prior tasks)

---

## Commit Strategy

| After Task | Message | Files | Verification |
|------------|---------|-------|--------------|
| 1 + 3 | `fix(tests): classify 10 missing test suites into safe/unsafe` then `feat(tests): add suite classification validator with depends-on check` | tests/suite.lisp | `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(sibyl.tests:run-tests-parallel)' --eval '(sb-ext:exit)'` |
| 2 | `feat(tests): implement true parallel execution for safe test suites` | tests/suite.lisp | same |
| 4 | `docs(agents): add test speed conventions and parallel execution guidelines` | AGENTS.md | grep checks |
| 5 | `feat(tests): add timing history recording and regression detection` | tests/suite.lisp, tests/timing-history.json | same + JSON validation |

---

## Success Criteria

### Verification Commands
```bash
# Full parallel test run — expect 0 failures
sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
     --eval '(sibyl.tests:run-tests-parallel)' \
     --eval '(sb-ext:exit :code 0)'

# ASDF entry point — expect clean exit
sbcl --eval '(ql:quickload :sibyl/tests :silent t)' \
     --eval '(asdf:test-system :sibyl)' \
     --eval '(sb-ext:exit :code 0)'

# Timing history — expect valid JSON
cat tests/timing-history.json | python3 -m json.tool
```

### Final Checklist
- [x] Safe スイートが真に並列実行される（wall-clock 検証）
- [x] 全テストが変わらずパスする
- [x] REPL 再実行が正しく動作する
- [x] 未分類スイートに対して警告が出る
- [x] タイミング履歴が JSON に記録される
- [x] 回帰検出が著しい劣化を警告する
- [x] `(asdf:test-system :sibyl)` が動作する
- [x] AGENTS.md にテスト規約が記載されている
- [x] FiveAM ソースが変更されていない
- [x] `run-sibyl-tests` が変更されていない
- [x] 新パッケージ依存が追加されていない
