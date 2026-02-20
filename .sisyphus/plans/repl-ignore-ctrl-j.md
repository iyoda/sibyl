# REPL: Ctrl+J readline-level unbinding

## TL;DR

> **Quick Summary**: Ctrl+J (LF, char 10) が GNU readline で `accept-line` にバインドされており、誤押下で入力が確定してしまう問題を、`cl-readline:unbind-key` で readline レベルで無効化する。
>
> **Deliverables**:
> - `start-repl` 内で `*ignore-ctrl-j*` が `t` の場合に `unbind-key` を呼ぶロジック追加
> - 既存コメントの更新
> - テスト追加
>
> **Estimated Effort**: Quick
> **Parallel Execution**: NO — single sequential task
> **Critical Path**: Task 1 (唯一のタスク)

---

## Context

### Original Request
Ctrl+J の入力を REPL で無視できるようにしたい。Ctrl+J を誤って押すと、Enter と同じく入力が確定（送信）されてしまう。

### Interview Summary
**Key Discussions**:
- 問題: Ctrl+J が readline の `accept-line` にバインドされており、誤送信が発生
- 既存の `%strip-ctrl-j` は readline 返却後の後処理であり、誤送信は防げない
- `cl-readline:unbind-key` で readline レベルで無効化するアプローチを採用
- デフォルトは `nil`（無効）のまま、`repl.ignore-ctrl-j` 設定で有効化

**Research Findings**:
- cl-readline は `unbind-key` を公開（`rl_unbind_key` の FFI バインディング）
- `(cl-readline:unbind-key (code-char 10))` で Ctrl+J を無効化、Enter (Ctrl+M, char 13) は影響なし
- `unbind-key` は key を null function にバインド（silent no-op）
- `ensure-initialization` を内部で呼ぶため、readline ロード後いつでも呼べる

### Metis Review
**Identified Gaps** (addressed):
- `readline-available-p` ガードが必要 → 計画に反映
- `unbind-key` 失敗時のログ出力が必要 → 計画に反映
- `(code-char 10)` を使用（`#\Newline` より明示的） → 計画に反映
- 既存コメント (L1269-1272) の更新が必要 → 計画に反映

---

## Work Objectives

### Core Objective
`*ignore-ctrl-j*` が有効な場合、cl-readline ロード後に Ctrl+J を readline レベルで無効化し、誤送信を防止する。

### Concrete Deliverables
- `src/repl.lisp` の `start-repl` 内: `%ensure-readline` 呼び出し後に `unbind-key` ロジック追加
- `src/repl.lisp` の既存コメント (L1269-1272) 更新
- `tests/rich-repl-test.lisp` にテスト追加

### Definition of Done
- [x] `repl.ignore-ctrl-j` を有効にした状態で REPL を起動し、Ctrl+J が無視される
- [x] 設定なし（デフォルト）では従来通り Ctrl+J が accept-line として動作
- [x] cl-readline が利用不可の場合もエラーにならない
- [x] 既存テストが全パス

### Must Have
- `readline-available-p` チェック（cl-readline 未ロード時の保護）
- `unbind-key` 失敗時の `log-warn` 出力
- `(code-char 10)` を使用（`#\Newline` ではなく）
- 既存の `%strip-ctrl-j` 後処理を defense-in-depth として維持

### Must NOT Have (Guardrails)
- ランタイムでのトグル切替機能（起動時に一度だけ設定）
- silent no-op 用のヘルパー関数作成（`unbind-key` の null function で十分）
- `.inputrc` ファイル統合
- 他のキーバインド変更
- ユーザー向けメッセージ・バナー追加
- デフォルト値の変更（`nil` のまま維持）

---

## Verification Strategy

> **ZERO HUMAN INTERVENTION** — ALL verification is agent-executed.

### Test Decision
- **Infrastructure exists**: YES
- **Automated tests**: YES (tests-after)
- **Framework**: FiveAM (`tests/rich-repl-test.lisp`)

### QA Policy
Evidence saved to `.sisyphus/evidence/task-{N}-{scenario-slug}.{ext}`.

- **REPL behavior**: Use interactive_bash (tmux) — Start REPL, send Ctrl+J, validate behavior

---

## Execution Strategy

### Parallel Execution Waves

```
Wave 1 (Single task):
└── Task 1: readline-level Ctrl+J unbinding + test [quick]

Wave FINAL (After Task 1):
├── Task F1: Plan compliance audit (oracle)
├── Task F2: Code quality review (unspecified-high)
├── Task F3: Real manual QA (unspecified-high)
└── Task F4: Scope fidelity check (deep)

Critical Path: Task 1 → F1-F4
```

### Dependency Matrix

| Task | Depends On | Blocks |
|------|-----------|--------|
| 1    | —         | F1-F4  |
| F1-F4| 1         | —      |

### Agent Dispatch Summary

- **Wave 1**: **1** — T1 → `quick`
- **FINAL**: **4** — F1 → `oracle`, F2 → `unspecified-high`, F3 → `unspecified-high`, F4 → `deep`

---

## TODOs

- [x] 1. Ctrl+J readline-level unbinding の実装とテスト

  **What to do**:

  **Step 1: `start-repl` に unbind-key ロジック追加** (`src/repl.lisp`)

  `(%ensure-readline)` の呼び出し（L1275）の直後、`(%ensure-utf8-locale)` の前に以下を挿入:

  ```lisp
  ;; Unbind Ctrl+J (linefeed) from accept-line at the readline level
  ;; to prevent accidental input submission. The post-processing
  ;; %strip-ctrl-j in read-user-input remains as defense-in-depth
  ;; for the fallback read-line path.
  (when (and *ignore-ctrl-j* (readline-available-p))
    (let ((result (funcall (find-symbol "UNBIND-KEY" :cl-readline)
                           (code-char 10))))
      (if result
          (log-warn "repl" "Failed to unbind Ctrl+J in readline")
          (log-info "repl" "Ctrl+J (linefeed) unbound from accept-line"))))
  ```

  注意:
  - `find-symbol` パターンを使用（compile-time 依存回避、既存パターンに準拠）
  - `unbind-key` は成功で `NIL`、失敗で `T` を返す（逆直感的なので注意）
  - 二重ガード: `*ignore-ctrl-j*` AND `readline-available-p`

  **Step 2: 既存コメント更新** (`src/repl.lisp` L1269-1272)

  現在のコメント:
  ```lisp
  ;; Ctrl+J filtering is handled at the application level by %strip-ctrl-j
  ;; in read-user-input.  Kernel-level termios manipulation (INLCR+IGNCR)
  ;; was removed because it also suppresses normal Enter (CR), making the
  ;; REPL unusable.
  ```

  更新後:
  ```lisp
  ;; When *ignore-ctrl-j* is T, Ctrl+J is unbound from accept-line at the
  ;; readline level (see unbind-key call after %ensure-readline).
  ;; The post-processing %strip-ctrl-j in read-user-input remains as
  ;; defense-in-depth for the fallback read-line path (no cl-readline).
  ```

  **Step 3: テスト追加** (`tests/rich-repl-test.lisp`)

  `rich-repl-test` スイート（または適切なスイート）に以下のテストを追加:

  ```lisp
  ;; Test: unbind-key is called when *ignore-ctrl-j* is t and readline is available
  ;; Mock cl-readline:unbind-key, set *ignore-ctrl-j* to t, verify it's called with (code-char 10)

  ;; Test: unbind-key is NOT called when *ignore-ctrl-j* is nil
  ;; Verify no call to unbind-key

  ;; Test: unbind-key is NOT called when readline is not available
  ;; Verify graceful handling without cl-readline
  ```

  **Must NOT do**:
  - `*ignore-ctrl-j*` のデフォルト値を変更しない
  - config.lisp のデフォルト (`("repl.ignore-ctrl-j" . nil)`) を変更しない
  - `%strip-ctrl-j` 関数を削除・変更しない
  - `read-user-input` の既存ロジックを変更しない
  - 新たなヘルパー関数を作らない

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: 1ファイルの小規模変更 + テスト追加。ロジックは単純（条件付き関数呼び出し）
  - **Skills**: []
    - 特別なスキル不要。Common Lisp コードの編集のみ
  - **Skills Evaluated but Omitted**:
    - `playwright`: ブラウザ不要
    - `git-master`: コミットは別タスク

  **Parallelization**:
  - **Can Run In Parallel**: NO (唯一のタスク)
  - **Parallel Group**: Wave 1 (単独)
  - **Blocks**: F1-F4
  - **Blocked By**: None

  **References** (CRITICAL):

  **Pattern References** (existing code to follow):
  - `src/repl.lisp:1179` — `find-symbol` パターンの参考（`(funcall (find-symbol "READLINE" :cl-readline) ...)`）
  - `src/repl.lisp:1318-1319` — history ロード時の `find-symbol` + `readline-available-p` ガードパターン
  - `src/repl.lisp:223-235` — `%ensure-readline` 実装（cl-readline のロードロジック）

  **API/Type References**:
  - `src/repl.lisp:52-53` — `*ignore-ctrl-j*` 変数定義
  - `src/repl.lisp:1161-1164` — `%strip-ctrl-j` 関数（defense-in-depth として維持）
  - `src/repl.lisp:1157-1159` — `readline-available-p` 関数
  - `src/repl.lisp:1267-1268` — `*ignore-ctrl-j*` の let バインディング
  - `src/config.lisp:114` — config デフォルト値 `("repl.ignore-ctrl-j" . nil)`

  **External References**:
  - cl-readline `unbind-key`: `/Users/iyoda/quicklisp/dists/quicklisp/software/cl-readline-20250622-git/cl-readline.lisp:677-689`
    — `unbind-key` の実装。`int-char` 型で key を受取り、`rl_unbind_key` を呼ぶ。成功で NIL、失敗で T を返す

  **Test References**:
  - `tests/rich-repl-test.lisp:234-237` — readline availability probe テスト（既存テストパターンの参考）

  **WHY Each Reference Matters**:
  - `find-symbol` パターン: cl-readline はランタイムロードのため compile-time シンボル参照不可。既存の `find-symbol` + `funcall` パターンに厳密に従うこと
  - `readline-available-p`: cl-readline 未ロード時に `find-symbol` が NIL を返す → `funcall` でエラー。必ずガードすること
  - `*ignore-ctrl-j*` の let バインディング (L1267): この let スコープ内でのみ `*ignore-ctrl-j*` が有効。unbind-key 呼び出しもこのスコープ内に配置すること

  **Acceptance Criteria**:

  - [x] `src/repl.lisp` に unbind-key 呼び出しロジックが追加されている
  - [x] 二重ガード (`*ignore-ctrl-j*` AND `readline-available-p`) が実装されている
  - [x] `(code-char 10)` が使用されている（`#\Newline` ではない）
  - [x] 失敗時に `log-warn` が出力される
  - [x] 成功時に `log-info` が出力される
  - [x] 既存コメント (L1269-1272) が更新されている
  - [x] `%strip-ctrl-j` と `read-user-input` の既存ロジックが変更されていない
  - [x] テストが追加されている
  - [x] `(asdf:test-system :sibyl)` → 全パス

  **QA Scenarios (MANDATORY):**

  ```
  Scenario: ignore-ctrl-j 有効時に REPL で Ctrl+J が無視される
    Tool: interactive_bash (tmux)
    Preconditions: ~/.sibyl/config.lisp に (setf (config-value "repl.ignore-ctrl-j") t) を設定
    Steps:
      1. tmux で REPL を起動: sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sibyl:with-config () (sibyl:start-repl :client (sibyl:make-anthropic-client)))'
      2. プロンプトが表示されるまで待機 (timeout: 30s)
      3. "hello" と入力（Enter を押さない）
      4. Ctrl+J を送信: send-keys C-j
      5. 1秒待機
      6. カーソル位置を確認 — 同じ行にいること（行が送信されていない）
      7. Enter を送信して入力を確定
    Expected Result: Ctrl+J 送信後も入力行が確定せず、"hello" がそのまま編集行に残っている。Enter で正常に送信される
    Failure Indicators: Ctrl+J 送信後に "hello" が LLM に送信される（応答が返ってくる）
    Evidence: .sisyphus/evidence/task-1-ctrl-j-ignored.png

  Scenario: ignore-ctrl-j 無効時（デフォルト）に Ctrl+J が従来通り動作
    Tool: interactive_bash (tmux)
    Preconditions: repl.ignore-ctrl-j 未設定（デフォルト nil）
    Steps:
      1. tmux で REPL を起動（上記と同じコマンド）
      2. プロンプトが表示されるまで待機 (timeout: 30s)
      3. "/help" と入力（Enter を押さない）
      4. Ctrl+J を送信: send-keys C-j
      5. 2秒待機
    Expected Result: Ctrl+J で "/help" が送信され、ヘルプメッセージが表示される
    Failure Indicators: Ctrl+J 送信後に入力行がそのまま残っている
    Evidence: .sisyphus/evidence/task-1-ctrl-j-default.png

  Scenario: cl-readline が利用不可の場合にエラーにならない
    Tool: Bash
    Preconditions: cl-readline パッケージが存在しない状態をシミュレート
    Steps:
      1. テストスイートを実行: sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --eval '(sb-ext:exit)'
      2. 全テストがパスすることを確認
    Expected Result: 全テストパス（0 failures, 0 errors）
    Failure Indicators: テスト失敗またはエラー
    Evidence: .sisyphus/evidence/task-1-test-results.txt
  ```

  **Evidence to Capture:**
  - [x] task-1-ctrl-j-ignored.png — Ctrl+J 無視の tmux スクリーンショット
  - [x] task-1-ctrl-j-default.png — デフォルト動作の tmux スクリーンショット
  - [x] task-1-test-results.txt — テスト実行結果

  **Commit**: YES
  - Message: `fix(repl): unbind Ctrl+J at readline level to prevent accidental submission`
  - Files: `src/repl.lisp`, `tests/rich-repl-test.lisp`
  - Pre-commit: `sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --eval '(sb-ext:exit)'`

---

## Final Verification Wave (MANDATORY — after ALL implementation tasks)

> 4 review agents run in PARALLEL. ALL must APPROVE. Rejection → fix → re-run.

- [x] F1. **Plan Compliance Audit** — `oracle`
  Read the plan end-to-end. Verify: unbind-key call exists with dual guard, `(code-char 10)` used, log-warn on failure, comment updated, `%strip-ctrl-j` preserved unchanged, config default unchanged. Check evidence files in `.sisyphus/evidence/`.
  Output: `Must Have [N/N] | Must NOT Have [N/N] | VERDICT: APPROVE/REJECT`

- [x] F2. **Code Quality Review** — `unspecified-high`
  Review changes in `src/repl.lisp` and `tests/rich-repl-test.lisp`. Check: no `as any` equivalent, no dead code, proper error handling, consistent `find-symbol` pattern usage, clean indentation (2-space CL convention). Run test suite.
  Output: `Tests [N pass/N fail] | Files [N clean/N issues] | VERDICT`

- [x] F3. **Real Manual QA** — `unspecified-high`
  Execute ALL QA scenarios from Task 1. Start REPL with and without config. Verify Ctrl+J behavior in both modes. Capture evidence screenshots.
  Output: `Scenarios [N/N pass] | VERDICT`

- [x] F4. **Scope Fidelity Check** — `deep`
  Verify 1:1 compliance: only files listed in plan were modified, no scope creep. Check that `config.lisp` default was NOT changed, `%strip-ctrl-j` was NOT modified, `read-user-input` was NOT changed. Flag any unaccounted changes.
  Output: `Tasks [N/N compliant] | Unaccounted [CLEAN/N files] | VERDICT`

---

## Commit Strategy

- **Task 1**: `fix(repl): unbind Ctrl+J at readline level to prevent accidental submission` — `src/repl.lisp`, `tests/rich-repl-test.lisp`

---

## Success Criteria

### Verification Commands
```bash
# Test suite passes
sbcl --eval '(ql:quickload :sibyl/tests :silent t)' --eval '(asdf:test-system :sibyl)' --eval '(sb-ext:exit)'
# Expected: all tests pass, 0 failures

# Clean load with config enabled
sbcl --eval '(ql:quickload :sibyl :silent t)' --eval '(sibyl:with-config () (let ((sibyl.repl::*ignore-ctrl-j* t)) (sibyl.repl::readline-available-p)))'
# Expected: no errors
```

### Final Checklist
- [x] unbind-key ロジックが `start-repl` に追加されている
- [x] 二重ガード (`*ignore-ctrl-j*` AND `readline-available-p`) 実装
- [x] `(code-char 10)` 使用
- [x] 失敗時 `log-warn`、成功時 `log-info`
- [x] 既存コメント更新
- [x] `%strip-ctrl-j` 維持（変更なし）
- [x] config デフォルト `nil` 維持（変更なし）
- [x] テスト追加・全パス
