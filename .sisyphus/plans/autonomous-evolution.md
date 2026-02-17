# Sibyl自律進化 Phase 7: 作成能力と自律継続改善

## TL;DR

> **Quick Summary**: Sibylの「作成ギャップ」（新ファイル・モジュール・パッケージの追加ができない）を解消し、`/evolve`コマンドによる継続的自己改善ループを実現する。Phase 0-6で構築した自己修正基盤を「修正→作成＋自律実行」に拡張。
> 
> **Deliverables**:
> - Phase 7A: 作成インフラ — 新ファイル作成、ASDF動的登録、パッケージ拡張、REPLコマンド動的登録
> - Phase 7B: 自律進化ループ — `/evolve`コマンドで継続的改善サイクルを自律実行
> 
> **Estimated Effort**: Large
> **Parallel Execution**: YES - 3 waves
> **Critical Path**: add-definition → create-module → ASDF登録 → 統合テスト → /evolve

---

## Context

### Original Request
oh-my-opencodeに近付けるための次のステップとして、まず自律性を向上させたい。

### Interview Summary
**Key Discussions**:
- **優先領域**: (1) 新ファイル・新モジュール作成、(2) マルチターン自律実行
- **自律実行のゴール像**: 継続的改善 — self-assess → improvement-plan → 実装のサイクルを自律的に回す
- **安全性モデル**: 最初から自動（TDDで品質保証、人間承認不要）
- **トリガー**: `/evolve` REPLコマンドで開始
- **停止条件**: 改善提案が無くなったら

**Research Findings**:
- 現在のアーキテクチャは単一エージェント・同期実行・20ツール・716テスト
- CLOSジェネリック関数で拡張容易（agent-step, complete, memory operations）
- safe-redefineはfboundp必須、sync-to-fileは既存定義必須 → 新規作成には新ツールが必要
- REPLディスパッチが`case`文（コンパイル時固定） → 動的ディスパッチへリファクタリング必要
- write-fileで新ファイルは書けるがASDFに登録されない

### Metis Review
**Identified Gaps** (addressed):
- **REPL dispatch refactoring**: `case`文 → 動的alist lookup に変更（register-commandの前提条件）
- **suggest-improvementsの停止条件**: 「提案が空」ではなく「high/medium提案が空、またはN回連続で新しい提案なし」に修正
- **新規作成は完全に新しいツール**: safe-redefine/sync-to-fileの拡張ではなく、新ツール群として設計

---

## Work Objectives

### Core Objective
Sibylが既存コードの修正だけでなく、新しいファイル・モジュール・関数・REPLコマンドを自律的に作成でき、`/evolve`コマンドで継続的自己改善サイクルを自動実行できるようにする。

### Concrete Deliverables
- `add-definition`ツール: 既存ファイルに新しいdefun/defmethod/deftool等を追加
- `create-module`ツール: 新.lispファイル作成 + ASDF登録 + コンパイル
- `add-export`ツール: packages.lispに新exportを追加
- REPLの動的ディスパッチ化 + `register-command`ツール
- `/evolve`コマンド: 継続的自己改善ループ
- 進化状態管理: 試行済み改善の追跡、重複防止

### Definition of Done
- [x] Sibylが新しい.lispファイルを作成し、ASDFに登録し、コンパイルできる
- [x] Sibylが既存ファイルに新しい関数定義を追加できる
- [x] Sibylが新しいREPLコマンドを動的に登録できる
- [x] `/evolve`で継続的改善サイクルが自律実行される
- [x] サイクルが適切な条件で停止する
- [x] 全テストがパスする: `(asdf:test-system :sibyl)` → 1040 checks, 100%

### Must Have
- TDD: 全ツールにテストファースト
- Condition Systemによるロールバック（作成の取り消しを含む）
- 作成されたコードのauto-compile
- `/evolve`の進捗表示（各サイクルで何を改善したか）
- 失敗した改善のスキップとログ記録

### Must NOT Have (Guardrails)
- **Sibylパッケージ外への書き込み禁止**: 新ファイルも`sibyl.*`パッケージに限定
- **ASDF破壊禁止**: sibyl.asdの変更は追加のみ（既存コンポーネントの削除・変更は不可）
- **無限ループ禁止**: `/evolve`には必ず停止条件（max cycles + 改善枯渇検出）
- **テスト無しの作成禁止**: 新しいツール・関数は必ずテスト付き
- **既存テストの破壊禁止**: `/evolve`サイクル中に既存テストが壊れたら即停止
- **system-promptの自動変更禁止**: `/evolve`がsystem-promptを書き換えない

---

## Verification Strategy (MANDATORY)

> **UNIVERSAL RULE: ZERO HUMAN INTERVENTION**
>
> ALL tasks in this plan MUST be verifiable WITHOUT any human action.

### Test Decision
- **Infrastructure exists**: YES (FiveAM, 716 tests passing)
- **Automated tests**: TDD (test-first)
- **Framework**: FiveAM (existing)

### TDD Workflow
Phase 0-6と同じRED→GREEN→REFACTORサイクル。

### Agent-Executed QA Scenarios (MANDATORY)

| Type | Tool | How Agent Verifies |
|------|------|-------------------|
| ツール定義 | Bash (sbcl --eval) | ql:quickload → ツール登録確認 → execute-tool → 結果検証 |
| ファイル作成 | Bash (sbcl --eval) | ファイル存在確認 → ASDF登録確認 → compile成功確認 |
| REPL拡張 | interactive_bash | REPLでコマンド実行 → 出力確認 |
| /evolveループ | interactive_bash | ループ開始 → 改善実行観察 → 停止確認 |

---

## Execution Strategy

### Parallel Execution Waves

```
Wave 1 (Start Immediately):
├── Task 7A-1: add-definition tool [no dependencies]
├── Task 7A-4: add-export tool [no dependencies]
└── Task 7A-5: REPL dynamic dispatch refactoring [no dependencies]

Wave 2 (After Wave 1):
├── Task 7A-2: create-module tool [depends: 7A-1]
├── Task 7A-6: register-command tool [depends: 7A-5]
├── Task 7B-2: Enhanced suggest-improvements [no dependencies from Wave 1]
└── Task 7B-3: Evolution state management [no dependencies from Wave 1]

Wave 3 (After Wave 2):
├── Task 7A-3: ASDF dynamic registration [depends: 7A-2]
└── Task 7B-4: Evolution progress reporting [depends: 7B-3]

Wave 4 (After Wave 3):
└── Task 7A-7: Creation integration test [depends: 7A-1 ~ 7A-6]

Wave 5 (After Wave 4):
└── Task 7B-1: /evolve command [depends: 7A-7, 7B-2, 7B-3, 7B-4]

Wave 6 (Final):
└── Task 7B-5: Autonomous evolution demonstration [depends: 7B-1]
```

### Dependency Matrix

| Task | Depends On | Blocks | Can Parallelize With |
|------|------------|--------|---------------------|
| 7A-1 | None | 7A-2, 7A-7 | 7A-4, 7A-5 |
| 7A-2 | 7A-1 | 7A-3, 7A-7 | 7A-6, 7B-2, 7B-3 |
| 7A-3 | 7A-2 | 7A-7 | 7B-4 |
| 7A-4 | None | 7A-7 | 7A-1, 7A-5 |
| 7A-5 | None | 7A-6 | 7A-1, 7A-4 |
| 7A-6 | 7A-5 | 7A-7 | 7A-2, 7B-2, 7B-3 |
| 7A-7 | 7A-1~7A-6 | 7B-1 | None |
| 7B-1 | 7A-7, 7B-2~7B-4 | 7B-5 | None |
| 7B-2 | None | 7B-1 | 7A-*, 7B-3 |
| 7B-3 | None | 7B-1 | 7A-*, 7B-2 |
| 7B-4 | 7B-3 | 7B-1 | 7A-3 |
| 7B-5 | 7B-1 | None | None |

---

## TODOs

### ═══════════════════════════════════════════
### PHASE 7A: Creation Infrastructure (作成インフラ)
### ═══════════════════════════════════════════
###
### Sibylに「新しいコードを作成する能力」を与える。
### 既存の「修正」能力を「作成＋修正」に拡張する。
### ═══════════════════════════════════════════

- [x] 7A-1. `add-definition` ツール — 既存ファイルに新定義を追加

  **What to do**:
  - 既存の.lispファイルに新しいdefun/defmethod/defclass/deftool等の定義を追加するツール
  - **戦略**: ファイル末尾（最後のフォームの後）に新定義を追記
    1. `read-sexp`でファイルの最後のフォームの終了行を取得
    2. 新定義のS式をバリデーション（`read-from-string`で構文チェック）
    3. ファイルの末尾に新定義テキストを追記
    4. `eval-form`で新定義をイメージに登録（auto-compile含む）
    5. Condition Systemで`undo-addition`リスタートを提供（追記を取り消し）
  - パラメータ: `file` (必須, ファイルパス), `definition` (必須, S-expression文字列), `after` (オプション, 既存定義名の後に挿入)
  - `after`パラメータ指定時: 指定定義の直後に挿入（関連する定義をまとめる）
  - **重要**: sync-to-fileとは異なり「新規追加」専用。既存定義の上書きは拒否

  **TDD**:
  - RED: テスト用ファイルに`add-definition`で新defunを追加 → defunが動作することを確認するテスト
  - RED: 不正なS式を渡したらtool-errorがシグナルされるテスト
  - RED: 追記後にファイルの既存内容が保持されていることを確認するテスト
  - GREEN: ファイル末尾追記 + eval-form + バリデーション実装
  - REFACTOR: `after`パラメータ対応

  **Must NOT do**:
  - 既存定義の上書き（それはsync-to-fileの役割）
  - パッケージ宣言の変更（それはadd-exportの役割）
  - sibyl.*パッケージ以外のファイルへの書き込み

  **Recommended Agent Profile**:
  - **Category**: `ultrabrain`
    - Reason: ファイル操作、S式バリデーション、Condition System統合の複合タスク
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with 7A-4, 7A-5)
  - **Blocks**: [7A-2, 7A-7]
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `src/tools/lisp-tools.lisp:734-796` — `sync-to-file`の実装。行範囲ベースのファイル編集パターン
  - `src/tools/lisp-tools.lisp:637-680` — `safe-redefine`のCondition System + ロールバックパターン
  - `src/tools/lisp-tools.lisp:1-50` — `read-sexp`のファイル読み込みとフォーム位置取得

  **API/Type References**:
  - `src/tools/protocol.lisp:51-79` — `deftool`マクロパターン
  - `src/conditions.lisp` — Condition階層。新しいcondition追加が必要な場合

  **Test References**:
  - `tests/sexp-tools-test.lisp` — sync-to-file-testsパターン。テンポラリファイルの使用方法

  **Acceptance Criteria**:
  - [ ] テスト用ファイルに新defunが追加される
  - [ ] 追加後にeval-formで関数が呼び出せる
  - [ ] 既存の内容（コメント含む）が保持される
  - [ ] 不正なS式でtool-errorがシグナルされる
  - [ ] `undo-addition`リスタートでファイルが元に戻る
  - [ ] `(fiveam:run 'add-definition-tests)` → PASS

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: add-definition appends new function to existing file
    Tool: Bash (sbcl --eval)
    Preconditions: Sibyl loaded, temp file with existing defun
    Steps:
      1. Create temp file with (defun existing-fn () 1)
      2. (sibyl.tools:execute-tool "add-definition"
           '(("file" . "/tmp/test-add.lisp")
             ("definition" . "(defun new-fn () 42)")))
      3. Assert: file contains both existing-fn and new-fn
      4. (load "/tmp/test-add.lisp")
      5. Assert: (new-fn) => 42
      6. Assert: (existing-fn) => 1
    Expected Result: New function added, existing preserved
    Evidence: File content + SBCL stdout

  Scenario: add-definition rejects invalid S-expression
    Tool: Bash (sbcl --eval)
    Steps:
      1. (handler-case
           (sibyl.tools:execute-tool "add-definition"
             '(("file" . "/tmp/test-add.lisp")
               ("definition" . "(defun incomplete")))
           (sibyl.conditions:tool-error (e) "CAUGHT"))
      2. Assert: returns "CAUGHT"
    Expected Result: tool-error signaled
    Evidence: SBCL stdout
  ```

  **Commit**: YES
  - Message: `feat(tools): add add-definition tool for appending new definitions to files`
  - Files: `src/tools/lisp-tools.lisp`, `tests/sexp-tools-test.lisp`
  - Pre-commit: `(asdf:test-system :sibyl)`

---

- [x] 7A-2. `create-module` ツール — 新Lispファイル作成

  **What to do**:
  - 新しい.lispソースファイルを作成するツール
  - 作成内容:
    1. パッケージ宣言（`in-package`）
    2. オプションでファイルヘッダコメント
    3. 初期定義（パラメータで指定、またはスケルトン）
  - ファイル作成後に`eval-form`でコンパイル
  - パラメータ: `path` (必須, "src/tools/new-tool.lisp"形式), `package` (必須, パッケージ名), `header-comment` (オプション), `initial-definitions` (オプション, S式文字列のリスト)
  - **重要**: ASDFへの登録は別ツール（7A-3）で行う。このツールはファイル作成のみ
  - Condition Systemで`remove-module`リスタート（ファイル削除）

  **TDD**:
  - RED: `create-module`で新ファイル作成 → ファイルが存在し、in-packageが含まれるテスト
  - RED: 既存ファイルパスで実行したらtool-errorテスト
  - GREEN: ファイル生成 + in-package + 初期定義
  - REFACTOR: ヘッダコメント、スケルトン生成

  **Must NOT do**:
  - `src/`以外のディレクトリへのファイル作成
  - sibyl.*以外のパッケージ指定
  - ASDF登録（それは7A-3の役割）

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: ファイル生成ロジック + バリデーション + Condition System
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO (after 7A-1)
  - **Parallel Group**: Wave 2 (with 7A-6, 7B-2, 7B-3)
  - **Blocks**: [7A-3, 7A-7]
  - **Blocked By**: [7A-1]

  **References**:

  **Pattern References**:
  - `src/tools/builtin.lisp:19-32` — `write-file`ツール。ファイル書き込みパターン
  - `src/tools/lisp-tools.lisp:1-8` — 既存lisp-tools.lispのファイルヘッダ。参考フォーマット
  - `src/packages.lisp:1-10` — パッケージ宣言パターン

  **Acceptance Criteria**:
  - [ ] 新ファイルが指定パスに作成される
  - [ ] ファイルに正しいin-package宣言がある
  - [ ] 初期定義が含まれている場合、それらがコンパイルされる
  - [ ] 既存ファイルパスでtool-errorがシグナルされる
  - [ ] `(fiveam:run 'create-module-tests)` → PASS

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: create-module creates new Lisp file
    Tool: Bash (sbcl --eval)
    Steps:
      1. (sibyl.tools:execute-tool "create-module"
           '(("path" . "/tmp/test-module.lisp")
             ("package" . "SIBYL.TOOLS")
             ("header-comment" . "Test module")))
      2. Assert: file /tmp/test-module.lisp exists
      3. Assert: file contains "(in-package #:sibyl.tools)"
      4. Cleanup: delete /tmp/test-module.lisp
    Expected Result: Lisp file with proper package header
    Evidence: File content

  Scenario: create-module rejects existing file path
    Tool: Bash (sbcl --eval)
    Steps:
      1. Create /tmp/existing.lisp
      2. (handler-case (sibyl.tools:execute-tool "create-module"
             '(("path" . "/tmp/existing.lisp") ("package" . "SIBYL")))
           (sibyl.conditions:tool-error (e) "CAUGHT"))
      3. Assert: "CAUGHT"
    Expected Result: tool-error for existing file
    Evidence: SBCL stdout
  ```

  **Commit**: YES
  - Message: `feat(tools): add create-module tool for new Lisp file creation`

---

- [x] 7A-3. ASDF動的登録 — `register-in-asdf` ツール

  **What to do**:
  - 新ファイルをsibyl.asdに登録し、ASDFシステム定義をリロードするツール
  - **戦略**:
    1. sibyl.asdを読み込み、適切なモジュールの`:components`リストを特定
    2. 新しい`(:file "filename")`エントリを追加（テキストレベルで挿入）
    3. ファイルを書き戻し
    4. `(asdf:clear-system :sibyl)`で古い定義をクリア
    5. `(asdf:find-system :sibyl t)`で再読み込み
    6. 新ファイルが認識されることを確認
  - パラメータ: `file` (必須, "new-tool"形式), `module` (必須, "tools"等のモジュール名), `after` (オプション, 挿入位置の指定)
  - Condition Systemで`undo-registration`リスタート（asdファイルを元に戻す）
  - **重要**: 追加のみ。既存コンポーネントの変更・削除は禁止

  **TDD**:
  - RED: ダミーファイルをASDFに登録 → `asdf:find-component`で見つかるテスト
  - GREEN: テキスト挿入 + clear-system + find-system
  - REFACTOR: モジュール自動検出

  **Must NOT do**:
  - 既存のASDFコンポーネントの削除や変更
  - テストシステム（sibyl/tests）の変更（テストファイルの登録は別フロー）
  - 直接的なASDF内部APIの使用（公開APIのみ）

  **Recommended Agent Profile**:
  - **Category**: `ultrabrain`
    - Reason: ASDFシステム定義の構造理解、テキスト挿入のパース、ASDF reload制御
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 3 (with 7B-4)
  - **Blocks**: [7A-7]
  - **Blocked By**: [7A-2]

  **References**:

  **Pattern References**:
  - `sibyl.asd:1-52` — ASDFシステム定義の構造。コンポーネントリストの位置
  - `src/system/asdf-protection.lisp` — ASDF interaction パターン

  **External References**:
  - ASDF Manual: `clear-system`, `find-system`, `find-component`

  **Acceptance Criteria**:
  - [ ] 新ファイルがsibyl.asdに追加される
  - [ ] `(asdf:find-component :sibyl '("tools" "new-file"))` → non-NIL
  - [ ] 追加後に`(ql:quickload :sibyl)`が成功する
  - [ ] `undo-registration`でsibyl.asdが元に戻る
  - [ ] `(fiveam:run 'asdf-registration-tests)` → PASS

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: register-in-asdf adds component to system
    Tool: Bash (sbcl --eval)
    Steps:
      1. Create dummy /tmp/sibyl-test-component.lisp
      2. (sibyl.tools:execute-tool "register-in-asdf"
           '(("file" . "sibyl-test-component") ("module" . "tools")))
      3. Assert: sibyl.asd contains (:file "sibyl-test-component")
      4. (asdf:find-component :sibyl '("tools" "sibyl-test-component")) → non-NIL
      5. Rollback: restore original sibyl.asd
    Expected Result: Component registered and findable
    Evidence: SBCL stdout + sibyl.asd content
  ```

  **Commit**: YES
  - Message: `feat(tools): add register-in-asdf for dynamic ASDF component registration`

---

- [x] 7A-4. `add-export` ツール — パッケージexport追加

  **What to do**:
  - packages.lispの指定パッケージの`:export`リストに新シンボルを追加するツール
  - **戦略**:
    1. packages.lispを読み込み
    2. 指定パッケージの`defpackage`フォームを特定（テキストレベル）
    3. `:export`セクションに新シンボルを追加
    4. ファイルを書き戻し
    5. `(export 'symbol (find-package :package))`で即座にexport
  - パラメータ: `package` (必須), `symbols` (必須, シンボル名のリスト)
  - 既にexportされているシンボルは無視（エラーにしない）
  - Condition Systemで`undo-export`リスタート

  **TDD**:
  - RED: テスト用シンボルをexport → `do-external-symbols`で確認テスト
  - GREEN: テキスト挿入 + runtime export
  - REFACTOR: 複数シンボル一括対応

  **Must NOT do**:
  - `:use`や`:import-from`の変更
  - sibyl.*パッケージ以外の変更

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: テキスト挿入 + export呼び出し
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with 7A-1, 7A-5)
  - **Blocks**: [7A-7]
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `src/packages.lisp:1-177` — パッケージ定義。export追加の対象
  - `src/tools/lisp-tools.lisp:734-796` — sync-to-fileのテキスト編集パターン

  **Acceptance Criteria**:
  - [ ] packages.lispの指定パッケージに新exportが追加される
  - [ ] ランタイムで`(find-symbol "NEW-SYMBOL" :sibyl.tools)` → :EXTERNAL
  - [ ] 既にexportされているシンボルでエラーにならない
  - [ ] `(fiveam:run 'add-export-tests)` → PASS

  **Commit**: YES (groups with 7A-1)
  - Message: `feat(tools): add add-export tool for dynamic package export management`

---

- [x] 7A-5. REPL動的ディスパッチ化 — case文をalist lookupにリファクタリング

  **What to do**:
  - **Metis指摘対応**: 現在のREPLコマンドディスパッチは`case`文（コンパイル時固定）
  - `*repl-commands*`のalist lookup方式に変更（ランタイム拡張可能に）
  - **現在の構造** (`src/repl.lisp:86`付近):
    ```lisp
    (case command
      (:help ...)
      (:tools ...)
      (:reset ...)
      (:history ...)
      (:improve ...)
      (:review ...)
      (:quit ...))
    ```
  - **変更後**:
    ```lisp
    (let ((handler (cdr (assoc command *repl-commands*))))
      (if handler
          (funcall handler agent original-input)
          (format t "Unknown command: ~a~%" command)))
    ```
  - `*repl-commands*`のaliast構造を `(command-keyword . handler-function)` に変更
  - 既存の全コマンドハンドラを個別関数に抽出

  **TDD**:
  - RED: 動的に追加されたコマンドがREPLで認識されるテスト
  - GREEN: case → alist lookup リファクタリング
  - REFACTOR: ハンドラ関数の整理

  **Must NOT do**:
  - 既存コマンドの挙動変更（リファクタリングのみ）
  - 新コマンドの追加（それは7A-6の役割）

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: 既存コードのリファクタリング + 後方互換性の維持
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 1 (with 7A-1, 7A-4)
  - **Blocks**: [7A-6]
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `src/repl.lisp:10-17` — `*repl-commands*` alist（現在の構造）
  - `src/repl.lisp:23-61` — `handle-repl-command` (リファクタリング対象)
  - `src/repl.lisp:86` — `case`ベースのディスパッチ（変更対象）
  - `src/repl.lisp:97-123` — メインループ

  **Test References**:
  - `tests/repl-test.lisp` — 既存のREPLテスト。全テストが引き続きパスすること

  **Acceptance Criteria**:
  - [ ] 既存のREPLコマンド（/help, /tools, /reset, /history, /improve, /review, /quit）が全て動作する
  - [ ] `*repl-commands*`にpushしたコマンドがディスパッチされる
  - [ ] 既存テストが全てパスする
  - [ ] `(fiveam:run 'repl-dynamic-dispatch-tests)` → PASS

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: Dynamic command dispatch works for existing commands
    Tool: Bash (sbcl --eval)
    Steps:
      1. (ql:quickload :sibyl)
      2. Verify *repl-commands* contains :help, :tools, :reset, :history, :improve, :review, :quit
      3. Verify each command has a handler function
    Expected Result: All commands registered with callable handlers
    Evidence: SBCL stdout

  Scenario: Dynamically added command is dispatched
    Tool: Bash (sbcl --eval)
    Steps:
      1. (push (cons :test-cmd (lambda (agent input) (declare (ignore agent input)) "TEST OK"))
              sibyl.repl::*repl-commands*)
      2. Verify :test-cmd is found in *repl-commands*
      3. (funcall (cdr (assoc :test-cmd sibyl.repl::*repl-commands*)) nil nil)
      4. Assert: returns "TEST OK"
    Expected Result: Dynamic command works
    Evidence: SBCL stdout
  ```

  **Commit**: YES
  - Message: `refactor(repl): convert command dispatch from case to dynamic alist lookup`
  - Files: `src/repl.lisp`, `tests/repl-test.lisp`
  - Pre-commit: `(asdf:test-system :sibyl)`

---

- [x] 7A-6. `register-command` ツール — REPLコマンド動的登録

  **What to do**:
  - 新しいREPLコマンドを動的に登録するツール
  - 7A-5のリファクタリング後の`*repl-commands*` alistに新コマンドを追加
  - パラメータ: `name` (必須, "evolve"等), `description` (必須, ヘルプテキスト), `handler-body` (必須, ラムダ式のS式文字列)
  - ハンドラをeval-formで評価 → *repl-commands*にpush
  - オプションでrepl.lispにも永続化（sync-to-file的にハンドラ追記）
  - パラメータ: `persist` (オプション, デフォルトnil)

  **TDD**:
  - RED: `register-command`で新コマンド登録 → `*repl-commands*`に含まれるテスト
  - GREEN: eval-form + alist push実装
  - REFACTOR: ヘルプテキスト統合

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
    - Reason: eval-form + alist操作の比較的シンプルなラッパー
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO (after 7A-5)
  - **Parallel Group**: Wave 2 (with 7A-2, 7B-2, 7B-3)
  - **Blocks**: [7A-7]
  - **Blocked By**: [7A-5]

  **References**:

  **Pattern References**:
  - Task 7A-5のリファクタリング結果 — 新しいalist構造
  - `src/repl.lisp:10-17` — *repl-commands*の構造

  **Acceptance Criteria**:
  - [ ] 登録したコマンドが`*repl-commands*`に含まれる
  - [ ] ハンドラがfuncall可能
  - [ ] /helpに新コマンドが表示される
  - [ ] `(fiveam:run 'register-command-tests)` → PASS

  **Commit**: YES
  - Message: `feat(tools): add register-command for dynamic REPL command registration`

---

- [x] 7A-7. 作成能力の統合テスト — 新モジュールE2E作成

  **What to do**:
  - Phase 7Aの全ツールを使って、新しいモジュールを一から作成するE2Eテスト
  - フロー:
    1. `create-module`で新ファイル`src/tools/test-evolution-module.lisp`を作成
    2. `add-definition`で新関数を追加
    3. `add-export`でsibyl.toolsにexport追加
    4. `register-in-asdf`でASDFに登録
    5. `(ql:quickload :sibyl)`が成功することを確認
    6. 新関数が呼び出せることを確認
    7. クリーンアップ（作成物を削除して元に戻す）

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: 全ツールを統合したE2Eテスト。自律的にフロー全体を検証
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 4 (sequential, after all 7A tasks)
  - **Blocks**: [7B-1]
  - **Blocked By**: [7A-1 ~ 7A-6]

  **References**:
  - Tasks 7A-1 ~ 7A-6の全ツール

  **Acceptance Criteria**:
  - [ ] 新モジュールが作成・登録・コンパイル・実行できる
  - [ ] クリーンアップ後にシステムが元の状態に戻る
  - [ ] 全テストがパスする
  - [ ] `(fiveam:run 'creation-integration-tests)` → PASS

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: Full module creation workflow E2E
    Tool: interactive_bash (sbcl REPL)
    Steps:
      1. (ql:quickload :sibyl)
      2. (sibyl.tools:execute-tool "create-module"
           '(("path" . "src/tools/test-evolution-module.lisp")
             ("package" . "SIBYL.TOOLS")))
      3. (sibyl.tools:execute-tool "add-definition"
           '(("file" . "src/tools/test-evolution-module.lisp")
             ("definition" . "(defun evolution-test-fn () :evolution-works)")))
      4. (sibyl.tools:execute-tool "add-export"
           '(("package" . "SIBYL.TOOLS")
             ("symbols" . "evolution-test-fn")))
      5. (sibyl.tools:execute-tool "register-in-asdf"
           '(("file" . "test-evolution-module") ("module" . "tools")))
      6. Assert: (sibyl.tools:evolution-test-fn) => :EVOLUTION-WORKS
      7. Cleanup: remove file, restore sibyl.asd, restore packages.lisp
    Expected Result: Complete module creation workflow succeeds
    Evidence: REPL output captured
  ```

  **Commit**: YES
  - Message: `test(tools): add E2E integration test for module creation workflow`

---

### ═══════════════════════════════════════════
### PHASE 7B: Autonomous Evolution Loop (自律進化ループ)
### ═══════════════════════════════════════════
###
### Sibylが `/evolve` で継続的自己改善サイクルを回す。
### self-assess → improvement-plan → implement → verify の自動ループ。
### ═══════════════════════════════════════════

- [x] 7B-1. `/evolve` コマンド — 自律継続改善ループ

  **What to do**:
  - REPLに`/evolve`コマンドを追加（7A-6の`register-command`で動的登録、またはrepl.lispに直接実装）
  - **ループフロー**:
    1. `self-assess`でcurrent stateを取得
    2. `suggest-improvements`で改善提案を取得
    3. 高・中優先度の提案をフィルタリング
    4. 提案が無ければ停止
    5. 各提案に対してTDDワークフローを実行:
       a. `write-test`でテスト作成（RED）
       b. `run-tests`で失敗確認
       c. 実装（`safe-redefine` or `add-definition`）
       d. `run-tests`で成功確認（GREEN）
       e. `sync-to-file`で永続化
    6. 全テストスイート実行で回帰チェック
    7. 失敗した改善はスキップ（ログに記録）
    8. 既存テストが壊れたら即停止
    9. サイクル完了後、1に戻る
  - パラメータ: `max-cycles` (オプション, デフォルト10), `priority-threshold` (オプション, デフォルト"medium")
  - **停止条件**（Metis指摘を反映）:
    - 高・中優先度の提案が無い
    - max-cyclesに到達
    - 既存テストが失敗（安全停止）
    - 連続3回スキップ（全提案が実装不能）
  - 各サイクルの結果をフォーマットして表示

  **TDD**:
  - RED: `/evolve`コマンドがREPLで認識されるテスト
  - RED: 空の改善提案でループが即停止するテスト
  - GREEN: ループ実装 + 停止条件
  - REFACTOR: エラーハンドリング最適化

  **Must NOT do**:
  - system-promptの自動変更
  - max-cycles無しの無限ループ
  - テスト失敗の無視

  **Recommended Agent Profile**:
  - **Category**: `ultrabrain`
    - Reason: 複数ツールのオーケストレーション、複雑な停止条件ロジック、LLM統合
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 5 (sequential)
  - **Blocks**: [7B-5]
  - **Blocked By**: [7A-7, 7B-2, 7B-3, 7B-4]

  **References**:

  **Pattern References**:
  - `src/repl.lisp:63-82` — `/improve`コマンドのハンドラ。エージェントへの指示パターン
  - `src/agent/core.lisp:192-209` — `agent-run`。エージェントループの呼び出し方
  - Phase 5のsuggest-improvements → /review → /improve フロー

  **API/Type References**:
  - `src/tools/lisp-tools.lisp:1801-1890` — self-assess出力構造
  - `src/tools/lisp-tools.lisp:2077-2186` — improvement-plan出力構造

  **Acceptance Criteria**:
  - [ ] `/evolve`コマンドがREPLで認識される
  - [ ] 改善提案がある限りサイクルが継続する
  - [ ] 高・中優先度の提案が無くなったら停止する
  - [ ] max-cyclesで強制停止する
  - [ ] テスト失敗時に即座に停止する
  - [ ] 各サイクルの結果が表示される
  - [ ] `(fiveam:run 'evolve-tests)` → PASS

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: /evolve stops when no high/medium suggestions
    Tool: interactive_bash (sbcl REPL)
    Steps:
      1. Start Sibyl REPL
      2. /evolve max-cycles=1
      3. Observe: self-assess runs
      4. Observe: suggest-improvements runs
      5. Observe: improvements are attempted or "no suggestions" message
      6. Assert: Loop terminates (doesn't hang)
    Expected Result: Clean termination with summary
    Evidence: REPL output captured

  Scenario: /evolve stops on test failure
    Tool: Bash (sbcl --eval)
    Steps:
      1. Verify evolve-tests include test for safety stop
      2. (fiveam:run 'evolve-tests)
      3. Assert: all pass
    Expected Result: Safety mechanisms tested
    Evidence: SBCL stdout
  ```

  **Commit**: YES
  - Message: `feat(repl): add /evolve command for autonomous continuous improvement`

---

- [x] 7B-2. `suggest-improvements` の強化 — 優先度フィルタリングと差分検出

  **What to do**:
  - **Metis指摘対応**: suggest-improvementsは低優先度の提案（docstring不足等）を常に返すため、「提案が空」にならない
  - 強化内容:
    1. 優先度フィルタリング: `min-priority`パラメータ追加（"high", "medium", "low"）
    2. 差分検出: 前回のサイクルと同じ提案を検出し、「新しい提案なし」を返す
    3. 提案の分類改善: 実装可能度（effort: low/medium/high）を追加
    4. 既に試行済みの提案を除外（7B-3の進化状態管理と連携）
  - パラメータ追加: `min-priority` (オプション, デフォルト"low"), `exclude-attempted` (オプション, デフォルトnil)

  **TDD**:
  - RED: `min-priority="high"`でフィルタリングされるテスト
  - RED: 同じスコープで2回呼んだ時に差分が検出されるテスト
  - GREEN: フィルタリング + 差分検出実装
  - REFACTOR: パフォーマンス改善

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with 7A-2, 7A-6, 7B-3)
  - **Blocks**: [7B-1]
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `src/tools/lisp-tools.lisp` — 既存のsuggest-improvements実装
  - `src/repl.lisp` — `*pending-suggestions*`の管理

  **Acceptance Criteria**:
  - [ ] `min-priority="high"`で低・中優先度の提案が除外される
  - [ ] 同じスコープで再実行時に前回と同じ提案が「既知」としてマークされる
  - [ ] `(fiveam:run 'suggest-improvements-enhanced-tests)` → PASS

  **Commit**: YES
  - Message: `feat(tools): enhance suggest-improvements with priority filtering and delta detection`

---

- [x] 7B-3. 進化状態管理 — `*evolution-state*`

  **What to do**:
  - `/evolve`サイクルの状態を管理する仕組み
  - **状態データ**:
    - 現在のサイクル番号
    - 試行済み改善のリスト（成功/失敗/スキップ）
    - 各改善の実行結果（テスト結果、エラー情報）
    - 開始時のテスト数とツール数（ベースライン）
    - 変更されたファイルのリスト
  - `*evolution-state*`特殊変数に格納
  - セッション間永続化: `.sibyl/evolution-log.json`に保存
  - ツール: `evolution-status`で現在の状態を確認

  **TDD**:
  - RED: 状態の作成・更新・永続化テスト
  - GREEN: plist/hash-table + JSONシリアライズ
  - REFACTOR: ログローテーション

  **Recommended Agent Profile**:
  - **Category**: `unspecified-low`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: YES
  - **Parallel Group**: Wave 2 (with 7A-2, 7A-6, 7B-2)
  - **Blocks**: [7B-4, 7B-1]
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `src/repl.lisp` — `*pending-suggestions*`の状態管理パターン

  **Acceptance Criteria**:
  - [ ] 進化状態の作成・更新・読み取りが動作する
  - [ ] JSONへの永続化と復元が動作する
  - [ ] `(fiveam:run 'evolution-state-tests)` → PASS

  **Commit**: YES (groups with 7B-2)
  - Message: `feat(agent): add evolution state management for /evolve tracking`

---

- [x] 7B-4. 進化レポート — 進捗表示とサマリー

  **What to do**:
  - `/evolve`サイクルの進捗をリアルタイム表示する仕組み
  - 表示内容:
    - サイクル番号 / max-cycles
    - 現在の改善提案名と種類
    - TDDステップ（RED/GREEN/REFACTOR/PERSIST）
    - 成功/失敗/スキップのカウント
    - 最終サマリー（何を改善したか、テスト数の変化、ツール数の変化）
  - フォーマット例:
    ```
    === Evolution Cycle 1/10 ===
    [1/3] Improving: Add test for agent-run function
          Step: RED (writing test)... GREEN (implementing)... PERSIST (syncing)... ✓
    [2/3] Improving: Add docstring to config-value
          Step: RED... GREEN... PERSIST... ✓
    [3/3] Improving: Error handling in shell tool
          Step: RED... GREEN... ✗ (test regression, skipped)
    
    Cycle 1 complete: 2 succeeded, 1 skipped
    Tests: 716 → 724 (+8)
    Tools: 20 → 20
    
    === Evolution Cycle 2/10 ===
    No new high/medium priority suggestions. Stopping.
    
    === Evolution Summary ===
    Cycles: 2 (1 productive, 1 empty)
    Improvements: 2 succeeded, 1 skipped
    Tests: 716 → 724 (+8)
    ```

  **Recommended Agent Profile**:
  - **Category**: `quick`
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO (partial dependency)
  - **Parallel Group**: Wave 3 (with 7A-3)
  - **Blocks**: [7B-1]
  - **Blocked By**: [7B-3]

  **Acceptance Criteria**:
  - [ ] サイクル進捗が表示される
  - [ ] 最終サマリーにテスト数・ツール数の変化が含まれる
  - [ ] `(fiveam:run 'evolution-report-tests)` → PASS

  **Commit**: YES (groups with 7B-3)
  - Message: `feat(agent): add evolution progress reporting and summary`

---

- [x] 7B-5. 自律進化デモンストレーション — `/evolve`の実行

  **What to do**:
  - `/evolve`を実際に実行し、Sibylが自律的に改善を行うことを実証
  - **デモ条件**:
    1. `/evolve max-cycles=3`を実行
    2. 最低1つの改善が成功する
    3. テストが増加する
    4. サイクルが適切に停止する
  - **記録**: 実行ログを`.sibyl/evolution-demo.log`に保存

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: 自律的なE2Eデモ実行
  - **Skills**: []

  **Parallelization**:
  - **Can Run In Parallel**: NO
  - **Parallel Group**: Wave 6 (final)
  - **Blocks**: None
  - **Blocked By**: [7B-1]

  **Acceptance Criteria**:
  - [ ] `/evolve`が実行され、少なくとも1つの改善が成功
  - [ ] テスト数が増加している
  - [ ] サイクルが停止条件で正常終了
  - [ ] 全テストがパスする

  **Agent-Executed QA Scenarios**:

  ```
  Scenario: /evolve executes autonomous improvement cycle
    Tool: interactive_bash (sbcl REPL)
    Preconditions: Full Phase 7A + 7B tools loaded
    Steps:
      1. Start Sibyl REPL
      2. Input: /evolve max-cycles=3
      3. Observe: self-assess runs
      4. Observe: suggest-improvements returns proposals
      5. Observe: TDD cycle executes for each proposal
      6. Observe: Cycle completes with summary
      7. Run: (asdf:test-system :sibyl)
      8. Assert: all tests pass (including new ones)
      9. Assert: test count > 716
    Expected Result: Autonomous evolution with measurable improvements
    Evidence: Full REPL session captured, test results
  ```

  **Commit**: YES
  - Message: `milestone(autonomous): Sibyl autonomously evolves via /evolve command`

---

## Commit Strategy

| After Task | Message | Verification |
|------------|---------|--------------|
| 7A-1, 7A-4 | `feat(tools): add add-definition and add-export creation tools` | `(asdf:test-system :sibyl)` |
| 7A-5 | `refactor(repl): convert command dispatch to dynamic alist lookup` | `(asdf:test-system :sibyl)` |
| 7A-2, 7A-6 | `feat(tools): add create-module and register-command tools` | `(asdf:test-system :sibyl)` |
| 7A-3 | `feat(tools): add register-in-asdf for dynamic ASDF registration` | `(asdf:test-system :sibyl)` |
| 7A-7 | `test(tools): E2E module creation integration test` | E2E test pass |
| 7B-2, 7B-3, 7B-4 | `feat(agent): add evolution loop infrastructure` | `(asdf:test-system :sibyl)` |
| 7B-1 | `feat(repl): add /evolve autonomous improvement command` | `(asdf:test-system :sibyl)` |
| 7B-5 | `milestone(autonomous): Sibyl autonomously evolves via /evolve` | E2E demo |

---

## Success Criteria

### Phase Gate Criteria

| Phase | Gate | Verification |
|-------|------|-------------|
| 7A | 新モジュールをE2Eで作成できる | create-module → add-definition → ASDF登録 → コンパイル成功 |
| 7B | `/evolve`で自律改善サイクルが回る | /evolve → 改善実行 → テスト増加 → 正常停止 |

### Final Checklist
- [x] 全テストがパスする: `(asdf:test-system :sibyl)` → 1040 checks, 100%
- [x] 新ファイルを作成しASDFに登録できる (create-module + register-in-asdf)
- [x] 既存ファイルに新定義を追加できる (add-definition)
- [x] パッケージexportを動的に追加できる (add-export)
- [x] REPLコマンドを動的に登録できる (register-command)
- [x] `/evolve`で継続的改善サイクルが自律実行される
- [x] 改善枯渇で適切に停止する (3 consecutive skip cycles)
- [x] テスト失敗時に安全停止する
