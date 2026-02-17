# Sibyl 設計ビジョン：S式が可能にする自己進化

> 作成日: 2025
> ステータス: 設計思想

---

## 命題

自己進化するエージェントには**ホモイコニシティ**が不可欠である。

コードとデータが同じ形をしていなければ、エージェントは自分自身を読めず、書けず、書き換えられない。Sibyl が Common Lisp で書かれているのはそのためだ。

```
ユーザー ⟷ Sibyl ⟷ プロダクト
              ↕
         自己進化（すべてS式で記述されているから可能）
```

---

## S式の一貫性

Sibyl のすべての層が同じ表現を使う：

| 層 | 実例 |
|----|------|
| システム定義 | `(asdf:defsystem :sibyl :components (...))` |
| 設定 | `((:llm.model "claude-sonnet-4-5") ...)` → `~/.sibyl/config.lisp` |
| ツール定義 | `(deftool "read-file" (:description ... :parameters ...) body)` |
| プラン永続化 | `(:plan (:id "plan-123" :title "..." :phases ...))` |
| エラー回復 | `(restart-case (execute-tool ...) (retry-tool () ...) (skip-tool () ...))` |
| 自己改変 | `(safe-redefine :name "fn" :new-definition "(defun fn ...)")` |
| 内省 | `(read-sexp :path "core.lisp")` → S式として構造を返す |

JSON や YAML ではコードとデータの間に変換層が挟まる。S式にはそれがない。ツール定義を読むことと、ツール定義を書くことと、ツール定義を実行することが、すべて同じ操作である。

---

## 二重の自己

```
┌───────────────────────┐
│  LLM (Claude)         │  ← 考える自己：S式を生成する
└─────────┬─────────────┘
          │ ツール呼び出し
┌─────────▼─────────────┐
│  Lisp (SBCL)          │  ← 動く自己：S式を実行する
└───────────────────────┘
```

S式が両者を橋渡しする：

- LLM が `(deftool ...)` を生成 → Lisp がそのまま `eval` → ツールが登録される
- LLM が `(defun ...)` を生成 → `safe-redefine` が評価・コンパイル・ロールバック可能な状態で適用
- LLM が `(:plan (:phases ...))` を生成 → `plan->sexp` / `sexp->plan` でそのまま永続化

考える自己と動く自己がS式という共通言語を持つことが自己進化の前提条件である。

---

## 5つの力

### 1. ツールはデータである

`deftool` マクロがツールを宣言的な構造体に変換し、グローバルレジストリに登録する。ツールのインターフェース（名前・説明・パラメータ）と実装（ハンドラ本体）が一つのS式に統合されている。

```lisp
(deftool "read-file"
    (:description "ファイルを読む"
     :parameters ((:name "path" :type "string" :required t
                   :description "ファイルパス")))
  (uiop:read-file-string (getf args :path)))
```

ツールは検査でき、直列化でき、実行時に生成できる。MCP サーバー接続時に外部ツールをこの同じレジストリに動的登録するのも同じ仕組みである。

### 2. プランは読めるコードである

プラン・フェーズ・タスクはすべて plist（S式）で、`~/.sibyl/plans/` にそのまま `.lisp` ファイルとして保存される。ORM も変換層もない。

```lisp
(:plan (:id "plan-xyz"
        :title "型チェックツール追加"
        :phases ((:id 1 :title "実装"
                  :tasks ((:id "t1" :title "deftool作成" :status :pending))))))
```

### 3. コンディションシステムが失敗を構造化する

ツール実行に `restart-case` を使う。失敗時に `retry-tool`（再試行）、`skip-tool`（スキップ）、`use-value`（代替値）を選べる。自己改変中にコンパイルが壊れたら `restore-definition` でロールバック。これは CL 固有の能力であり、自己書き換えを安全にする要である。

### 4. 内省がネイティブである

`read-sexp` はソースファイルをS式として構造的に読む。`describe-symbol` はシンボルの束縛・型・メソッドを返す。`who-calls` は呼び出し関係を辿る。`codebase-map` は全モジュール構造を返す。エージェントは自分のコードを**テキストではなく構造として**理解する。

### 5. コード生成が自然である

`write-test` ツールはテストコードをS式文字列として生成し、`eval-form` でその場で評価・コンパイルし、ファイルに永続化する。同じパターンで新しいツールもプランも生成できる。`format` + `eval` + ファイル I/O。これがホモイコニシティの実用的帰結である。

---

## 欠けているもの：3つの自己知識

| 次元 | 意味 | 現状 |
|------|------|------|
| 実行文脈 | 今どこで何を作っているか | ❌ |
| 能力 | 何ができ、何ができないか | △ |
| 状態 | 何をして、何を約束したか | ❌ |

---

## 進化のモード

### 摩擦駆動（Mode 1）

「できない」に遭遇した瞬間、ツールをS式で自作し、登録し、使う。

```lisp
;; エージェントが実行時に生成・評価するS式
(deftool "read-tsconfig"
    (:description "TypeScript設定を読む"
     :parameters ((:name "path" :type "string" :required t)))
  (yason:parse (uiop:read-file-string (getf args :path))))
```

### 振り返り駆動（Mode 2）

セッション終了時に不足能力をプランとして記録し、次回起動時に自動実行する。プランはS式だから、読んで、変更して、再保存できる。

### プロダクト駆動（Mode 3）

プロジェクト開始時に `read-sexp` と `codebase-map` でコードベースを分析し、必要な能力を特定して自己拡張する。

---

## 次の一手

`start-repl` にプロジェクトコンテキストを導入する：

```lisp
(sibyl:start-repl
  :client (sibyl:make-anthropic-client)
  :project "/path/to/product"
  :project-name "my-app")
```

これにより：

1. システムプロンプトにプロダクトの文脈が注入される
2. ツールの対象が明確になる（Sibyl 自身 vs プロダクト）
3. プランがプロジェクト単位で分離される
4. 進化の単位がセッションからプロジェクトに変わる

---

## 優先度

| 優先度 | 項目 | S式との接点 |
|--------|------|-------------|
| 🔴 高 | プロジェクトコンテキスト導入 | 文脈を plist で保持・注入 |
| 🔴 高 | Self/Product 境界の明確化 | `deftool` に `:target` スロット追加 |
| 🟡 中 | 摩擦駆動進化 | LLM が `deftool` S式を生成 → `eval` |
| 🟡 中 | 振り返り記録 | `plan->sexp` で能力ギャップを永続化 |
| 🟢 低 | プロダクト駆動進化 | `read-sexp` + `codebase-map` で自動分析 |
