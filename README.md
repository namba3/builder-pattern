# builder-pattern

[English](README.en.md) | 日本語

Builder パターンの実装を生成する derive マクロです。

このプロジェクトは Rust の derive マクロを書く練習用です。

## 使用例

```rust
use builder_pattern_derive::Builder;

#[derive(Builder)]
struct Person {
    id: u64,
    name: String,
    dead: bool,
    another_id: Option<u64>,
    nums: Vec<u64>,
}

let p = Person::builder()
    .id(1)
    .name("name".to_owned())
    .dead()
    .another_id(2)
    .nums([3, 4, 5])
    .nums([6, 7, 8])
    .build();

assert_eq!(p.id, 1);
assert_eq!(p.name, "name");
assert_eq!(p.dead, true);
assert_eq!(p.another_id, Some(2));
assert_eq!(p.nums, [3, 4, 5, 6, 7, 8]);
```

## コンパイル時の保証

特別なフィールドを除き、すべてのフィールドに値を設定する必要があり、同じフィールドを複数回設定することはできません。

次の例は、`b` フィールドが未設定のためコンパイルに失敗します。

```rust
#[derive(Builder)]
struct S {
    a: i64,
    b: i64,
}

S::builder()
    .a(1)
    .build(); // ここでコンパイルエラー
```

次の例は、`b` フィールドを2回設定しているためコンパイルに失敗します。

```rust
#[derive(Builder)]
struct S {
    a: i64,
    b: i64,
}

S::builder()
    .a(1)
    .b(2)
    .b(3) // ここでコンパイルエラー
    .build();
```

## 特別なフィールド

`bool`、`Option`、`Vec` のフィールドには特別な扱いがあります。

### `bool` フィールド

`bool` フィールドの初期値は `false` です。セッターは引数を取らず、呼び出すと値が `true` になります。

```rust
#[derive(Builder)]
struct S {
    flag: bool,
}

let s = S::builder().build();
assert_eq!(s.flag, false);

let s = S::builder().flag().build();
assert_eq!(s.flag, true);
```

### `Option` フィールド

`Option` フィールドの初期値は `None` です。セッターには `Option` の内側の型の値を渡します。

```rust
#[derive(Builder)]
struct S {
    opt: Option<u64>,
}

let s = S::builder().build();
assert_eq!(s.opt, None);

let s = S::builder().opt(1).build();
assert_eq!(s.opt, Some(1));
```

### `Vec` フィールド

`Vec` フィールドの初期値は空の `Vec` です。セッターには要素の値を渡します。セッターは何度でも呼び出せ、そのたびに要素が追加されます。

```rust
#[derive(Builder)]
struct S {
    nums: Vec<u64>,
}

let s = S::builder().build();
assert_eq!(s.nums, vec![]);

let s = S::builder().nums([1, 2, 3]).build();
assert_eq!(s.nums, vec![1, 2, 3]);

let s = S::builder().nums([1, 2, 3]).nums([4, 5, 6]).build();
assert_eq!(s.nums, vec![1, 2, 3, 4, 5, 6]);
```

## フィールド属性

### `name`

`name` 属性でセッター名を変更できます。

```rust
#[derive(Builder)]
struct S {
    #[builder(name = "set_a")]
    a: u64,
}

let s = S::builder().set_a(1).build();
assert_eq!(s.a, 1);
```

### `as_is`

`as_is` 属性を付けると、特別なフィールドも通常のフィールドとして扱います。

```rust
#[derive(Builder)]
struct S {
    #[builder(as_is)]
    bool: bool,
    #[builder(as_is)]
    option: Option<u64>,
    #[builder(as_is)]
    vec: Vec<u64>,
}

let s = S::builder()
    .bool(true)
    .option(None)
    .vec(vec![1, 2, 3])
    .build();
assert_eq!(s.bool, true);
assert_eq!(s.option, None);
assert_eq!(s.vec, vec![1, 2, 3]);
```

### `each`

`each` 属性を付けると、`Vec` フィールドに要素を1つずつ追加するセッターを生成します。

```rust
#[derive(Builder)]
struct S {
    #[builder(each = "num")]
    nums: Vec<u64>,
}

let s = S::builder()
    .num(1)
    .num(2)
    .num(3)
    .build();
assert_eq!(s.nums, vec![1, 2, 3]);
```

### `default`

`default` 属性でフィールドの初期値を指定できます。値だけでなく式も指定できます。

```rust
#[derive(Builder)]
struct S {
    #[builder(default = "2u64.pow(2)")]
    a: u64,
}

let s = S::builder().build();
assert_eq!(s.a, 4);

let s = S::builder().a(100).build();
assert_eq!(s.a, 100);
```

### `fixed`

`fixed` 属性でフィールドの値を固定できます。値だけでなく式も指定できます。

```rust
#[derive(Builder)]
struct S {
    #[builder(fixed = "2u64.pow(2)")]
    a: u64,
}

let s = S::builder().build();
assert_eq!(s.a, 4);
```

## ベンチマーク

nightly のベンチマーク機能を使わず、`std::time::Instant` と `std::hint::black_box` で計測します。release モードで実行してください。

```sh
cargo bench -p example --bench builder
```

計測回数は `BENCH_ITERS` で変更できます（既定値は `1_000_000`）。計測前に最大 10,000 回のウォームアップを行います。

```sh
BENCH_ITERS=100000 cargo bench -p example --bench builder
```
