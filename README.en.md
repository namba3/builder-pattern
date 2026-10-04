# builder-pattern

[English](README.en.md) | [日本語](README.md)

A derive macro generating an impl of the builder pattern.

This project is for my practice writing Rust's derive macro.

## Example

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

The generated `<StructName>Builder` type and `builder()` method have the same visibility as the original struct.
Generated code resolves the Cargo dependency name for `builder_pattern`, so the dependency can be renamed.
Named-field structs, including empty structs, are supported. Generic structs retain their type, lifetime, and const parameters and `where` clauses. Unit and tuple structs are not supported.

## Compile Guarantee

Ordinary required fields must be set exactly once; leaving one unset or setting it twice is a compile error. Setter call counts are:

| Field | Setter calls |
| --- | --- |
| Ordinary field | Exactly once |
| `bool` / `Option<T>` | Zero or once |
| `Vec<T>` | Zero or more |
| With `default` | Zero or once |
| With `fixed` | Zero (no setter is generated) |
| Special type with `as_is` and no `default` / `fixed` | Exactly once |

The setters for `bool` and `Option<T>` cannot be called more than once. A `Vec<T>` setter can be called repeatedly.
`as_is` makes a special type behave like an ordinary field. If `default` or `fixed` is also specified, its initialization rule applies.
Generated setter names must be unique, and `build` is reserved for the generated build method. Duplicate or reserved names are reported as derive errors.

This fails to compile because 'b' field has no value set.

```rust
#[derive(Builder)]
struct S {
    a: i64,
    b: i64,
}

S::builder()
    .a(1)
    .build(); // compile error here
```

And this fails to compile because 'b' field is set twice.

```rust
#[derive(Builder)]
struct S {
    a: i64,
    b: i64,
}

S::builder()
    .a(1)
    .b(2)
    .b(3) // compile error here
    .build();
```

## Special fields

`bool`, `Option`, and `Vec` fields are treated specially.
Detection is based on path spelling and does not expand type aliases. The recognized paths are `bool` / `core::primitive::bool` / `std::primitive::bool`, `Option` / `core::option::Option` / `std::option::Option`, and `Vec` / `std::vec::Vec`. Type aliases are treated as ordinary required fields.

### bool fields

`bool` fields are set false by default and the setter takes no arguments.

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

### Option fields

`Option` fields are set None by default and the setter takes the value of the Option's inner type.

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

### Vec fields

`Vec` fields are set empty vec by default and the setter takes the values of the Vec's inner type.
In Vec fields, you can call the setter as many times as you like, each time appending values to the vec.

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

## Field attributes

Apply `#[builder(...)]` to fields. Struct-level builder attributes are not supported.

Use one `#[builder(...)]` attribute per field and combine multiple settings inside it.

### name

`name` attribute changes the setter name.
The value must be a valid Rust method name. Use a raw identifier such as `r#type` for a keyword.

```rust
#[derive(Builder)]
struct S {
    #[builder(name = "set_a")]
    a: u64,
}

let s = S::builder().set_a(1).build();
assert_eq!(s.a, 1);
```

### as_is

`as_is` attribute treats the special fields as normal.

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

### each

`each` attribute generates a setter that adds each value to the Vec field one by one.
The value must be a valid Rust method name.

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

### default

`default` attribute sets a default value to the field.
You can use a Rust expression directly. The legacy string form remains supported. To use a string literal as the value, use a block to distinguish it from the legacy form (for example, `default = { let text = "text"; text }`). The same syntax applies to `fixed`.

```rust
#[derive(Builder)]
struct S {
    #[builder(default = 2u64.pow(2))]
    a: u64,
}

let s = S::builder().build();
assert_eq!(s.a, 4);

let s = S::builder().a(100).build();
assert_eq!(s.a, 100);
```

### fixed

`fixed` attribute sets a fixed value to the field.
You can specify not only the value but also the expression.

```rust
#[derive(Builder)]
struct S {
    #[builder(fixed = 2u64.pow(2))]
    a: u64,
}

let s = S::builder().build();
assert_eq!(s.a, 4);
```

## Benchmarks

The benchmark uses `std::time::Instant` and `std::hint::black_box` without nightly's benchmark feature. Run it in release mode.
It compares a builder with four required fields against a struct literal, and four `each` calls against four `Vec::push` calls. It also measures the default and setter paths for `bool`, `Option`, and `default` fields.

```sh
cargo bench -p example --bench builder
```

Set `BENCH_ITERS` to change measured iterations per sample (default: `1_000_000`). Each comparison runs 11 samples by default and reports the median, minimum, and maximum. The order of each pair alternates between samples. It warms up for up to 10,000 iterations before measuring.

```sh
BENCH_ITERS=100000 BENCH_SAMPLES=15 cargo bench -p example --bench builder
```

Set `BENCH_SAMPLES` to change the sample count.
