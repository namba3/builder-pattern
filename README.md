# builder-pattern

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

## Compile Guarantee

Guarantees that all fields are set to a value and that the same field is never set more than once, except for the special fields.

This fails to compile because 'b' field has no value ​​set.

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

`bool`, `Option`, `Vec` fields are treated little specially.

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
In Vec fields, you can call the setter as many times as you like, each time appending values ​​to the vec.

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

### name

`name` attribute changes the setter name.

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
You can specify not only the value but also the expression.

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

### fixed

`fixed` attribute sets a fixed value to the field.
You can specify not only the value but also the expression.

```rust
#[derive(Builder)]
struct S {
    #[builder(fixed = "2u64.pow(2)")]
    a: u64,
}

let s = S::builder().build();
assert_eq!(s.a, 4);
```
