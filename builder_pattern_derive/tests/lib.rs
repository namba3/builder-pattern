#![allow(dead_code)]

use builder_pattern_derive::Builder;

#[test]
fn basic_usage() {
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
}

#[test]
fn bool_field_value_is_false_by_default() {
    #[derive(Builder)]
    struct S {
        bool: bool,
    }

    let s = S::builder().build();

    assert_eq!(s.bool, false);
}

#[test]
fn option_field_value_is_none_by_default() {
    #[derive(Builder)]
    struct S {
        option: Option<i64>,
    }

    let s = S::builder().build();

    assert_eq!(s.option, None);
}

#[test]
fn vec_field_value_is_empty_by_default() {
    #[derive(Builder)]
    struct S {
        vec: Vec<i64>,
    }

    let s = S::builder().build();

    assert!(s.vec.is_empty());
}

/// ```compile_fail
/// use builder_pattern_derive::Builder;
/// #[derive(Builder)]
/// struct S {
///     a: i64,
/// }
/// fn main() {
///     S::builder().build();
/// }
/// ```
fn should_fail_to_compile_when_no_field_value_is_given() {}

/// ```compile_fail
/// use builder_pattern_derive::Builder;
/// #[derive(Builder)]
/// struct S {
///     a: i64,
/// }
/// fn main() {
///     S::builder().a(1).a(2).build();
/// }
/// ```
fn should_fail_to_compile_when_value_is_given_more_than_once_for_the_same_field() {}
