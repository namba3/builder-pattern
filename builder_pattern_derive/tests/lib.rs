#![allow(dead_code)]

use builder_pattern_derive::Builder;
use std::sync::{
    Arc,
    atomic::{AtomicUsize, Ordering},
};

struct DropProbe(Arc<AtomicUsize>);

impl Drop for DropProbe {
    fn drop(&mut self) {
        self.0.fetch_add(1, Ordering::SeqCst);
    }
}

fn drop_probe(counter: &Arc<AtomicUsize>) -> DropProbe {
    DropProbe(Arc::clone(counter))
}

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

#[test]
fn custom_name_attribute_renames_the_setter() {
    #[derive(Builder)]
    struct S {
        #[builder(name = "set_value")]
        value: u8,
    }

    let s = S::builder().set_value(42).build();

    assert_eq!(s.value, 42);
}

#[test]
fn as_is_uses_normal_setters_for_special_types() {
    #[derive(Builder)]
    struct S {
        #[builder(as_is)]
        flag: bool,
        #[builder(as_is)]
        option: Option<u8>,
        #[builder(as_is)]
        values: Vec<u8>,
    }

    let s = S::builder()
        .flag(true)
        .option(Some(7))
        .values(vec![1, 2])
        .build();

    assert!(s.flag);
    assert_eq!(s.option, Some(7));
    assert_eq!(s.values, vec![1, 2]);
}

#[test]
fn each_attribute_appends_individual_values() {
    #[derive(Builder)]
    struct S {
        #[builder(each = "value")]
        values: Vec<u8>,
    }

    let s = S::builder().value(1).value(2).build();

    assert_eq!(s.values, vec![1, 2]);
}

#[test]
fn default_expression_is_used_unless_overridden() {
    #[derive(Builder)]
    struct S {
        #[builder(default = "2u8.pow(2)")]
        value: u8,
    }

    let defaulted = S::builder().build();
    let overridden = S::builder().value(9).build();

    assert_eq!(defaulted.value, 4);
    assert_eq!(overridden.value, 9);
}

#[test]
fn fixed_expression_initializes_a_non_settable_field() {
    #[derive(Builder)]
    struct S {
        #[builder(fixed = "2u8.pow(2)")]
        value: u8,
    }

    let s = S::builder().build();

    assert_eq!(s.value, 4);
}

#[test]
fn dropping_incomplete_builder_drops_initialized_values_once() {
    #[derive(Builder)]
    struct S {
        value: DropProbe,
        pending: String,
    }

    let drops = Arc::new(AtomicUsize::new(0));
    let builder = S::builder().value(drop_probe(&drops));

    drop(builder);

    assert_eq!(drops.load(Ordering::SeqCst), 1);
}

#[test]
fn built_struct_drops_each_required_value_once() {
    #[derive(Builder)]
    struct S {
        first: DropProbe,
        second: DropProbe,
    }

    let drops = Arc::new(AtomicUsize::new(0));
    let value = S::builder()
        .first(drop_probe(&drops))
        .second(drop_probe(&drops))
        .build();

    drop(value);

    assert_eq!(drops.load(Ordering::SeqCst), 2);
}

#[test]
fn generated_state_names_do_not_shadow_user_types() {
    #[allow(non_camel_case_types)]
    struct __BuilderState0(u8);

    #[derive(Builder)]
    struct S {
        value: __BuilderState0,
    }

    let s = S::builder().value(__BuilderState0(7)).build();

    assert_eq!(s.value.0, 7);
}

mod public_api {
    use super::Builder;

    #[derive(Builder)]
    pub struct PublicThing {
        pub value: u32,
    }
}

#[test]
fn public_struct_exposes_its_generated_builder_type() {
    let builder: public_api::PublicThingBuilder<_> = public_api::PublicThing::builder();
    let value = builder.value(42).build();

    assert_eq!(value.value, 42);
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
