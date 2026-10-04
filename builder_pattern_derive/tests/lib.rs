#![allow(dead_code)]

use builder_pattern_derive::Builder;
use std::sync::{
    Arc, OnceLock,
    atomic::{AtomicUsize, Ordering},
};

mod standard_library_names_can_be_shadowed {
    #[allow(dead_code)]
    mod core {}
    #[allow(dead_code)]
    mod std {}

    use builder_pattern_derive::Builder;

    #[derive(Builder)]
    struct Fields {
        enabled: bool,
        optional: Option<u8>,
        values: Vec<u8>,
    }

    #[derive(Builder)]
    struct Items {
        #[builder(each = "item")]
        items: Vec<u8>,
    }

    #[test]
    fn generated_standard_library_paths_are_absolute() {
        let fields = Fields::builder()
            .enabled()
            .optional(7)
            .values([1, 2])
            .build();
        let items = Items::builder().item(3).items([4, 5]).build();

        assert!(fields.enabled);
        assert_eq!(fields.optional, Some(7));
        assert_eq!(fields.values, [1, 2]);
        assert_eq!(items.items, [3, 4, 5]);
    }
}

struct DropProbe(Arc<AtomicUsize>);

impl Drop for DropProbe {
    fn drop(&mut self) {
        self.0.fetch_add(1, Ordering::SeqCst);
    }
}

fn drop_probe(counter: &Arc<AtomicUsize>) -> DropProbe {
    DropProbe(Arc::clone(counter))
}

fn expression_drop_counter() -> &'static Arc<AtomicUsize> {
    static DROPS: OnceLock<Arc<AtomicUsize>> = OnceLock::new();
    DROPS.get_or_init(|| Arc::new(AtomicUsize::new(0)))
}

fn expression_drop_probe() -> DropProbe {
    drop_probe(expression_drop_counter())
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
    assert!(p.dead);
    assert_eq!(p.another_id, Some(2));
    assert_eq!(p.nums, [3, 4, 5, 6, 7, 8]);
}

#[test]
fn setters_can_be_called_in_any_order() {
    #[derive(Builder)]
    struct Record {
        id: u8,
        name: String,
        enabled: bool,
        optional: Option<u8>,
        values: Vec<u8>,
    }

    let record = Record::builder()
        .values([3, 4])
        .optional(2)
        .enabled()
        .name(String::from("record"))
        .id(1)
        .build();

    assert_eq!(record.id, 1);
    assert_eq!(record.name, "record");
    assert!(record.enabled);
    assert_eq!(record.optional, Some(2));
    assert_eq!(record.values, [3, 4]);
}

#[test]
fn empty_named_struct_can_be_built() {
    #[derive(Builder)]
    struct Empty {}

    let _value = Empty::builder().build();
}

#[test]
fn generic_struct_supports_lifetime_type_const_and_where_generics() {
    #[derive(Builder)]
    struct GenericData<'a, Iter, const N: usize>
    where
        Iter: Clone,
    {
        label: &'a str,
        values: [Iter; N],
        #[builder(each = "item")]
        items: Vec<Iter>,
        enabled: bool,
    }

    let data = GenericData::builder()
        .label("example")
        .values([String::from("first"), String::from("second")])
        .items([String::from("third")])
        .item(String::from("fourth"))
        .enabled()
        .build();

    assert_eq!(data.label, "example");
    assert_eq!(data.values, ["first", "second"]);
    assert_eq!(data.items, ["third", "fourth"]);
    assert!(data.enabled);
}

#[test]
fn generic_struct_supports_default_type_parameters() {
    #[derive(Builder)]
    struct GenericValue<T = u32> {
        value: T,
    }

    let defaulted: GenericValue = GenericValue::builder().value(42).build();
    let value = GenericValue::builder()
        .value(String::from("generic"))
        .build();

    assert_eq!(defaulted.value, 42);
    assert_eq!(value.value, "generic");
}

#[test]
fn generic_struct_does_not_require_clone_for_field_types() {
    struct NotClone(u8);

    #[derive(Builder)]
    struct GenericValue<T> {
        value: T,
    }

    let value = GenericValue::builder().value(NotClone(42)).build();

    assert_eq!(value.value.0, 42);
}

#[test]
fn generic_struct_supports_default_const_parameters() {
    #[derive(Builder)]
    struct FixedArray<const N: usize = 2> {
        values: [u8; N],
    }

    let defaulted = FixedArray::builder().values([1, 2]).build();
    let explicit = FixedArray::<3>::builder().values([3, 4, 5]).build();

    assert_eq!(defaulted.values, [1, 2]);
    assert_eq!(explicit.values, [3, 4, 5]);
}

#[test]
fn aliases_of_special_types_use_regular_field_setters() {
    type Maybe<T> = Option<T>;
    type Items<T> = Vec<T>;

    #[derive(Builder)]
    struct AliasedFields {
        optional: Maybe<u8>,
        values: Items<u8>,
    }

    let value = AliasedFields::builder()
        .optional(Some(7))
        .values(vec![1, 2, 3])
        .build();

    assert_eq!(value.optional, Some(7));
    assert_eq!(value.values, [1, 2, 3]);
}

#[test]
fn absolute_paths_of_special_types_keep_special_behavior() {
    #[derive(Builder)]
    struct AbsolutePaths {
        enabled: ::core::primitive::bool,
        value: ::std::option::Option<u8>,
        values: ::std::vec::Vec<u8>,
    }

    let value = AbsolutePaths::builder()
        .enabled()
        .value(7)
        .values([1, 2])
        .values([3])
        .build();

    assert!(value.enabled);
    assert_eq!(value.value, Some(7));
    assert_eq!(value.values, [1, 2, 3]);
}

#[test]
fn special_field_types_can_be_nested() {
    #[derive(Builder)]
    struct Nested {
        optional_values: Option<Vec<u8>>,
        optional_items: Vec<Option<u8>>,
    }

    let value = Nested::builder()
        .optional_values(vec![1, 2])
        .optional_items([Some(3), None])
        .build();

    assert_eq!(value.optional_values, Some(vec![1, 2]));
    assert_eq!(value.optional_items, [Some(3), None]);
}

#[test]
fn derive_expansion_resolves_hyphenated_runtime_dependency_alias() {
    #[derive(Builder)]
    struct S {
        value: u8,
    }

    let value = S::builder().value(42).build();

    assert_eq!(value.value, 42);
}

#[test]
fn bool_field_value_is_false_by_default() {
    #[derive(Builder)]
    struct S {
        bool: bool,
    }

    let s = S::builder().build();

    assert!(!s.bool);
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
fn legacy_name_attribute_syntax_renames_the_setter() {
    #[derive(Builder)]
    struct S {
        #[builder = "set_value"]
        value: u8,
    }

    let s = S::builder().set_value(42).build();

    assert_eq!(s.value, 42);
}

#[test]
fn custom_setter_name_can_use_a_raw_keyword_identifier() {
    #[derive(Builder)]
    struct S {
        #[builder(name = "r#type")]
        value: u8,
    }

    let s = S::builder().r#type(7).build();

    assert_eq!(s.value, 7);
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
fn as_is_special_types_can_use_defaults_and_be_overridden() {
    #[derive(Builder)]
    struct S {
        #[builder(as_is, default = false)]
        flag: bool,
        #[builder(as_is, default = None)]
        option: Option<u8>,
        #[builder(as_is, default = vec![1])]
        values: Vec<u8>,
    }

    let defaulted = S::builder().build();
    let overridden = S::builder()
        .flag(true)
        .option(Some(2))
        .values(vec![3, 4])
        .build();

    assert!(!defaulted.flag);
    assert_eq!(defaulted.option, None);
    assert_eq!(defaulted.values, vec![1]);
    assert!(overridden.flag);
    assert_eq!(overridden.option, Some(2));
    assert_eq!(overridden.values, vec![3, 4]);
}

#[test]
fn as_is_special_types_can_use_fixed_values() {
    #[derive(Builder)]
    struct S {
        #[builder(as_is, fixed = false)]
        flag: bool,
        #[builder(as_is, fixed = None)]
        option: Option<u8>,
        #[builder(as_is, fixed = vec![1, 2])]
        values: Vec<u8>,
    }

    let value = S::builder().build();

    assert!(!value.flag);
    assert_eq!(value.option, None);
    assert_eq!(value.values, vec![1, 2]);
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
fn vec_setter_accepts_iterator_chains() {
    #[derive(Builder)]
    struct S {
        values: Vec<u8>,
    }

    let values = std::iter::once(1).chain([2, 3]);
    let s = S::builder().values(values).build();

    assert_eq!(s.values, vec![1, 2, 3]);
}

#[test]
fn each_setter_can_use_a_raw_keyword_identifier() {
    #[derive(Builder)]
    struct S {
        #[builder(each = "r#type")]
        values: Vec<u8>,
    }

    let s = S::builder().r#type(1).r#type(2).build();

    assert_eq!(s.values, vec![1, 2]);
}

#[test]
fn each_and_name_attributes_rename_bulk_and_item_setters() {
    #[derive(Builder)]
    struct S {
        #[builder(each = "item", name = "append_all")]
        values: Vec<u8>,
    }

    let s = S::builder().item(1).append_all([2, 3]).item(4).build();

    assert_eq!(s.values, vec![1, 2, 3, 4]);
}

#[test]
fn default_expression_is_used_unless_overridden() {
    #[derive(Builder)]
    struct S {
        #[builder(default = 2u8.pow(2))]
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
        #[builder(fixed = 2u8.pow(2))]
        value: u8,
    }

    let s = S::builder().build();

    assert_eq!(s.value, 4);
}

#[test]
#[allow(clippy::let_and_return)]
fn legacy_string_expressions_remain_supported() {
    #[derive(Builder)]
    struct S {
        #[builder(default = "2u8.pow(2)")]
        defaulted: u8,
        #[builder(fixed = "2u8.pow(3)")]
        fixed: u8,
        #[builder(default = { let text = "native string literal"; text })]
        text: &'static str,
        #[builder(fixed = { let text = "fixed string literal"; text })]
        fixed_text: &'static str,
    }

    let s = S::builder().build();

    assert_eq!(s.defaulted, 4);
    assert_eq!(s.fixed, 8);
    assert_eq!(s.text, "native string literal");
    assert_eq!(s.fixed_text, "fixed string literal");
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
fn option_setter_accepts_non_clone_values_and_drops_them_once() {
    #[derive(Builder)]
    struct S {
        required: u8,
        optional: Option<DropProbe>,
    }

    let drops = Arc::new(AtomicUsize::new(0));
    drop(S::builder().optional(drop_probe(&drops)));
    assert_eq!(drops.load(Ordering::SeqCst), 1);

    drops.store(0, Ordering::SeqCst);
    let value = S::builder()
        .required(7)
        .optional(drop_probe(&drops))
        .build();

    assert_eq!(drops.load(Ordering::SeqCst), 0);
    drop(value);
    assert_eq!(drops.load(Ordering::SeqCst), 1);
}

#[test]
fn vec_setter_accepts_non_clone_values_and_drops_them_once() {
    #[derive(Builder)]
    struct S {
        values: Vec<DropProbe>,
    }

    let drops = Arc::new(AtomicUsize::new(0));
    let value = S::builder()
        .values([drop_probe(&drops), drop_probe(&drops)])
        .build();

    assert_eq!(drops.load(Ordering::SeqCst), 0);
    drop(value);
    assert_eq!(drops.load(Ordering::SeqCst), 2);
}

#[test]
fn vec_setter_drops_appended_values_when_iterator_panics() {
    #[derive(Builder)]
    struct S {
        values: Vec<DropProbe>,
    }

    let drops = Arc::new(AtomicUsize::new(0));
    let mut yielded = false;
    let values = std::iter::from_fn(|| {
        if yielded {
            panic!("iterator failed");
        }
        yielded = true;
        Some(drop_probe(&drops))
    });

    let result =
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| S::builder().values(values)));

    assert!(result.is_err());
    assert_eq!(drops.load(Ordering::SeqCst), 1);
}

#[test]
fn each_setter_accepts_non_clone_values_and_drops_them_once() {
    #[derive(Builder)]
    struct S {
        #[builder(each = "value")]
        values: Vec<DropProbe>,
    }

    let drops = Arc::new(AtomicUsize::new(0));
    let value = S::builder()
        .value(drop_probe(&drops))
        .value(drop_probe(&drops))
        .build();

    assert_eq!(drops.load(Ordering::SeqCst), 0);
    drop(value);
    assert_eq!(drops.load(Ordering::SeqCst), 2);
}

#[test]
fn dropping_incomplete_builder_drops_each_vec_value_once() {
    #[derive(Builder)]
    struct S {
        #[builder(each = "value")]
        values: Vec<DropProbe>,
        required: String,
    }

    let drops = Arc::new(AtomicUsize::new(0));
    let builder = S::builder()
        .value(drop_probe(&drops))
        .value(drop_probe(&drops));

    drop(builder);

    assert_eq!(drops.load(Ordering::SeqCst), 2);
}

#[test]
fn defaulted_and_fixed_values_drop_once() {
    #[derive(Builder)]
    struct S {
        #[builder(default = expression_drop_probe())]
        defaulted: DropProbe,
        #[builder(fixed = expression_drop_probe())]
        fixed: DropProbe,
    }

    let drops = expression_drop_counter();
    drops.store(0, Ordering::SeqCst);

    drop(S::builder());

    assert_eq!(drops.load(Ordering::SeqCst), 2);

    drops.store(0, Ordering::SeqCst);
    let value = S::builder().build();
    assert_eq!(drops.load(Ordering::SeqCst), 0);

    drop(value);

    assert_eq!(drops.load(Ordering::SeqCst), 2);
}

#[test]
fn overridden_default_value_is_dropped_once() {
    #[derive(Builder)]
    struct S {
        #[builder(default = expression_drop_probe())]
        value: DropProbe,
    }

    let drops = expression_drop_counter();
    drops.store(0, Ordering::SeqCst);

    let value = S::builder().value(drop_probe(drops)).build();

    assert_eq!(drops.load(Ordering::SeqCst), 1);
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

#[test]
fn generated_state_names_do_not_shadow_raw_user_identifiers() {
    #[derive(Builder)]
    struct Generic<r#__BuilderState0> {
        value: r#__BuilderState0,
    }

    let value = Generic::<u8>::builder().value(7).build();

    assert_eq!(value.value, 7);
}

#[test]
fn builder_supports_raw_identifiers_for_struct_and_field_names() {
    #[allow(non_camel_case_types)]
    #[derive(Builder)]
    struct r#type {
        r#match: u8,
    }

    let value = r#type::builder().r#match(7).build();

    assert_eq!(value.r#match, 7);
}

mod public_api {
    use super::Builder;

    #[derive(Builder)]
    pub struct PublicThing {
        pub value: u32,
    }
}

mod crate_visible_api {
    use super::Builder;

    #[derive(Builder)]
    pub(crate) struct CrateVisibleThing {
        pub(crate) value: u32,
    }
}

#[test]
fn public_struct_exposes_its_generated_builder_type() {
    let builder: public_api::PublicThingBuilder<_> = public_api::PublicThing::builder();
    let value = builder.value(42).build();

    assert_eq!(value.value, 42);
}

#[test]
fn crate_visible_struct_exposes_a_crate_visible_builder_type() {
    let builder: crate_visible_api::CrateVisibleThingBuilder<_> =
        crate_visible_api::CrateVisibleThing::builder();
    let value = builder.value(42).build();

    assert_eq!(value.value, 42);
}
