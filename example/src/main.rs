use builder_pattern_derive::Builder;

#[derive(Builder, Debug)]
#[repr(C)]
struct Something {
    id: u64,
    #[builder = "good_name"]
    name: String,
    #[builder(name = "set_age")]
    age: u64,

    has_drivers_licence: bool,
    #[builder(name = "dead")]
    is_dead: bool,
    #[builder(as_is, name = "human")]
    is_human: bool,

    another_id: Option<u64>,
    #[builder(as_is)]
    another_name: Option<String>,

    strings: Vec<String>,
    #[builder(each = "num", name = "nums_append")]
    nums: Vec<u64>,
    #[builder(as_is)]
    bools: Vec<bool>,

    #[builder(default = "100")]
    val1: u8,
    #[builder(default = "100")]
    val2: u8,

    #[builder(fixed = "[0,1,2,3]")]
    reserved1: [u8; 4],
    #[builder(fixed = "init_reserved2()")]
    reserved2: [u8; 4],

    #[builder(fixed = r#"String::from("z")"#)]
    z: String,
}

fn init_reserved2() -> [u8; 4] {
    [3, 2, 1, 0]
}

fn main() {
    let s = Something::builder()
        .id(100)
        .good_name("qwerty".to_string())
        .set_age(99)
        .has_drivers_licence()
        .dead()
        .human(true)
        .another_id(1234567890)
        .another_name(Some("1234567890".to_string()))
        .strings(["a".to_string(), "b".to_string()])
        .strings(["c".to_string(), "d".to_string()])
        .num(1)
        .num(2)
        .nums_append([3, 4, 5])
        .nums_append([6, 7])
        .bools(vec![false, true, false, true])
        .val2(111)
        .build();

    println!("{s:?}");
}
