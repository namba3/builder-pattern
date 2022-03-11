use builder_pattern_derive::Builder;

#[allow(dead_code)]
#[derive(Builder, Debug)]
struct Something {
    // #[builder]
    id: u64,
    #[builder = "good_name"]
    name: String,
    #[builder(name = "set_age")]
    age: u64,

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
}

fn main() {
    let s = Something::builder()
        .id(100)
        .good_name("qwerty".to_string())
        .set_age(99)
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
        .build();

    println!("{s:?}");
}
