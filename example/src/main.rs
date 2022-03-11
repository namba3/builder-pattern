use builder_pattern_derive::Builder;

#[allow(dead_code)]
#[derive(Builder, Debug)]
#[builder(validate = "")]
struct Something {
    id: u64,
    name: String,
    age: u64,
    dead: bool,
    another_name: Option<String>,
    numbers: Vec<u64>,
}

fn main() {
    let s = Something::builder()
        .id(100)
        .name("qwerty".to_string())
        .age(99)
        .dead(false)
        .another_name("1234567890".to_string())
        .numbers(1)
        .numbers(2)
        .numbers_append([3, 4, 5])
        .build();

    println!("{s:?}");
}
