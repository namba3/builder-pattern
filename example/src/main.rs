use builder_pattern_derive::Builder;

#[derive(Builder, Debug)]
struct S {
    id: u64,
    name: String,
    age: u64,
    dead: bool,
    numbers: Vec<u64>,
}

fn main() {
    let s = S::builder()
        .id(100)
        .name("qwerty".to_string())
        .age(99)
        .dead(false)
        .numbers(vec![9, 1, 4, 2, 9, 2, 5])
        .build();
    println!("{s:?}");

    f();
}

fn f() {
    let a = A::builder()
        .c("33".to_string())
        .c("44".to_string())
        .a(0.01)
        .b(22)
        .build();
    println!("{a:?}");
}

use std::mem::transmute;
use std::mem::MaybeUninit;
#[derive(Debug)]
struct A {
    a: f64,
    b: u32,
    c: Vec<String>,
}
impl A {
    fn builder() -> ABuilder<MaybeUninit<f64>, MaybeUninit<u32>, MaybeUninit<Vec<String>>> {
        unsafe { std::mem::zeroed() }
    }
}

struct ABuilder<T1, T2, T3>
where
    T1: Sized,
    T2: Sized,
    T3: Sized,
{
    a: T1,
    b: T2,
    c: T3,
}

impl<T2, T3> ABuilder<MaybeUninit<f64>, T2, T3> {
    fn a(mut self, value: f64) -> ABuilder<f64, T2, T3> {
        unsafe {
            self.a = core::mem::MaybeUninit::new(value);
            let builder = core::mem::transmute_copy(&self);
            core::mem::forget(self);
            builder
        }
    }
}
impl<T1, T3> ABuilder<T1, MaybeUninit<u32>, T3> {
    fn b(mut self, value: u32) -> ABuilder<T1, u32, T3> {
        unsafe {
            self.b = core::mem::MaybeUninit::new(value);
            let builder = core::mem::transmute_copy(&self);
            core::mem::forget(self);
            builder
        }
    }
}
impl<T1, T2> ABuilder<T1, T2, MaybeUninit<Vec<String>>> {
    fn c(mut self, value: String) -> ABuilder<T1, T2, Vec<String>> {
        unsafe {
            self.c = core::mem::MaybeUninit::new(vec![value]);
            let builder = core::mem::transmute_copy(&self);
            core::mem::forget(self);
            builder
        }
    }
}
impl<T1, T2> ABuilder<T1, T2, Vec<String>> {
    fn c(mut self, value: String) -> ABuilder<T1, T2, Vec<String>> {
        self.c.push(value);
        self
    }
}
impl ABuilder<f64, u32, Vec<String>> {
    fn build(self) -> A {
        unsafe { transmute(self) }
    }
}
