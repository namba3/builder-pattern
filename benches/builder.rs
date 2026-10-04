#![allow(dead_code)]
// The benchmark baseline intentionally measures repeated Vec::push calls.
#![allow(clippy::vec_init_then_push)]

use builder_pattern_derive::Builder;
use std::hint::black_box;
use std::time::Instant;

#[derive(Builder)]
struct RequiredFields {
    first: u64,
    second: u64,
    third: u64,
    fourth: u64,
}

struct DirectRequiredFields {
    first: u64,
    second: u64,
    third: u64,
    fourth: u64,
}

#[derive(Builder)]
struct Items {
    #[builder(each = "item")]
    items: Vec<u64>,
}

#[derive(Builder)]
struct SpecialFields {
    enabled: bool,
    optional: Option<u64>,
    #[builder(default = "7u64")]
    count: u64,
}

struct DirectSpecialFields {
    enabled: bool,
    optional: Option<u64>,
    count: u64,
}

fn measure_once<T>(iterations: usize, operation: &mut impl FnMut() -> T) -> f64 {
    let start = Instant::now();
    for _ in 0..iterations {
        black_box(operation());
    }
    start.elapsed().as_nanos() as f64 / iterations as f64
}

fn summarize(samples: &mut [f64]) -> (f64, f64, f64) {
    samples.sort_by(f64::total_cmp);

    let median = if samples.len() % 2 == 0 {
        (samples[samples.len() / 2 - 1] + samples[samples.len() / 2]) / 2.0
    } else {
        samples[samples.len() / 2]
    };

    (median, samples[0], samples[samples.len() - 1])
}

fn print_summary(name: &str, mut samples: Vec<f64>) {
    let (median, min, max) = summarize(&mut samples);
    println!("{name:<32} median {median:>8.2} ns/iter (min {min:.2}, max {max:.2})");
}

fn measure_pair<T, U>(
    left_name: &str,
    right_name: &str,
    warmup: usize,
    iterations: usize,
    sample_count: usize,
    mut left: impl FnMut() -> T,
    mut right: impl FnMut() -> U,
) {
    for index in 0..warmup {
        if index % 2 == 0 {
            black_box(left());
            black_box(right());
        } else {
            black_box(right());
            black_box(left());
        }
    }

    let mut left_samples = Vec::with_capacity(sample_count);
    let mut right_samples = Vec::with_capacity(sample_count);

    for sample in 0..sample_count {
        if sample % 2 == 0 {
            left_samples.push(measure_once(iterations, &mut left));
            right_samples.push(measure_once(iterations, &mut right));
        } else {
            right_samples.push(measure_once(iterations, &mut right));
            left_samples.push(measure_once(iterations, &mut left));
        }
    }

    print_summary(left_name, left_samples);
    print_summary(right_name, right_samples);
}

fn iterations() -> usize {
    const DEFAULT_ITERATIONS: usize = 1_000_000;

    match std::env::var("BENCH_ITERS") {
        Ok(value) => match value.parse::<usize>() {
            Ok(iterations) if iterations > 0 => iterations,
            _ => {
                eprintln!("BENCH_ITERS must be a positive integer; using {DEFAULT_ITERATIONS}");
                DEFAULT_ITERATIONS
            }
        },
        Err(_) => DEFAULT_ITERATIONS,
    }
}

fn sample_count() -> usize {
    const DEFAULT_SAMPLES: usize = 11;

    match std::env::var("BENCH_SAMPLES") {
        Ok(value) => match value.parse::<usize>() {
            Ok(samples) if samples > 0 => samples,
            _ => {
                eprintln!("BENCH_SAMPLES must be a positive integer; using {DEFAULT_SAMPLES}");
                DEFAULT_SAMPLES
            }
        },
        Err(_) => DEFAULT_SAMPLES,
    }
}

fn main() {
    let iterations = iterations();
    let warmup = iterations.min(10_000);
    let samples = sample_count();

    println!(
        "Manual Instant benchmark (warmup: {warmup}, measured: {iterations} per sample, samples: {samples})"
    );

    measure_pair(
        "derive builder / required fields",
        "struct literal / required fields",
        warmup,
        iterations,
        samples,
        || {
            RequiredFields::builder()
                .first(black_box(1))
                .second(black_box(2))
                .third(black_box(3))
                .fourth(black_box(4))
                .build()
        },
        || DirectRequiredFields {
            first: black_box(1),
            second: black_box(2),
            third: black_box(3),
            fourth: black_box(4),
        },
    );

    measure_pair(
        "derive builder / Vec items",
        "Vec push / Vec items",
        warmup,
        iterations,
        samples,
        || {
            Items::builder()
                .item(black_box(1))
                .item(black_box(2))
                .item(black_box(3))
                .item(black_box(4))
                .build()
        },
        || {
            let mut items = Vec::new();
            items.push(black_box(1));
            items.push(black_box(2));
            items.push(black_box(3));
            items.push(black_box(4));
            items
        },
    );

    measure_pair(
        "derive builder / special defaults",
        "struct literal / special defaults",
        warmup,
        iterations,
        samples,
        || SpecialFields::builder().build(),
        || DirectSpecialFields {
            enabled: false,
            optional: None,
            count: 7,
        },
    );

    measure_pair(
        "derive builder / special setters",
        "struct literal / special values",
        warmup,
        iterations,
        samples,
        || {
            SpecialFields::builder()
                .enabled()
                .optional(black_box(11))
                .count(black_box(12))
                .build()
        },
        || DirectSpecialFields {
            enabled: true,
            optional: Some(black_box(11)),
            count: black_box(12),
        },
    );
}
