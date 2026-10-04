use builder_pattern_derive::Builder;
use std::{
    hint::black_box,
    time::{Duration, Instant},
};

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

fn measure<T>(name: &str, warmup: usize, iterations: usize, mut operation: impl FnMut() -> T) {
    for _ in 0..warmup {
        black_box(operation());
    }

    let start = Instant::now();
    for _ in 0..iterations {
        black_box(operation());
    }
    let elapsed = start.elapsed();

    println!(
        "{name:<32} {:>10.2} ns/iter ({iterations} iterations, {})",
        elapsed.as_nanos() as f64 / iterations as f64,
        format_duration(elapsed),
    );
}

fn format_duration(duration: Duration) -> String {
    if duration.as_secs() > 0 {
        format!("{:.3} s", duration.as_secs_f64())
    } else if duration.as_millis() > 0 {
        format!("{:.3} ms", duration.as_secs_f64() * 1_000.0)
    } else {
        format!("{:.3} µs", duration.as_secs_f64() * 1_000_000.0)
    }
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

fn main() {
    let iterations = iterations();
    let warmup = iterations.min(10_000);

    println!("Manual Instant benchmark (warmup: {warmup}, measured: {iterations})");

    measure(
        "derive builder / required fields",
        warmup,
        iterations,
        || {
            let value = RequiredFields::builder()
                .first(black_box(1))
                .second(black_box(2))
                .third(black_box(3))
                .fourth(black_box(4))
                .build();
            value.first + value.second + value.third + value.fourth
        },
    );

    measure(
        "struct literal / required fields",
        warmup,
        iterations,
        || {
            let value = DirectRequiredFields {
                first: black_box(1),
                second: black_box(2),
                third: black_box(3),
                fourth: black_box(4),
            };
            value.first + value.second + value.third + value.fourth
        },
    );

    measure("derive builder / Vec items", warmup, iterations, || {
        let value = Items::builder()
            .item(black_box(1))
            .item(black_box(2))
            .item(black_box(3))
            .item(black_box(4))
            .build();
        value.items.iter().sum::<u64>()
    });

    measure("Vec literal / Vec items", warmup, iterations, || {
        vec![black_box(1), black_box(2), black_box(3), black_box(4)]
            .iter()
            .sum::<u64>()
    });
}
