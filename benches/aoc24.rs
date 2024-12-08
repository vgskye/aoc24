use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;

fn criterion_benchmark(c: &mut Criterion) {
    let day6 = std::fs::read_to_string("day6.txt").unwrap();
    c.bench_function("day6pt2", |b| b.iter(|| aoc24::day6pt2::solve(black_box(&day6))));
    let day7 = std::fs::read_to_string("day7.txt").unwrap();
    c.bench_function("day7pt2", |b| b.iter(|| aoc24::day7pt2::solve(black_box(&day7))));
    c.bench_function("day7pt2b", |b| b.iter(|| aoc24::day7pt2b::solve(black_box(&day7))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);