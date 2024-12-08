use rayon::{iter::ParallelIterator, str::ParallelString};

fn concat(a: u64, b: u64) -> u64 {
    a * (10_u64.pow(1 + b.ilog10())) + b
}

fn make_possibilities(remaining: &[u64], callback: &dyn Fn(u64) -> bool) -> bool {
    if remaining.len() == 1 {
        let item = remaining[0];
        return callback(item);
    }
    let (&tail, head) = remaining.split_last().unwrap();
    make_possibilities(head, &|e| {
        callback(e * tail) || callback(e + tail) || callback(concat(e, tail))
    })
}

fn is_okay((target, numbers): (u64, Vec<u64>)) -> Option<u64> {
    if make_possibilities(&numbers, &|e| e == target) {
        Some(target)
    } else {
        None
    }
}

fn parse_line(line: &str) -> (u64, Vec<u64>) {
    let (target, numbers) = line.split_once(": ").unwrap();
    (
        target.parse().unwrap(),
        numbers.split(' ').map(|e| e.parse().unwrap()).collect(),
    )
}

fn main() {
    let file = std::fs::read_to_string("day7.txt").unwrap();
    let answer: u64 = file.par_lines().map(parse_line).filter_map(is_okay).sum();
    println!("{answer}");
}
