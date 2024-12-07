use rayon::{iter::ParallelIterator, str::ParallelString};

fn is_okay((target, numbers): (u64, Vec<u64>)) -> Option<u64> {
    let (&first, rest) = numbers.split_first().unwrap();
    let mut prev = vec![target];
    for &item in rest.iter().rev() {
        let mut cur = vec![];
        for &val in prev.iter() {
            if val % item == 0 {
                cur.push(val / item);
            }
            if val > item {
                cur.push(val - item);
            }
            let n = 10_u64.pow(1 + item.ilog10());
            if val % n == item {
                cur.push(val / n);
            }
        }
        prev = cur;
    }
    if prev.contains(&first) {
        Some(target)
    } else {
        None
    }
}

fn parse_line(line: &str) -> (u64, Vec<u64>) {
    let (target, numbers) = line.split_once(": ").unwrap();
    (target.parse().unwrap(), numbers.split(' ').map(|e| e.parse().unwrap()).collect())
}

fn main() {
    let file = std::fs::read_to_string("day7.txt").unwrap();
    let answer: u64 = file
        .par_lines()
        .map(parse_line)
        .filter_map(is_okay)
        .sum();
    println!("{answer}");
}