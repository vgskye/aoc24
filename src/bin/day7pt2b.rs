fn main() {
    let file = std::fs::read_to_string("day7.txt").unwrap();
    let answer = aoc24::day7pt2b::solve(&file);
    println!("{answer}");
}