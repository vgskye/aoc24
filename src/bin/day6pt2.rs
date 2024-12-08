fn main() {
    let file = std::fs::read_to_string("day6.txt").unwrap();
    let answer = aoc24::day6pt2::solve(&file);
    println!("{answer}");
}
