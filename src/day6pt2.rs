use std::{collections::HashSet, ops::Add};

use rayon::iter::{IntoParallelIterator, ParallelIterator};

#[derive(PartialEq, Eq, Clone, Copy)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl Direction {
    fn right(self) -> Self {
        match self {
            Direction::Up => Direction::Right,
            Direction::Right => Direction::Down,
            Direction::Down => Direction::Left,
            Direction::Left => Direction::Up,
        }
    }
}

impl Add<Direction> for (isize, isize) {
    type Output = (isize, isize);

    fn add(self, rhs: Direction) -> Self::Output {
        match rhs {
            Direction::Up => (self.0, self.1 - 1),
            Direction::Right => (self.0 + 1, self.1),
            Direction::Down => (self.0, self.1 + 1),
            Direction::Left => (self.0 - 1, self.1),
        }
    }
}

fn step(map: &[&[u8]], pos: (isize, isize), dir: Direction) -> ((isize, isize), Direction) {
    let (x, y) = pos + dir;
    if map
        .get(y as usize)
        .copied()
        .unwrap_or(b"")
        .get(x as usize)
        .copied()
        .unwrap_or(b'.')
        == b'#'
    {
        (pos, dir.right())
    } else {
        ((x, y), dir)
    }
}

fn step_with_obstacle(
    map: &[&[u8]],
    pos: (isize, isize),
    dir: Direction,
    obstacle: (isize, isize),
) -> ((isize, isize), Direction) {
    let (x, y) = pos + dir;
    if (x, y) == obstacle
        || map
            .get(y as usize)
            .copied()
            .unwrap_or(b"")
            .get(x as usize)
            .copied()
            .unwrap_or(b'.')
            == b'#'
    {
        (pos, dir.right())
    } else {
        ((x, y), dir)
    }
}

fn loops(
    map: &[&[u8]],
    (x, y): (isize, isize),
    dir: Direction,
    visited: &mut HashSet<(isize, isize)>,
    obstacle: (isize, isize),
) -> bool {
    let w = map[0].len() as isize;
    let h = map.len() as isize;
    if x < 0 || x >= w || y < 0 || y >= h {
        return false;
    }
    let (new_pos, new_dir) = step_with_obstacle(map, (x, y), dir, obstacle);
    if dir != new_dir && new_dir == Direction::Right && !visited.insert(new_pos) {
        return true;
    }
    loops(map, new_pos, new_dir, visited, obstacle)
}

fn collect_pos(
    map: &[&[u8]],
    (x, y): (isize, isize),
    dir: Direction,
    visited: &mut HashSet<(isize, isize)>,
) {
    let w = map[0].len() as isize;
    let h = map.len() as isize;
    if x < 0 || x >= w || y < 0 || y >= h {
        return;
    }
    visited.insert((x, y));
    let (new_pos, new_dir) = step(map, (x, y), dir);
    collect_pos(map, new_pos, new_dir, visited);
}

pub fn solve(file: &str) -> u64 {
    let map: Vec<_> = file.lines().map(|e| e.as_bytes()).collect();
    let guard_y = map.iter().position(|line| line.contains(&b'^')).unwrap() as isize;
    let guard_x = map
        .iter()
        .find_map(|&line| line.iter().position(|&e| e == b'^'))
        .unwrap() as isize;
    let mut to_check = HashSet::new();
    collect_pos(&map, (guard_x, guard_y), Direction::Up, &mut to_check);
    to_check.remove(&(guard_x, guard_y));
    let answer = to_check
        .into_par_iter()
        .filter(|&pos| {
            loops(
                &map,
                (guard_x, guard_y),
                Direction::Up,
                &mut HashSet::new(),
                pos,
            )
        })
        .count();
    answer as u64
}
