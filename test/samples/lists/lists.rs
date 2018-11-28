use std::io;

/// * `n` - the first list's size
/// * `list_int` - a list containing ints
/// * `size` - an other size
/// * `list_char` - a list of char
/// * `list_string4` - a list of strings of size 4
/// * `matrix` - a matrix of int
fn lists(n: i32, list_int: Vec<i32>, size: i32, list_char: Vec<char>, list_string4: Vec<String>, matrix: Vec<Vec<i32>>) {
    /* TODO Aren't these lists beautifull? */
}

fn main() {
    let n: i32 = read_line().parse().unwrap();
    let list_int: Vec<i32> = read_vec_int();
    let size: i32 = read_line().parse().unwrap();
    let list_char: Vec<char> = read_line().chars().collect();
    let mut list_string4: Vec<String> = Vec::with_capacity(size as usize);
    for _ in 0..size {
        list_string4.push(read_line());
    }
    let mut matrix: Vec<Vec<i32>> = Vec::with_capacity(size as usize);
    for _ in 0..size {
        matrix.push(read_vec_int());
    }

    lists(n, list_int, size, list_char, list_string4, matrix);
}

fn read_line() -> String {
    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .expect("Failed to read line");
    line.trim().to_string()
}

fn read_vec_int() -> Vec<i32> {
    read_line()
        .split_whitespace()
        .collect::<Vec<&str>>()
        .iter()
        .map(|x| x.parse().unwrap())
        .collect()
}
