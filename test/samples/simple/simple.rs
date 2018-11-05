use std::io;

/// * `n` - the first number
/// * `other_number` - the second number
fn simple(n: i32, other_number: i32) {
    /* TODO Just do what you want with these numbers, like sum them. */
}

fn main() {
    let n: i32 = read_line().parse().unwrap();
    let other_number: i32 = read_line().parse().unwrap();

    simple(n, other_number);
}

fn read_line() -> String {
    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .expect("Failed to read line");
    line.trim().to_string()
}
