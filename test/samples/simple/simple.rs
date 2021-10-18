/// * `n` - the first number
/// * `other_number` - the second number
fn simple(n: i32, other_number: i32) {
    /* TODO Just do what you want with these numbers, like sum them. */
}

fn main() {
    let mut buffer = String::new();

    let n = read_line(&mut buffer)
        .parse()
        .expect("invalid `N` parameter");

    let other_number = read_line(&mut buffer)
        .parse()
        .expect("invalid `other number` parameter");

    simple(n, other_number);
}

fn read_line(buffer: &mut String) -> &str {
    buffer.clear();
    std::io::stdin()
        .read_line(buffer)
        .expect("impossible to read a new line");
    buffer.trim_end()
}
