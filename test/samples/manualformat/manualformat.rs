/// * `a` - a first number
/// * `b` - a second number
/// * `c` - a third number
/// * `n` - This one on a new line
/// * `one_per_line` - an integer list, one per line
fn manual_format(a: i32, b: i32, c: i32, n: i32, one_per_line: Vec<i32>) {
    /* TODO From the function perspective, this is just 4 integers */
}

fn main() {
    let mut buffer = String::new();

    let (a, b, c) = match read_line(&mut buffer)
        .split_whitespace()
        .collect::<Vec<&str>>()
        .as_slice()
    {
        [a, b, c] => (
            a.parse().expect("invalid `A` parameter"),
            b.parse().expect("invalid `B` parameter"),
            c.parse().expect("invalid `C` parameter"),
        ),
        _ => panic!("invalid `A` `B` `C` parameters"),
    };

    let n = read_line(&mut buffer)
        .parse()
        .expect("invalid `N` parameter");

    let one_per_line = (0..3)
        .map(|_| read_line(&mut buffer).parse())
        .collect::<Result<_, _>>()
        .expect("invalid `one_per_line` parameter");

    manual_format(a, b, c, n, one_per_line);
}

fn read_line(buffer: &mut String) -> &str {
    buffer.clear();
    std::io::stdin()
        .read_line(buffer)
        .expect("impossible to read a new line");
    buffer.trim_end()
}
