/// * `n` - the first list's size
/// * `list_int` - a list containing ints
/// * `size` - an other size
/// * `list_char` - a list of char
/// * `string` - a string
/// * `list_string4` - a list of strings of size 4
/// * `list_list_string2` - a list of list of strings of size 2 of size 2 of size 2
/// * `matrix` - a matrix of int
fn lists(n: i32, list_int: Vec<i32>, size: i32, list_char: Vec<char>, string: String, list_string4: Vec<String>, list_list_string2: Vec<Vec<String>>, matrix: Vec<Vec<i32>>) {
    /* TODO Aren't these lists beautifull? */
}

fn main() {
    let mut buffer = String::new();

    let n = read_line(&mut buffer)
        .parse()
        .expect("invalid `N` parameter");

    let list_int = read_line(&mut buffer)
        .split_whitespace()
        .map(str::parse)
        .collect::<Result<_, _>>()
        .expect("invalid `list int` parameter");

    let size = read_line(&mut buffer)
        .parse()
        .expect("invalid `size` parameter");

    let list_char = read_line(&mut buffer).chars().collect();

    let string = read_line(&mut buffer).to_string();

    let list_string4 = (0..size)
        .map(|_| read_line(&mut buffer).to_string())
        .collect();

    let list_list_string2 = (0..2)
        .map(|_| {
            (0..2)
                .map(|_| read_line(&mut buffer).to_string())
                .collect()
        })
        .collect();

    let matrix = (0..size)
        .map(|_| {
            read_line(&mut buffer)
                .split_whitespace()
                .map(str::parse)
                .collect::<Result<_, _>>()
        })
        .collect::<Result<_, _>>()
        .expect("invalid `matrix` parameter");

    lists(n, list_int, size, list_char, string, list_string4, list_list_string2, matrix);
}

fn read_line(mut buffer: &mut String) -> &str {
    buffer.clear();
    std::io::stdin()
        .read_line(&mut buffer)
        .expect("impossible to read a new line");
    buffer.trim_end()
}
