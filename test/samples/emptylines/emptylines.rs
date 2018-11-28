use std::io;

/// a char struct
struct StructWithAChar {
    /// a char
    char1: char,
    /// an integer
    int2: i32,
}

/// a struct
struct A {
    /// a list in a struct
    list_in_struct: Vec<i32>,
    /// a struct in a struct
    struct_in_struct: StructWithAChar,
}

/// a sized struct
struct SizedStruct {
    /// the size
    size: i32,
    /// the string
    string_in_struct: String,
}

/// * `empty_list` - an empty list
/// * `buffer_string` - here to check correct parsing of empty line above
/// * `n` - an integer, will be 0 in the sample input
/// * `empty_in_sample` - an empty list (only in the sample)
/// * `empty_string` - an empty string
/// * `main` - an other buffer string
/// * `empty_char_list` - an empty char list
/// * `non_empty_char_list` - an char list, non empty
/// * `struct_with_empty_line` - a struct containing an empty line, then a struct
/// * `a_sized_struct` - a sized struct containing an empty line
/// * `finish` - a string to finish
fn empty_lines(empty_list: Vec<i32>, buffer_string: String, n: i32, empty_in_sample: Vec<i32>, empty_string: String, main: String, empty_char_list: Vec<char>, non_empty_char_list: Vec<char>, struct_with_empty_line: A, a_sized_struct: SizedStruct, finish: String) {
    /* TODO Wow, lots of empty lines! */
}

fn main() {
    let empty_list: Vec<i32> = read_vec_int();
    let buffer_string: String = read_line();
    let n: i32 = read_line().parse().unwrap();
    let empty_in_sample: Vec<i32> = read_vec_int();
    let empty_string: String = read_line();
    let main: String = read_line();
    let empty_char_list: Vec<char> = read_line().chars().collect();
    let non_empty_char_list: Vec<char> = read_line().chars().collect();
    let struct_with_empty_line: A = read_struct_a();
    let a_sized_struct: SizedStruct = read_struct_sized_struct();
    let finish: String = read_line();

    empty_lines(empty_list, buffer_string, n, empty_in_sample, empty_string, main, empty_char_list, non_empty_char_list, struct_with_empty_line, a_sized_struct, finish);
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

fn read_struct_struct_with_a_char() -> StructWithAChar {
    let line = read_line();
    let words: Vec<&str> = line.split_whitespace().collect();
    StructWithAChar {
        char1: words[0].parse().unwrap(),
        int2: words[1].parse().unwrap(),
    }
}

fn read_struct_a() -> A {
    let list_in_struct: Vec<i32> = read_vec_int();
    let struct_in_struct: StructWithAChar = read_struct_struct_with_a_char();
    A { list_in_struct, struct_in_struct }
}

fn read_struct_sized_struct() -> SizedStruct {
    let size: i32 = read_line().parse().unwrap();
    let string_in_struct: String = read_line();
    SizedStruct { size, string_in_struct }
}
