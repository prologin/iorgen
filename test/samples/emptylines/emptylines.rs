/// a char struct
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct StructWithAChar {
    /// a char
    char1: char,
    /// an integer
    int2: i32,
}

/// a struct
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct A {
    /// a list in a struct
    list_in_struct: Vec<i32>,
    /// a struct in a struct
    struct_in_struct: StructWithAChar,
}

/// a sized struct
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
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
    let mut buffer = String::new();

    let empty_list = read_line(&mut buffer)
        .split_whitespace()
        .map(str::parse)
        .collect::<Result<_, _>>()
        .expect("invalid `empty list` parameter");

    let buffer_string = read_line(&mut buffer).to_string();

    let n = read_line(&mut buffer)
        .parse()
        .expect("invalid `N` parameter");

    let empty_in_sample = read_line(&mut buffer)
        .split_whitespace()
        .map(str::parse)
        .collect::<Result<_, _>>()
        .expect("invalid `empty in sample` parameter");

    let empty_string = read_line(&mut buffer).to_string();

    let main = read_line(&mut buffer).to_string();

    let empty_char_list = read_line(&mut buffer).chars().collect();

    let non_empty_char_list = read_line(&mut buffer).chars().collect();

    let struct_with_empty_line = read_struct_a(&mut buffer).expect("invalid `struct_with_empty_line` parameter");

    let a_sized_struct = read_struct_sized_struct(&mut buffer).expect("invalid `a_sized_struct` parameter");

    let finish = read_line(&mut buffer).to_string();

    empty_lines(empty_list, buffer_string, n, empty_in_sample, empty_string, main, empty_char_list, non_empty_char_list, struct_with_empty_line, a_sized_struct, finish);
}

fn read_line(buffer: &mut String) -> &str {
    buffer.clear();
    std::io::stdin()
        .read_line(buffer)
        .expect("impossible to read a new line");
    buffer.trim_end()
}

impl std::str::FromStr for StructWithAChar {
    type Err = Box<dyn std::error::Error>;

    fn from_str(line: &str) -> Result<Self, Self::Err> {
        let mut line = line.split_whitespace();
        Ok(Self {
            char1: line.next().ok_or("missing `char1`")?.parse()?,
            int2: line.next().ok_or("missing `int2`")?.parse()?,
        })
    }
}

fn read_struct_a(mut buffer: &mut String) -> Result<A, Box<dyn std::error::Error>> {
    let list_in_struct = read_line(&mut buffer)
        .split_whitespace()
        .map(str::parse)
        .collect::<Result<_, _>>()?;
    let struct_in_struct = read_line(&mut buffer).parse()?;
    Ok(A { list_in_struct, struct_in_struct })
}

fn read_struct_sized_struct(mut buffer: &mut String) -> Result<SizedStruct, Box<dyn std::error::Error>> {
    let size = read_line(&mut buffer).parse()?;
    let string_in_struct = read_line(&mut buffer).to_string();
    Ok(SizedStruct { size, string_in_struct })
}
