/// A simple struct
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Struct1 {
    /// a field
    foo: i32,
    /// a field
    bar: i32,
}

/// Represents a position
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Position {
    /// X
    x: i32,
    /// Y
    y: i32,
    /// Z
    z: i32,
}

/// A point's name and position
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Point {
    /// the point's name (single character)
    name: char,
    /// the point's description
    description: String,
    /// the point's position
    pos: Position,
}

/// a struct of chars
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Chars {
    /// a first char
    first_char: char,
    /// a second char
    second_char: char,
    /// a third char
    third_char: char,
}

/// * `struct_` - a struct 1 instance
/// * `n` - a number
/// * `struct_list` - a list a struct 1
/// * `triangle` - a triangle
/// * `struct_chars` - a struct of chars
fn structs(struct_: Struct1, n: i32, struct_list: Vec<Struct1>, triangle: Vec<Point>, struct_chars: Chars) {
    /* TODO Look at them structs. */
}

fn main() {
    let mut buffer = String::new();

    let struct_ = read_line(&mut buffer)
        .parse()
        .expect("invalid `struct` parameter");

    let n = read_line(&mut buffer)
        .parse()
        .expect("invalid `n` parameter");

    let struct_list = (0..n)
        .map(|_| read_line(&mut buffer).parse())
        .collect::<Result<_, _>>()
        .expect("invalid `struct_list` parameter");

    let triangle = (0..3)
        .map(|_| read_struct_point(&mut buffer))
        .collect::<Result<_, _>>()
        .expect("invalid `triangle` parameter");

    let struct_chars = read_line(&mut buffer)
        .parse()
        .expect("invalid `struct chars` parameter");

    structs(struct_, n, struct_list, triangle, struct_chars);
}

fn read_line(mut buffer: &mut String) -> &str {
    buffer.clear();
    std::io::stdin()
        .read_line(&mut buffer)
        .expect("impossible to read a new line");
    buffer.trim_end()
}

impl std::str::FromStr for Struct1 {
    type Err = Box<dyn std::error::Error>;

    fn from_str(line: &str) -> Result<Self, Self::Err> {
        let mut line = line.split_whitespace();
        Ok(Self {
            foo: line.next().ok_or("missing `foo`")?.parse()?,
            bar: line.next().ok_or("missing `bar`")?.parse()?,
        })
    }
}

impl std::str::FromStr for Position {
    type Err = Box<dyn std::error::Error>;

    fn from_str(line: &str) -> Result<Self, Self::Err> {
        let mut line = line.split_whitespace();
        Ok(Self {
            x: line.next().ok_or("missing `x`")?.parse()?,
            y: line.next().ok_or("missing `y`")?.parse()?,
            z: line.next().ok_or("missing `z`")?.parse()?,
        })
    }
}

fn read_struct_point(mut buffer: &mut String) -> Result<Point, Box<dyn std::error::Error>> {
    let name = read_line(&mut buffer).parse()?;
    let description = read_line(&mut buffer).to_string();
    let pos = read_line(&mut buffer).parse()?;
    Ok(Point { name, description, pos })
}

impl std::str::FromStr for Chars {
    type Err = Box<dyn std::error::Error>;

    fn from_str(line: &str) -> Result<Self, Self::Err> {
        let mut line = line.split_whitespace();
        Ok(Self {
            first_char: line.next().ok_or("missing `first_char`")?.parse()?,
            second_char: line.next().ok_or("missing `second_char`")?.parse()?,
            third_char: line.next().ok_or("missing `third_char`")?.parse()?,
        })
    }
}
