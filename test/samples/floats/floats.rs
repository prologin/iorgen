/// Represents coordinates
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
struct Coordinates {
    /// X
    x: f64,
    /// Y
    y: f64,
    /// Z
    z: f64,
}

/// Mix of fields that go on one line
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
struct InlinedMix {
    /// an integer
    integer: i32,
    /// a char
    char: char,
    /// a float
    float: f64,
}

/// a struct of chars
#[derive(Clone, Debug, PartialEq, PartialOrd)]
struct MultilineMix {
    /// an other integer
    integer_2: i32,
    /// a string of size 5
    string: String,
    /// an other float
    float_2: f64,
}

/// * `f` - a float
/// * `g` - a float, greater than f
/// * `point` - some coordinates
/// * `n` - a number
/// * `float_list` - a list of floats
/// * `other_list` - a list of floats
/// * `inlined` - some inlined structs
/// * `multiline` - a multiline struct
fn floats(f: f64, g: f64, point: Coordinates, n: i32, float_list: Vec<f64>, other_list: Vec<f64>, inlined: Vec<InlinedMix>, multiline: MultilineMix) {
    /* TODO Parsing is often easy, reprint mode is harder */
}

fn main() {
    let mut buffer = String::new();

    let f = read_line(&mut buffer)
        .parse()
        .expect("invalid `f` parameter");

    let g = read_line(&mut buffer)
        .parse()
        .expect("invalid `g` parameter");

    let point = read_line(&mut buffer)
        .parse()
        .expect("invalid `point` parameter");

    let n = read_line(&mut buffer)
        .parse()
        .expect("invalid `n` parameter");

    let float_list = read_line(&mut buffer)
        .split_whitespace()
        .map(str::parse)
        .collect::<Result<_, _>>()
        .expect("invalid `float list` parameter");

    let other_list = read_line(&mut buffer)
        .split_whitespace()
        .map(str::parse)
        .collect::<Result<_, _>>()
        .expect("invalid `other list` parameter");

    let inlined = (0..3)
        .map(|_| read_line(&mut buffer).parse())
        .collect::<Result<_, _>>()
        .expect("invalid `inlined` parameter");

    let multiline = read_struct_multiline_mix(&mut buffer).expect("invalid `multiline` parameter");

    floats(f, g, point, n, float_list, other_list, inlined, multiline);
}

fn read_line(buffer: &mut String) -> &str {
    buffer.clear();
    std::io::stdin()
        .read_line(buffer)
        .expect("impossible to read a new line");
    buffer.trim_end()
}

impl std::str::FromStr for Coordinates {
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

impl std::str::FromStr for InlinedMix {
    type Err = Box<dyn std::error::Error>;

    fn from_str(line: &str) -> Result<Self, Self::Err> {
        let mut line = line.split_whitespace();
        Ok(Self {
            integer: line.next().ok_or("missing `integer`")?.parse()?,
            char: line.next().ok_or("missing `char`")?.parse()?,
            float: line.next().ok_or("missing `float`")?.parse()?,
        })
    }
}

fn read_struct_multiline_mix(mut buffer: &mut String) -> Result<MultilineMix, Box<dyn std::error::Error>> {
    let integer_2 = read_line(&mut buffer).parse()?;
    let string = read_line(&mut buffer).to_string();
    let float_2 = read_line(&mut buffer).parse()?;
    Ok(MultilineMix { integer_2, string, float_2 })
}
