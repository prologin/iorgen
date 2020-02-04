/// A struct for the example
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct AStruct {
    /// an integer
    integer: i32,
    /// a char
    character: char,
}

/// * `n` - a number, used as a size
/// * `list` - a list of structs
fn example(n: i32, list: Vec<AStruct>) {
    /* TODO In a real life scenario, you will describe here what you want the
    end user to do with this generated code */
}

fn main() {
    let mut buffer = String::new();

    let n = read_line(&mut buffer)
        .parse()
        .expect("invalid `N` parameter");

    let list = (0..n)
        .map(|_| read_line(&mut buffer).parse())
        .collect::<Result<_, _>>()
        .expect("invalid `list` parameter");

    example(n, list);
}

fn read_line(mut buffer: &mut String) -> &str {
    buffer.clear();
    std::io::stdin()
        .read_line(&mut buffer)
        .expect("impossible to read a new line");
    buffer.trim_end()
}

impl std::str::FromStr for AStruct {
    type Err = Box<dyn std::error::Error>;

    fn from_str(line: &str) -> Result<Self, Self::Err> {
        let mut line = line.split_whitespace();
        Ok(Self {
            integer: line.next().ok_or("missing `integer`")?.parse()?,
            character: line.next().ok_or("missing `character`")?.parse()?,
        })
    }
}
