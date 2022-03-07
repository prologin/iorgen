/// may conflict in c#
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Console {
    /// the first letter of the alphabet
    a: i32,
    /// an integer
    static_: i32,
}

/// may conflict in c#
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct System {
    /// not the end of the function
    return_: i32,
    /// not nothing
    void: Vec<i32>,
}

/// not the main function
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Main {
    /// not an integer
    int: System,
    /// should not cause conflict
    if_true: i32,
}

/// * `if_` - not a condition
/// * `class` - not a class
/// * `i` - just a string
/// * `in_` - not in
/// * `for_` - not a loop
/// * `words` - contains lots of things
/// * `words_1` - an integer
fn keywords(if_: i32, class: char, i: String, in_: Console, for_: Vec<i32>, words: Vec<Main>, words_1: i32) {
    /* TODO If this compiles, it is already a good step! */
}

fn main() {
    let mut buffer = String::new();

    let if_ = read_line(&mut buffer)
        .parse()
        .expect("invalid `if` parameter");

    let class = read_line(&mut buffer)
        .parse()
        .expect("invalid `class` parameter");

    let i = read_line(&mut buffer).to_string();

    let in_ = read_line(&mut buffer)
        .parse()
        .expect("invalid `in` parameter");

    let for_ = read_line(&mut buffer)
        .split_whitespace()
        .map(str::parse)
        .collect::<Result<_, _>>()
        .expect("invalid `for` parameter");

    let words = (0..2)
        .map(|_| read_struct_main(&mut buffer))
        .collect::<Result<_, _>>()
        .expect("invalid `words` parameter");

    let words_1 = read_line(&mut buffer)
        .parse()
        .expect("invalid `words 1` parameter");

    keywords(if_, class, i, in_, for_, words, words_1);
}

fn read_line(buffer: &mut String) -> &str {
    buffer.clear();
    std::io::stdin()
        .read_line(buffer)
        .expect("impossible to read a new line");
    buffer.trim_end()
}

impl std::str::FromStr for Console {
    type Err = Box<dyn std::error::Error>;

    fn from_str(line: &str) -> Result<Self, Self::Err> {
        let mut line = line.split_whitespace();
        Ok(Self {
            a: line.next().ok_or("missing `a`")?.parse()?,
            static_: line.next().ok_or("missing `static_`")?.parse()?,
        })
    }
}

fn read_struct_system(mut buffer: &mut String) -> Result<System, Box<dyn std::error::Error>> {
    let return_ = read_line(&mut buffer).parse()?;
    let void = read_line(&mut buffer)
        .split_whitespace()
        .map(str::parse)
        .collect::<Result<_, _>>()?;
    Ok(System { return_, void })
}

fn read_struct_main(mut buffer: &mut String) -> Result<Main, Box<dyn std::error::Error>> {
    let int = read_struct_system(&mut buffer)?;
    let if_true = read_line(&mut buffer).parse()?;
    Ok(Main { int, if_true })
}
