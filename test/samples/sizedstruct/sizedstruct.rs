/// contains a list
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct List {
    /// the list's size
    size1: i32,
    /// the integer list
    int_list: Vec<i32>,
}

/// contains a string
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct String_ {
    /// the list's size
    size2: i32,
    /// the string list
    string_list: String,
}

/// contains a matrix
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Matrix {
    /// the list's size
    size3: i32,
    /// the list list
    list_list: Vec<Vec<i32>>,
}

/// this is not a 'sized struct', but a regular one!
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct NotASizedStruct {
    /// not the list's size
    size4: i32,
    /// the integer list
    int_list_n: Vec<i32>,
}

/// * `n` - the size of the lists
/// * `lists` - a list of list of different sizes
/// * `strings` - a list of strings of different sizes
/// * `matrices` - a list of matrices of different sizes
/// * `same` - a list of list of same sizes
fn sized_struct(n: i32, lists: Vec<List>, strings: Vec<String_>, matrices: Vec<Matrix>, same: Vec<NotASizedStruct>) {
    /* TODO The is a special case. */
}

fn main() {
    let mut buffer = String::new();

    let n = read_line(&mut buffer)
        .parse()
        .expect("invalid `n` parameter");

    let lists = (0..n)
        .map(|_| read_struct_list(&mut buffer))
        .collect::<Result<_, _>>()
        .expect("invalid `lists` parameter");

    let strings = (0..n)
        .map(|_| read_struct_string(&mut buffer))
        .collect::<Result<_, _>>()
        .expect("invalid `strings` parameter");

    let matrices = (0..2)
        .map(|_| read_struct_matrix(&mut buffer))
        .collect::<Result<_, _>>()
        .expect("invalid `matrices` parameter");

    let same = (0..n)
        .map(|_| read_struct_not_a_sized_struct(&mut buffer))
        .collect::<Result<_, _>>()
        .expect("invalid `same` parameter");

    sized_struct(n, lists, strings, matrices, same);
}

fn read_line(buffer: &mut String) -> &str {
    buffer.clear();
    std::io::stdin()
        .read_line(buffer)
        .expect("impossible to read a new line");
    buffer.trim_end()
}

fn read_struct_list(mut buffer: &mut String) -> Result<List, Box<dyn std::error::Error>> {
    let size1 = read_line(&mut buffer).parse()?;
    let int_list = read_line(&mut buffer)
        .split_whitespace()
        .map(str::parse)
        .collect::<Result<_, _>>()?;
    Ok(List { size1, int_list })
}

fn read_struct_string(mut buffer: &mut String) -> Result<String_, Box<dyn std::error::Error>> {
    let size2 = read_line(&mut buffer).parse()?;
    let string_list = read_line(&mut buffer).to_string();
    Ok(String_ { size2, string_list })
}

fn read_struct_matrix(mut buffer: &mut String) -> Result<Matrix, Box<dyn std::error::Error>> {
    let size3 = read_line(&mut buffer).parse()?;
    let list_list = (0..size3)
        .map(|_| {
            read_line(&mut buffer)
                .split_whitespace()
                .map(str::parse)
                .collect::<Result<_, _>>()
        })
        .collect::<Result<_, _>>()?;
    Ok(Matrix { size3, list_list })
}

fn read_struct_not_a_sized_struct(mut buffer: &mut String) -> Result<NotASizedStruct, Box<dyn std::error::Error>> {
    let size4 = read_line(&mut buffer).parse()?;
    let int_list_n = read_line(&mut buffer)
        .split_whitespace()
        .map(str::parse)
        .collect::<Result<_, _>>()?;
    Ok(NotASizedStruct { size4, int_list_n })
}
