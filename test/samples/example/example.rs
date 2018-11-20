use std::io;

/// A struct for the example
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
    let n: i32 = read_line().parse().unwrap();
    let mut list: Vec<AStruct> = Vec::with_capacity(n as usize);
    for _ in 0..n {
        list.push(read_struct_a_struct());
    }

    example(n, list);
}

fn read_line() -> String {
    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .expect("Failed to read line");
    line.trim().to_string()
}

fn read_struct_a_struct() -> AStruct {
    let line = read_line();
    let words: Vec<&str> = line.split(' ').collect();
    AStruct {
        integer: words[0].parse().unwrap(),
        character: words[1].parse().unwrap(),
    }
}
