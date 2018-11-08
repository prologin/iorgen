use std::io;

/// may conflict in c#
struct Console {
    /// the first letter of the alphabet
    a: i32,
    /// an integer
    static_: i32,
}

/// may conflict in c#
struct System {
    /// not the end of the function
    return_: i32,
    /// not nothing
    void: Vec<i32>,
}

/// not the main function
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
fn keywords(if_: i32, class: char, i: String, in_: Console, for_: Vec<i32>, words: Vec<Main>) {
    /* TODO If this compiles, it is already a good step! */
}

fn main() {
    let if_: i32 = read_line().parse().unwrap();
    let class: char = read_line().parse().unwrap();
    let i: String = read_line();
    let in_: Console = read_struct_console();
    let for_: Vec<i32> = read_vec_int();
    let mut words: Vec<Main> = Vec::with_capacity(2 as usize);
    for _ in 0..2 {
        let words_elem: Main = read_struct_main();
        words.push(words_elem);
    }

    keywords(if_, class, i, in_, for_, words);
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
        .split(' ')
        .collect::<Vec<&str>>()
        .iter()
        .map(|x| x.parse().unwrap())
        .collect()
}

fn read_struct_console() -> Console {
    let line = read_line();
    let words: Vec<&str> = line.split(' ').collect();
    Console {
        a: words[0].parse().unwrap(),
        static_: words[1].parse().unwrap(),
    }
}

fn read_struct_system() -> System {
    let return_: i32 = read_line().parse().unwrap();
    let void: Vec<i32> = read_vec_int();
    System { return_, void }
}

fn read_struct_main() -> Main {
    let int: System = read_struct_system();
    let if_true: i32 = read_line().parse().unwrap();
    Main { int, if_true }
}
