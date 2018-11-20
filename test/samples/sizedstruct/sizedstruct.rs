use std::io;

/// contains a list
struct List {
    /// the list's size
    size1: i32,
    /// the integer list
    int_list: Vec<i32>,
}

/// contains a string
struct String_ {
    /// the list's size
    size2: i32,
    /// the string list
    string_list: String,
}

/// contains a matrix
struct Matrix {
    /// the list's size
    size3: i32,
    /// the list list
    list_list: Vec<Vec<i32>>,
}

/// this is not a 'sized struct', but a regular one!
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
    let n: i32 = read_line().parse().unwrap();
    let mut lists: Vec<List> = Vec::with_capacity(n as usize);
    for _ in 0..n {
        let lists_elem: List = read_struct_list();
        lists.push(lists_elem);
    }
    let mut strings: Vec<String_> = Vec::with_capacity(n as usize);
    for _ in 0..n {
        let strings_elem: String_ = read_struct_string();
        strings.push(strings_elem);
    }
    let mut matrices: Vec<Matrix> = Vec::with_capacity(2 as usize);
    for _ in 0..2 {
        let matrices_elem: Matrix = read_struct_matrix();
        matrices.push(matrices_elem);
    }
    let mut same: Vec<NotASizedStruct> = Vec::with_capacity(n as usize);
    for _ in 0..n {
        let same_elem: NotASizedStruct = read_struct_not_a_sized_struct();
        same.push(same_elem);
    }

    sized_struct(n, lists, strings, matrices, same);
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

fn read_struct_list() -> List {
    let size1: i32 = read_line().parse().unwrap();
    let int_list: Vec<i32> = read_vec_int();
    List { size1, int_list }
}

fn read_struct_string() -> String_ {
    let size2: i32 = read_line().parse().unwrap();
    let string_list: String = read_line();
    String_ { size2, string_list }
}

fn read_struct_matrix() -> Matrix {
    let size3: i32 = read_line().parse().unwrap();
    let mut list_list: Vec<Vec<i32>> = Vec::with_capacity(size3 as usize);
    for _ in 0..size3 {
        list_list.push(read_vec_int());
    }
    Matrix { size3, list_list }
}

fn read_struct_not_a_sized_struct() -> NotASizedStruct {
    let size4: i32 = read_line().parse().unwrap();
    let int_list_n: Vec<i32> = read_vec_int();
    NotASizedStruct { size4, int_list_n }
}
