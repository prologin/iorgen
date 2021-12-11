import Base: parse

"""
a char struct
"""
struct StructWithAChar
    """a char"""
    char1::Char
    """an integer"""
    int2::Int
end


"""
a struct
"""
struct A
    """a list in a struct"""
    list_in_struct::Vector{Int}
    """a struct in a struct"""
    struct_in_struct::StructWithAChar
end


"""
a sized struct
"""
struct SizedStruct
    """the size"""
    size::Int
    """the string"""
    string_in_struct::String
end


"""
- `empty_list::Vector{Int}`: an empty list
- `buffer_string::String`: here to check correct parsing of empty line above
- `n::Int`: an integer, will be 0 in the sample input
- `empty_in_sample::Vector{Int}`: an empty list (only in the sample)
- `empty_string::String`: an empty string
- `main::String`: an other buffer string
- `empty_char_list::Vector{Char}`: an empty char list
- `non_empty_char_list::Vector{Char}`: an char list, non empty
- `struct_with_empty_line::A`: a struct containing an empty line, then a struct
- `a_sized_struct::SizedStruct`: a sized struct containing an empty line
- `finish::String`: a string to finish
"""
function empty_lines(empty_list::Vector{Int}, buffer_string::String, n::Int, empty_in_sample::Vector{Int}, empty_string::String, main::String, empty_char_list::Vector{Char}, non_empty_char_list::Vector{Char}, struct_with_empty_line::A, a_sized_struct::SizedStruct, finish::String)
    # TODO Wow, lots of empty lines!
end

function parse(::Type{StructWithAChar}, s::AbstractString)
    s = split(s)
    StructWithAChar(s[1][1], parse(Int, s[2]))
end

function read_struct_a()
    list_in_struct = map(s -> parse(Int, s), split(readline()))
    struct_in_struct = parse(StructWithAChar, readline())
    A(list_in_struct, struct_in_struct)
end

function read_struct_sized_struct()
    size = parse(Int, readline())
    string_in_struct = readline()
    SizedStruct(size, string_in_struct)
end

empty_list = map(s -> parse(Int, s), split(readline()))
buffer_string = readline()
n = parse(Int, readline())
empty_in_sample = map(s -> parse(Int, s), split(readline()))
empty_string = readline()
main = readline()
empty_char_list = collect(readline())
non_empty_char_list = collect(readline())
struct_with_empty_line = read_struct_a()
a_sized_struct = read_struct_sized_struct()
finish = readline()
empty_lines(empty_list, buffer_string, n, empty_in_sample, empty_string, main, empty_char_list, non_empty_char_list, struct_with_empty_line, a_sized_struct, finish)
