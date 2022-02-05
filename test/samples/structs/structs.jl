import Base: parse

"""
A simple struct
"""
struct Struct1
    """a field"""
    foo::Int
    """a field"""
    bar::Int
end


"""
Represents a position
"""
struct Position
    """X"""
    x::Int
    """Y"""
    y::Int
    """Z"""
    z::Int
end


"""
A point's name and position
"""
struct Point
    """the point's name (single character)"""
    name::Char
    """the point's description"""
    description::String
    """the point's position"""
    pos::Position
end


"""
a struct of chars
"""
struct Chars
    """a first char"""
    first_char::Char
    """a second char"""
    second_char::Char
    """a third char"""
    third_char::Char
end


"""
contains a big list inside
"""
struct WithList
    """int"""
    int::Int
    """list nested 3 times!"""
    big_list::Vector{Vector{Vector{Int}}}
end


"""
- `struct_::Struct1`: a struct 1 instance
- `n::Int`: a number
- `struct_list::Vector{Struct1}`: a list a struct 1
- `triangle::Vector{Point}`: a triangle
- `struct_chars::Chars`: a struct of chars
- `big_list_struct::WithList`: the big list struct
"""
function structs(struct_::Struct1, n::Int, struct_list::Vector{Struct1}, triangle::Vector{Point}, struct_chars::Chars, big_list_struct::WithList)
    # TODO Look at them structs.
end

function parse(::Type{Struct1}, s::AbstractString)
    s = split(s)
    Struct1(parse(Int, s[1]), parse(Int, s[2]))
end

function parse(::Type{Position}, s::AbstractString)
    s = split(s)
    Position(parse(Int, s[1]), parse(Int, s[2]), parse(Int, s[3]))
end

function read_struct_point()
    name = readline()[1]
    description = readline()
    pos = parse(Position, readline())
    Point(name, description, pos)
end

function parse(::Type{Chars}, s::AbstractString)
    s = split(s)
    Chars(s[1][1], s[2][1], s[3][1])
end

function read_struct_with_list()
    int = parse(Int, readline())
    big_list = [ [ map(s -> parse(Int, s), split(readline())) for _=1:2 ] for _=1:2 ]
    WithList(int, big_list)
end

struct_ = parse(Struct1, readline())
n = parse(Int, readline())
struct_list = [ parse(Struct1, readline()) for _=1:n ]
triangle = [ read_struct_point() for _=1:3 ]
struct_chars = parse(Chars, readline())
big_list_struct = read_struct_with_list()
structs(struct_, n, struct_list, triangle, struct_chars, big_list_struct)
