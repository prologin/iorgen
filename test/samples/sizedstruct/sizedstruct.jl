"""
contains a list
"""
struct List
    """the list's size"""
    size1::Int
    """the integer list"""
    int_list::Vector{Int}
end


"""
contains a string
"""
struct String_
    """the list's size"""
    size2::Int
    """the string list"""
    string_list::String
end


"""
contains a matrix
"""
struct Matrix
    """the list's size"""
    size3::Int
    """the list list"""
    list_list::Vector{Vector{Int}}
end


"""
this is not a 'sized struct', but a regular one!
"""
struct NotASizedStruct
    """not the list's size"""
    size4::Int
    """the integer list"""
    int_list_n::Vector{Int}
end


"""
- `n::Int`: the size of the lists
- `lists::Vector{List}`: a list of list of different sizes
- `strings::Vector{String_}`: a list of strings of different sizes
- `matrices::Vector{Matrix}`: a list of matrices of different sizes
- `same::Vector{NotASizedStruct}`: a list of list of same sizes
"""
function sized_struct(n::Int, lists::Vector{List}, strings::Vector{String_}, matrices::Vector{Matrix}, same::Vector{NotASizedStruct})
    # TODO The is a special case.
end

function read_struct_list()
    size1 = parse(Int, readline())
    int_list = map(s -> parse(Int, s), split(readline()))
    List(size1, int_list)
end

function read_struct_string()
    size2 = parse(Int, readline())
    string_list = readline()
    String_(size2, string_list)
end

function read_struct_matrix()
    size3 = parse(Int, readline())
    list_list = [ map(s -> parse(Int, s), split(readline())) for _=1:size3 ]
    Matrix(size3, list_list)
end

function read_struct_not_a_sized_struct()
    size4 = parse(Int, readline())
    int_list_n = map(s -> parse(Int, s), split(readline()))
    NotASizedStruct(size4, int_list_n)
end

n = parse(Int, readline())
lists = [ read_struct_list() for _=1:n ]
strings = [ read_struct_string() for _=1:n ]
matrices = [ read_struct_matrix() for _=1:2 ]
same = [ read_struct_not_a_sized_struct() for _=1:n ]
sized_struct(n, lists, strings, matrices, same)
