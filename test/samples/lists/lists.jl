"""
- `n::Int`: the first list's size
- `list_int::Vector{Int}`: a list containing ints
- `size::Int`: an other size
- `list_char::Vector{Char}`: a list of char
- `string::String`: a string
- `list_string4::Vector{String}`: a list of strings of size 4
- `list_list_string2::Vector{Vector{String}}`: a list of list of strings of size 2 of size 2 of size 2
- `matrix::Vector{Vector{Int}}`: a matrix of int
"""
function lists(n::Int, list_int::Vector{Int}, size::Int, list_char::Vector{Char}, string::String, list_string4::Vector{String}, list_list_string2::Vector{Vector{String}}, matrix::Vector{Vector{Int}})
    # TODO Aren't these lists beautifull?
end

n = parse(Int, readline())
list_int = map(s -> parse(Int, s), split(readline()))
size = parse(Int, readline())
list_char = collect(readline())
string = readline()
list_string4 = [ readline() for _=1:size ]
list_list_string2 = [ [ readline() for _=1:2 ] for _=1:2 ]
matrix = [ map(s -> parse(Int, s), split(readline())) for _=1:size ]
lists(n, list_int, size, list_char, string, list_string4, list_list_string2, matrix)
