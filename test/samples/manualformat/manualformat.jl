"""
- `a::Int`: a first number
- `b::Int`: a second number
- `c::Int`: a third number
- `n::Int`: This one on a new line
- `one_per_line::Vector{Int}`: an integer list, one per line
"""
function manual_format(a::Int, b::Int, c::Int, n::Int, one_per_line::Vector{Int})
    # TODO From the function perspective, this is just 4 integers
end

a, b, c = map(s -> parse(Int, s), split(readline()))
n = parse(Int, readline())
one_per_line = [ parse(Int, readline()) for _=1:3 ]
manual_format(a, b, c, n, one_per_line)
