import Base: parse

"""
A struct for the example
"""
struct AStruct
    """an integer"""
    integer::Int
    """a char"""
    character::Char
end


"""
- `n::Int`: a number, used as a size
- `list::Vector{AStruct}`: a list of structs
"""
function example(n::Int, list::Vector{AStruct})
    # TODO In a real life scenario, you will describe here what you want the
    # end user to do with this generated code
end

function parse(::Type{AStruct}, s::AbstractString)
    s = split(s)
    AStruct(parse(Int, s[1]), s[2][1])
end

n = parse(Int, readline())
list = [ parse(AStruct, readline()) for _=1:n ]
example(n, list)
