import Base: parse

"""
Represents coordinates
"""
struct Coordinates
    """X"""
    x::Float64
    """Y"""
    y::Float64
    """Z"""
    z::Float64
end


"""
Mix of fields that go on one line
"""
struct InlinedMix
    """an integer"""
    integer::Int
    """a char"""
    char::Char
    """a float"""
    float::Float64
end


"""
a struct of chars
"""
struct MultilineMix
    """an other integer"""
    integer_2::Int
    """a string of size 5"""
    string::String
    """an other float"""
    float_2::Float64
end


"""
- `f::Float64`: a float
- `g::Float64`: a float, greater than f
- `point::Coordinates`: some coordinates
- `n::Int`: a number
- `float_list::Vector{Float64}`: a list of floats
- `other_list::Vector{Float64}`: a list of floats
- `inlined::Vector{InlinedMix}`: some inlined structs
- `multiline::MultilineMix`: a multiline struct
"""
function floats(f::Float64, g::Float64, point::Coordinates, n::Int, float_list::Vector{Float64}, other_list::Vector{Float64}, inlined::Vector{InlinedMix}, multiline::MultilineMix)
    # TODO Parsing is often easy, reprint mode is harder
end

function parse(::Type{Coordinates}, s::AbstractString)
    s = split(s)
    Coordinates(parse(Float64, s[1]), parse(Float64, s[2]), parse(Float64, s[3]))
end

function parse(::Type{InlinedMix}, s::AbstractString)
    s = split(s)
    InlinedMix(parse(Int, s[1]), s[2][1], parse(Float64, s[3]))
end

function read_struct_multiline_mix()
    integer_2 = parse(Int, readline())
    string = readline()
    float_2 = parse(Float64, readline())
    MultilineMix(integer_2, string, float_2)
end

f = parse(Float64, readline())
g = parse(Float64, readline())
point = parse(Coordinates, readline())
n = parse(Int, readline())
float_list = map(s -> parse(Float64, s), split(readline()))
other_list = map(s -> parse(Float64, s), split(readline()))
inlined = [ parse(InlinedMix, readline()) for _=1:3 ]
multiline = read_struct_multiline_mix()
floats(f, g, point, n, float_list, other_list, inlined, multiline)
