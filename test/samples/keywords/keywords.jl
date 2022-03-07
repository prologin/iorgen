import Base: parse

"""
may conflict in c#
"""
struct Console
    """the first letter of the alphabet"""
    a::Int
    """an integer"""
    static::Int
end


"""
may conflict in c#
"""
struct System
    """not the end of the function"""
    return_::Int
    """not nothing"""
    void::Vector{Int}
end


"""
not the main function
"""
struct Main_
    """not an integer"""
    int::System
    """should not cause conflict"""
    if_true::Int
end


"""
- `if_::Int`: not a condition
- `class::Char`: not a class
- `i::String`: just a string
- `in::Console`: not in
- `for_::Vector{Int}`: not a loop
- `words::Vector{Main_}`: contains lots of things
- `words_1::Int`: an integer
"""
function keywords(if_::Int, class::Char, i::String, in::Console, for_::Vector{Int}, words::Vector{Main_}, words_1::Int)
    # TODO If this compiles, it is already a good step!
end

function parse(::Type{Console}, s::AbstractString)
    s = split(s)
    Console(parse(Int, s[1]), parse(Int, s[2]))
end

function read_struct_system()
    return_ = parse(Int, readline())
    void = map(s -> parse(Int, s), split(readline()))
    System(return_, void)
end

function read_struct_main()
    int = read_struct_system()
    if_true = parse(Int, readline())
    Main_(int, if_true)
end

if_ = parse(Int, readline())
class = readline()[1]
i = readline()
in = parse(Console, readline())
for_ = map(s -> parse(Int, s), split(readline()))
words = [ read_struct_main() for _=1:2 ]
words_1 = parse(Int, readline())
keywords(if_, class, i, in, for_, words, words_1)
