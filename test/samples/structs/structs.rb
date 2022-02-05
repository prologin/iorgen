# +struct+:: a struct 1 instance
# +n+:: a number
# +struct_list+:: a list a struct 1
# +triangle+:: a triangle
# +struct_chars+:: a struct of chars
# +big_list_struct+:: the big list struct
def structs(struct, n, struct_list, triangle, struct_chars, big_list_struct)
    # TODO Look at them structs.
end

struct = Hash[["foo", "bar"].zip(STDIN.gets.split.map(&:to_i))]
n = STDIN.gets.to_i
struct_list = Array.new(n) { Hash[["foo", "bar"].zip(STDIN.gets.split.map(&:to_i))] }
triangle = Array.new(3) { {
    "name" => STDIN.gets[0],
    "description" => STDIN.gets.chomp,
    "pos" => Hash[["x", "y", "z"].zip(STDIN.gets.split.map(&:to_i))]
} }
struct_chars = Hash[["first char", "second char", "third char"].zip(STDIN.gets.chomp.split(" "))]
big_list_struct = {
    "int" => STDIN.gets.to_i,
    "big list" => Array.new(2) { Array.new(2) { STDIN.gets.split.map(&:to_i) } }
}

structs(struct, n, struct_list, triangle, struct_chars, big_list_struct)
