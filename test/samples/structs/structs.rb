# +struct+:: a struct 1 instance
# +n+:: a number
# +struct_list+:: a list a struct 1
# +triangle+:: a triangle
def structs(struct, n, struct_list, triangle)
    # TODO Look at them structs.
end

struct = Hash[["foo", "bar"].zip(STDIN.gets.split.map(&:to_i))]
n = STDIN.gets.to_i
struct_list = Array.new(n) { Hash[["foo", "bar"].zip(STDIN.gets.split.map(&:to_i))] }
triangle = Array.new(3) { {
    "name" => STDIN.gets[0],
    "pos" => Hash[["x", "y", "z"].zip(STDIN.gets.split.map(&:to_i))]
} }

structs(struct, n, struct_list, triangle)
