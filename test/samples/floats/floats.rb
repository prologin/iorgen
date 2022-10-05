# +f+:: a float
# +g+:: a float, greater than f
# +point+:: some coordinates
# +n+:: a number
# +float_list+:: a list of floats
# +other_list+:: a list of floats
# +inlined+:: some inlined structs
# +multiline+:: a multiline struct
def floats(f, g, point, n, float_list, other_list, inlined, multiline)
    # TODO Parsing is often easy, reprint mode is harder
end

f = STDIN.gets.to_f
g = STDIN.gets.to_f
point = Hash[["x", "y", "z"].zip(STDIN.gets.split.map(&:to_f))]
n = STDIN.gets.to_i
float_list = STDIN.gets.split.map(&:to_f)
other_list = STDIN.gets.split.map(&:to_f)
inlined = Array.new(3) {
    Hash[["integer", "char", "float"].zip(STDIN.gets.split, [:to_i, :to_s, :to_f]).map{ |k,v,s| [k, v.send(s)] }]
}
multiline = {
    "integer 2" => STDIN.gets.to_i,
    "string" => STDIN.gets.chomp,
    "float 2" => STDIN.gets.to_f
}

floats(f, g, point, n, float_list, other_list, inlined, multiline)
