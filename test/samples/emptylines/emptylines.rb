# +empty_list+:: an empty list
# +buffer_string+:: here to check correct parsing of empty line above
# +n+:: an integer, will be 0 in the sample input
# +empty_in_sample+:: an empty list (only in the sample)
# +empty_string+:: an empty string
# +main+:: an other buffer string
# +empty_char_list+:: an empty char list
# +non_empty_char_list+:: an char list, non empty
# +struct_with_empty_line+:: a struct containing an empty line, then a struct
# +a_sized_struct+:: a sized struct containing an empty line
# +finish+:: a string to finish
def empty_lines(empty_list, buffer_string, n, empty_in_sample, empty_string, main, empty_char_list, non_empty_char_list, struct_with_empty_line, a_sized_struct, finish)
    # TODO Wow, lots of empty lines!
end

empty_list = STDIN.gets.split.map(&:to_i)
buffer_string = STDIN.gets.chomp
n = STDIN.gets.to_i
empty_in_sample = STDIN.gets.split.map(&:to_i)
empty_string = STDIN.gets.chomp
main = STDIN.gets.chomp
empty_char_list = STDIN.gets.chomp.split("")
non_empty_char_list = STDIN.gets.chomp.split("")
struct_with_empty_line = {
    "list in struct" => STDIN.gets.split.map(&:to_i),
    "struct in struct" => Hash[["char1", "int2"].zip(STDIN.gets.split, [:to_s, :to_i]).map{ |k,v,s| [k, v.send(s)] }]
}
a_sized_struct = (lambda { |i| {
    "size" => i,
    "string in struct" => STDIN.gets.chomp
} }).call(STDIN.gets.to_i)
finish = STDIN.gets.chomp

empty_lines(empty_list, buffer_string, n, empty_in_sample, empty_string, main, empty_char_list, non_empty_char_list, struct_with_empty_line, a_sized_struct, finish)
