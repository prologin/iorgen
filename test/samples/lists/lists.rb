# +n+:: the first list's size
# +list_int+:: a list containing ints
# +size+:: an other size
# +list_char+:: a list of char
# +string+:: a string
# +list_string4+:: a list of strings of size 4
# +list_list_string2+:: a list of list of strings of size 2 of size 2 of size 2
# +matrix+:: a matrix of int
def lists(n, list_int, size, list_char, string, list_string4, list_list_string2, matrix)
    # TODO Aren't these lists beautifull?
end

n = STDIN.gets.to_i
list_int = STDIN.gets.split.map(&:to_i)
size = STDIN.gets.to_i
list_char = STDIN.gets.chomp.split("")
string = STDIN.gets.chomp
list_string4 = Array.new(size) { STDIN.gets.chomp }
list_list_string2 = Array.new(2) { Array.new(2) { STDIN.gets.chomp } }
matrix = Array.new(size) { STDIN.gets.split.map(&:to_i) }

lists(n, list_int, size, list_char, string, list_string4, list_list_string2, matrix)
