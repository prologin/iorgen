# +n+:: the first list's size
# +list_int+:: a list containing ints
# +size+:: an other size
# +list_char+:: a list of char
# +list_string4+:: a list of strings of size 4
# +matrix+:: a matrix of int
def lists(n, list_int, size, list_char, list_string4, matrix)
    # TODO Aren't these lists beautifull?
end

n = STDIN.gets.to_i
list_int = STDIN.gets.split.map(&:to_i)
size = STDIN.gets.to_i
list_char = STDIN.gets.chomp.split("")
list_string4 = Array.new(size) { STDIN.gets.chomp }
matrix = Array.new(size) { STDIN.gets.split.map(&:to_i) }

lists(n, list_int, size, list_char, list_string4, matrix)
