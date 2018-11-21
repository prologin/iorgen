# +n+:: the size of the lists
# +lists+:: a list of list of different sizes
# +strings+:: a list of strings of different sizes
# +matrices+:: a list of matrices of different sizes
# +same+:: a list of list of same sizes
def sized_struct(n, lists, strings, matrices, same)
    # TODO The is a special case.
end

n = STDIN.gets.to_i
lists = Array.new(n) {
    (lambda { |i| {
        "size1" => i,
        "int list" => STDIN.gets.split.map(&:to_i)
    } }).call(STDIN.gets.to_i)
}
strings = Array.new(n) {
    (lambda { |i| {
        "size2" => i,
        "string list" => STDIN.gets.chomp
    } }).call(STDIN.gets.to_i)
}
matrices = Array.new(2) {
    (lambda { |i| {
        "size3" => i,
        "list list" => Array.new(i) { STDIN.gets.split.map(&:to_i) }
    } }).call(STDIN.gets.to_i)
}
same = Array.new(n) { {
    "size4" => STDIN.gets.to_i,
    "int list n" => STDIN.gets.split.map(&:to_i)
} }

sized_struct(n, lists, strings, matrices, same)
