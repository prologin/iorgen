# +a+:: a first number
# +b+:: a second number
# +c+:: a third number
# +n+:: This one on a new line
# +one_per_line+:: an integer list, one per line
def manual_format(a, b, c, n, one_per_line)
    # TODO From the function perspective, this is just 4 integers
end

a, b, c = STDIN.gets.split.map(&:to_i)
n = STDIN.gets.to_i
one_per_line = Array.new(3) { STDIN.gets.to_i }

manual_format(a, b, c, n, one_per_line)
