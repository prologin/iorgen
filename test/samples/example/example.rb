# +n+:: a number, used as a size
# +list+:: a list of structs
def example(n, list)
    # TODO In a real life scenario, you will describe here what you want the
    # end user to do with this generated code
end

n = STDIN.gets.to_i
list = Array.new(n) {
    Hash[["integer", "character"].zip([1, 0], STDIN.gets.split).map{ |x,y,z| [x, y == 1 ? z.to_i : z] }]
}

example(n, list)
