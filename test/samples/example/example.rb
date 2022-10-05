# +n+:: a number, used as a size
# +list+:: a list of structs
def example(n, list)
    # TODO In a real life scenario, you will describe here what you want the
    # end user to do with this generated code
end

n = STDIN.gets.to_i
list = Array.new(n) {
    Hash[["integer", "character"].zip(STDIN.gets.split, [:to_i, :to_s]).map{ |k,v,s| [k, v.send(s)] }]
}

example(n, list)
