# +if_+:: not a condition
# +class_+:: not a class
# +i+:: just a string
# +in_+:: not in
# +for_+:: not a loop
# +words+:: contains lots of things
def keywords(if_, class_, i, in_, for_, words)
    # TODO If this compiles, it is already a good step!
end

if_ = STDIN.gets.to_i
class_ = STDIN.gets[0]
i = STDIN.gets.chomp
in_ = Hash[["a", "static"].zip(STDIN.gets.split.map(&:to_i))]
for_ = STDIN.gets.split.map(&:to_i)
words = Array.new(2) { {
    "int" => {
        "return" => STDIN.gets.to_i,
        "void" => STDIN.gets.split.map(&:to_i)
    },
    "if true" => STDIN.gets.to_i
} }

keywords(if_, class_, i, in_, for_, words)
