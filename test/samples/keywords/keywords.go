package main

import "fmt"

// may conflict in c#
type Console struct {
    a int // the first letter of the alphabet
    static int // an integer
}

// may conflict in c#
type System struct {
    return_ int // not the end of the function
    void []int // not nothing
}

// not the main function
type Main struct {
    int System // not an integer
    ifTrue int // should not cause conflict
}

// if_: not a condition
// class: not a class
// i: just a string
// in: not in
// for_: not a loop
// words: contains lots of things
func keywords(if_ int, class byte, i string, in Console, for_ []int, words []Main) {
    /* TODO If this compiles, it is already a good step! */
}

func main() {
    var if_ int
    fmt.Scanln(&if_)
    var class byte
    fmt.Scanf("%c\n", &class)
    var i string
    fmt.Scanln(&i)
    var in Console
    fmt.Scanln(&in.a, &in.static)
    for_ := make([]int, if_)
    for j := range for_ {
        fmt.Scan(&for_[j])
    }
    words := make([]Main, 2)
    for j := range words {
        fmt.Scanln(&words[j].int.return_)
        words[j].int.void = make([]int, 3)
        for k := range words[j].int.void {
            fmt.Scan(&words[j].int.void[k])
        }
        fmt.Scanln(&words[j].ifTrue)
    }
    keywords(if_, class, i, in, for_, words);
}
