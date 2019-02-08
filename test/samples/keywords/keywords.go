package main

import "bufio"
import "fmt"
import "os"
import "strconv"
import "strings"

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
    scanner := bufio.NewScanner(os.Stdin)
    scanner.Buffer(make([]byte, 0, 64 * 1024), 8589934588)
    var if_ int
    scanner.Scan()
    if_, _ = strconv.Atoi(scanner.Text())
    var class byte
    scanner.Scan()
    class = scanner.Text()[0]
    var i string
    scanner.Scan()
    i = scanner.Text()
    var in Console
    scanner.Scan()
    fmt.Sscanf(scanner.Text(), "%d %d", &in.a, &in.static)
    for_ := make([]int, if_)
    scanner.Scan()
    for j, jValue := range strings.SplitN(scanner.Text(), " ", if_) {
        for_[j], _ = strconv.Atoi(jValue)
    }
    words := make([]Main, 2)
    for j := range words {
        scanner.Scan()
        words[j].int.return_, _ = strconv.Atoi(scanner.Text())
        words[j].int.void = make([]int, 3)
        scanner.Scan()
        for k, kValue := range strings.SplitN(scanner.Text(), " ", 3) {
            words[j].int.void[k], _ = strconv.Atoi(kValue)
        }
        scanner.Scan()
        words[j].ifTrue, _ = strconv.Atoi(scanner.Text())
    }
    keywords(if_, class, i, in, for_, words);
}
