package main

import "bufio"
import "fmt"
import "os"
import "strconv"

// A struct for the example
type AStruct struct {
    integer int // an integer
    character byte // a char
}

// n: a number, used as a size
// list: a list of structs
func example(n int, list []AStruct) {
    /* TODO In a real life scenario, you will describe here what you want the
    end user to do with this generated code */
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    var n int
    scanner.Scan()
    n, _ = strconv.Atoi(scanner.Text())
    list := make([]AStruct, n)
    for i := range list {
        scanner.Scan()
        fmt.Sscanf(scanner.Text(), "%d %c", &list[i].integer, &list[i].character)
    }
    example(n, list);
}
