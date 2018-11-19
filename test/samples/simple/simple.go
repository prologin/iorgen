package main

import "bufio"
import "os"
import "strconv"

// n: the first number
// otherNumber: the second number
func simple(n int, otherNumber int) {
    /* TODO Just do what you want with these numbers, like sum them. */
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    var n int
    scanner.Scan()
    n, _ = strconv.Atoi(scanner.Text())
    var otherNumber int
    scanner.Scan()
    otherNumber, _ = strconv.Atoi(scanner.Text())
    simple(n, otherNumber);
}
