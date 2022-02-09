package main

import "bufio"
import "fmt"
import "os"
import "strconv"

// a: a first number
// b: a second number
// c: a third number
// n: This one on a new line
// onePerLine: an integer list, one per line
func manualFormat(a int, b int, c int, n int, onePerLine []int) {
    /* TODO From the function perspective, this is just 4 integers */
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    var a, b, c int
    scanner.Scan()
    fmt.Sscanf(scanner.Text(), "%d %d %d", &a, &b, &c);
    var n int
    scanner.Scan()
    n, _ = strconv.Atoi(scanner.Text())
    onePerLine := make([]int, 3)
    for i := range onePerLine {
        scanner.Scan()
        onePerLine[i], _ = strconv.Atoi(scanner.Text())
    }
    manualFormat(a, b, c, n, onePerLine);
}
