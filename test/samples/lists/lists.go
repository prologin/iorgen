package main

import "bufio"
import "os"
import "strconv"
import "strings"

// n: the first list's size
// listInt: a list containing ints
// size: an other size
// listChar: a list of char
// listString4: a list of strings of size 4
// matrix: a matrix of int
func lists(n int, listInt []int, size int, listChar []byte, listString4 []string, matrix [][]int) {
    /* TODO Aren't these lists beautifull? */
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    var n int
    scanner.Scan()
    n, _ = strconv.Atoi(scanner.Text())
    listInt := make([]int, n)
    scanner.Scan()
    for i, iValue := range strings.SplitN(scanner.Text(), " ", n) {
        listInt[i], _ = strconv.Atoi(iValue)
    }
    var size int
    scanner.Scan()
    size, _ = strconv.Atoi(scanner.Text())
    var listChar []byte
    scanner.Scan()
    listChar = scanner.Bytes()
    listString4 := make([]string, size)
    for i := range listString4 {
        scanner.Scan()
        listString4[i] = scanner.Text()
    }
    matrix := make([][]int, size)
    for i := range matrix {
        matrix[i] = make([]int, size)
        scanner.Scan()
        for j, jValue := range strings.SplitN(scanner.Text(), " ", size) {
            matrix[i][j], _ = strconv.Atoi(jValue)
        }
    }
    lists(n, listInt, size, listChar, listString4, matrix);
}
