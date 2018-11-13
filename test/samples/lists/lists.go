package main

import "fmt"

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
    var n int
    fmt.Scanln(&n)
    listInt := make([]int, n)
    for i := range listInt {
        fmt.Scan(&listInt[i])
    }
    var size int
    fmt.Scanln(&size)
    var listChar []byte
    fmt.Scanln(&listChar)
    listString4 := make([]string, size)
    for i := range listString4 {
        fmt.Scanln(&listString4[i])
    }
    matrix := make([][]int, size)
    for i := range matrix {
        matrix[i] = make([]int, size)
        for j := range matrix[i] {
            fmt.Scan(&matrix[i][j])
        }
    }
    lists(n, listInt, size, listChar, listString4, matrix);
}
