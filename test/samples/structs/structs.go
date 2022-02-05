package main

import "bufio"
import "fmt"
import "os"
import "strconv"
import "strings"

// A simple struct
type Struct1 struct {
    foo int // a field
    bar int // a field
}

// Represents a position
type Position struct {
    x int // X
    y int // Y
    z int // Z
}

// A point's name and position
type Point struct {
    name byte // the point's name (single character)
    description string // the point's description
    pos Position // the point's position
}

// a struct of chars
type Chars struct {
    firstChar byte // a first char
    secondChar byte // a second char
    thirdChar byte // a third char
}

// contains a big list inside
type WithList struct {
    int_ int // int
    bigList [][][]int // list nested 3 times!
}

// struct_: a struct 1 instance
// n: a number
// structList: a list a struct 1
// triangle: a triangle
// structChars: a struct of chars
// bigListStruct: the big list struct
func structs(struct_ Struct1, n int, structList []Struct1, triangle []Point, structChars Chars, bigListStruct WithList) {
    /* TODO Look at them structs. */
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    var struct_ Struct1
    scanner.Scan()
    fmt.Sscanf(scanner.Text(), "%d %d", &struct_.foo, &struct_.bar)
    var n int
    scanner.Scan()
    n, _ = strconv.Atoi(scanner.Text())
    structList := make([]Struct1, n)
    for i := range structList {
        scanner.Scan()
        fmt.Sscanf(scanner.Text(), "%d %d", &structList[i].foo, &structList[i].bar)
    }
    triangle := make([]Point, 3)
    for i := range triangle {
        scanner.Scan()
        triangle[i].name = scanner.Text()[0]
        scanner.Scan()
        triangle[i].description = scanner.Text()
        scanner.Scan()
        fmt.Sscanf(scanner.Text(), "%d %d %d", &triangle[i].pos.x, &triangle[i].pos.y, &triangle[i].pos.z)
    }
    var structChars Chars
    scanner.Scan()
    fmt.Sscanf(scanner.Text(), "%c %c %c", &structChars.firstChar, &structChars.secondChar, &structChars.thirdChar)
    var bigListStruct WithList
    scanner.Scan()
    bigListStruct.int_, _ = strconv.Atoi(scanner.Text())
    bigListStruct.bigList = make([][][]int, 2)
    for i := range bigListStruct.bigList {
        bigListStruct.bigList[i] = make([][]int, 2)
        for j := range bigListStruct.bigList[i] {
            bigListStruct.bigList[i][j] = make([]int, 2)
            scanner.Scan()
            for k, kValue := range strings.SplitN(scanner.Text(), " ", 2) {
                bigListStruct.bigList[i][j][k], _ = strconv.Atoi(kValue)
            }
        }
    }
    structs(struct_, n, structList, triangle, structChars, bigListStruct);
}
