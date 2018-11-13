package main

import "fmt"

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
    pos Position // the point's position
}

// struct_: a struct 1 instance
// n: a number
// structList: a list a struct 1
// triangle: a triangle
func structs(struct_ Struct1, n int, structList []Struct1, triangle []Point) {
    /* TODO Look at them structs. */
}

func main() {
    var struct_ Struct1
    fmt.Scanln(&struct_.foo, &struct_.bar)
    var n int
    fmt.Scanln(&n)
    structList := make([]Struct1, n)
    for i := range structList {
        fmt.Scanln(&structList[i].foo, &structList[i].bar)
    }
    triangle := make([]Point, 3)
    for i := range triangle {
        fmt.Scanf("%c\n", &triangle[i].name)
        fmt.Scanln(&triangle[i].pos.x, &triangle[i].pos.y, &triangle[i].pos.z)
    }
    structs(struct_, n, structList, triangle);
}
