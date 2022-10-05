package main

import "bufio"
import "fmt"
import "os"
import "strconv"
import "strings"

// Represents coordinates
type Coordinates struct {
    x float64 // X
    y float64 // Y
    z float64 // Z
}

// Mix of fields that go on one line
type InlinedMix struct {
    integer int // an integer
    char byte // a char
    float float64 // a float
}

// a struct of chars
type MultilineMix struct {
    integer2 int // an other integer
    string_ string // a string of size 5
    float2 float64 // an other float
}

// f: a float
// g: a float, greater than f
// point: some coordinates
// n: a number
// floatList: a list of floats
// otherList: a list of floats
// inlined: some inlined structs
// multiline: a multiline struct
func floats(f float64, g float64, point Coordinates, n int, floatList []float64, otherList []float64, inlined []InlinedMix, multiline MultilineMix) {
    /* TODO Parsing is often easy, reprint mode is harder */
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    var f float64
    scanner.Scan()
    f, _ = strconv.ParseFloat(scanner.Text(), 64)
    var g float64
    scanner.Scan()
    g, _ = strconv.ParseFloat(scanner.Text(), 64)
    var point Coordinates
    scanner.Scan()
    fmt.Sscanf(scanner.Text(), "%g %g %g", &point.x, &point.y, &point.z)
    var n int
    scanner.Scan()
    n, _ = strconv.Atoi(scanner.Text())
    floatList := make([]float64, n)
    scanner.Scan()
    for i, iValue := range strings.SplitN(scanner.Text(), " ", n) {
        floatList[i], _ = strconv.ParseFloat(iValue, 64)
    }
    otherList := make([]float64, 9)
    scanner.Scan()
    for i, iValue := range strings.SplitN(scanner.Text(), " ", 9) {
        otherList[i], _ = strconv.ParseFloat(iValue, 64)
    }
    inlined := make([]InlinedMix, 3)
    for i := range inlined {
        scanner.Scan()
        fmt.Sscanf(scanner.Text(), "%d %c %g", &inlined[i].integer, &inlined[i].char, &inlined[i].float)
    }
    var multiline MultilineMix
    scanner.Scan()
    multiline.integer2, _ = strconv.Atoi(scanner.Text())
    scanner.Scan()
    multiline.string_ = scanner.Text()
    scanner.Scan()
    multiline.float2, _ = strconv.ParseFloat(scanner.Text(), 64)
    floats(f, g, point, n, floatList, otherList, inlined, multiline);
}
