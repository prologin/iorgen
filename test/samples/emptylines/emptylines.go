package main

import "bufio"
import "fmt"
import "os"
import "strconv"
import "strings"

// a char struct
type StructWithAChar struct {
    char1 byte // a char
    int2 int // an integer
}

// a struct
type A struct {
    listInStruct []int // a list in a struct
    structInStruct StructWithAChar // a struct in a struct
}

// a sized struct
type SizedStruct struct {
    size int // the size
    stringInStruct string // the string
}

// emptyList: an empty list
// bufferString: here to check correct parsing of empty line above
// n: an integer, will be 0 in the sample input
// emptyInSample: an empty list (only in the sample)
// emptyString: an empty string
// main_: an other buffer string
// emptyCharList: an empty char list
// nonEmptyCharList: an char list, non empty
// structWithEmptyLine: a struct containing an empty line, then a struct
// aSizedStruct: a sized struct containing an empty line
// finish: a string to finish
func emptyLines(emptyList []int, bufferString string, n int, emptyInSample []int, emptyString string, main_ string, emptyCharList []byte, nonEmptyCharList []byte, structWithEmptyLine A, aSizedStruct SizedStruct, finish string) {
    /* TODO Wow, lots of empty lines! */
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    scanner.Buffer(make([]byte, 0, 64 * 1024), 25769803764)
    emptyList := make([]int, 0)
    scanner.Scan()
    for i, iValue := range strings.SplitN(scanner.Text(), " ", 0) {
        emptyList[i], _ = strconv.Atoi(iValue)
    }
    var bufferString string
    scanner.Scan()
    bufferString = scanner.Text()
    var n int
    scanner.Scan()
    n, _ = strconv.Atoi(scanner.Text())
    emptyInSample := make([]int, n)
    scanner.Scan()
    for i, iValue := range strings.SplitN(scanner.Text(), " ", n) {
        emptyInSample[i], _ = strconv.Atoi(iValue)
    }
    var emptyString string
    scanner.Scan()
    emptyString = scanner.Text()
    var main_ string
    scanner.Scan()
    main_ = scanner.Text()
    var emptyCharList []byte
    scanner.Scan()
    emptyCharList = scanner.Bytes()
    var nonEmptyCharList []byte
    scanner.Scan()
    nonEmptyCharList = scanner.Bytes()
    var structWithEmptyLine A
    structWithEmptyLine.listInStruct = make([]int, n)
    scanner.Scan()
    for i, iValue := range strings.SplitN(scanner.Text(), " ", n) {
        structWithEmptyLine.listInStruct[i], _ = strconv.Atoi(iValue)
    }
    scanner.Scan()
    fmt.Sscanf(scanner.Text(), "%c %d", &structWithEmptyLine.structInStruct.char1, &structWithEmptyLine.structInStruct.int2)
    var aSizedStruct SizedStruct
    scanner.Scan()
    aSizedStruct.size, _ = strconv.Atoi(scanner.Text())
    scanner.Scan()
    aSizedStruct.stringInStruct = scanner.Text()
    var finish string
    scanner.Scan()
    finish = scanner.Text()
    emptyLines(emptyList, bufferString, n, emptyInSample, emptyString, main_, emptyCharList, nonEmptyCharList, structWithEmptyLine, aSizedStruct, finish);
}
