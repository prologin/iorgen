package main

import "bufio"
import "os"
import "strconv"
import "strings"

// contains a list
type List struct {
    size1 int // the list's size
    intList []int // the integer list
}

// contains a string
type String struct {
    size2 int // the list's size
    stringList string // the string list
}

// contains a matrix
type Matrix struct {
    size3 int // the list's size
    listList [][]int // the list list
}

// this is not a 'sized struct', but a regular one!
type NotASizedStruct struct {
    size4 int // not the list's size
    intListN []int // the integer list
}

// n: the size of the lists
// lists: a list of list of different sizes
// strings_: a list of strings of different sizes
// matrices: a list of matrices of different sizes
// same: a list of list of same sizes
func sizedStruct(n int, lists []List, strings_ []String, matrices []Matrix, same []NotASizedStruct) {
    /* TODO The is a special case. */
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    var n int
    scanner.Scan()
    n, _ = strconv.Atoi(scanner.Text())
    lists := make([]List, n)
    for i := range lists {
        scanner.Scan()
        lists[i].size1, _ = strconv.Atoi(scanner.Text())
        lists[i].intList = make([]int, lists[i].size1)
        scanner.Scan()
        for j, jValue := range strings.Split(scanner.Text(), " ") {
            lists[i].intList[j], _ = strconv.Atoi(jValue)
        }
    }
    strings_ := make([]String, n)
    for i := range strings_ {
        scanner.Scan()
        strings_[i].size2, _ = strconv.Atoi(scanner.Text())
        scanner.Scan()
        strings_[i].stringList = scanner.Text()
    }
    matrices := make([]Matrix, 2)
    for i := range matrices {
        scanner.Scan()
        matrices[i].size3, _ = strconv.Atoi(scanner.Text())
        matrices[i].listList = make([][]int, matrices[i].size3)
        for j := range matrices[i].listList {
            matrices[i].listList[j] = make([]int, 2)
            scanner.Scan()
            for k, kValue := range strings.Split(scanner.Text(), " ") {
                matrices[i].listList[j][k], _ = strconv.Atoi(kValue)
            }
        }
    }
    same := make([]NotASizedStruct, n)
    for i := range same {
        scanner.Scan()
        same[i].size4, _ = strconv.Atoi(scanner.Text())
        same[i].intListN = make([]int, n)
        scanner.Scan()
        for j, jValue := range strings.Split(scanner.Text(), " ") {
            same[i].intListN[j], _ = strconv.Atoi(jValue)
        }
    }
    sizedStruct(n, lists, strings_, matrices, same);
}
