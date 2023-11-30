import java.io.BufferedReader
import java.io.InputStreamReader

/**
 * a char struct

 * @property char1 a char
 * @property int2 an integer
 */
data class StructWithAChar(val char1: Char, val int2: Int)

/**
 * a struct

 * @property listInStruct a list in a struct
 * @property structInStruct a struct in a struct
 */
data class A(val listInStruct: List<Int>, val structInStruct: StructWithAChar)

/**
 * a sized struct

 * @property size the size
 * @property stringInStruct the string
 */
data class SizedStruct(val size: Int, val stringInStruct: String)

/**
 * @param emptyList an empty list
 * @param bufferString here to check correct parsing of empty line above
 * @param n an integer, will be 0 in the sample input
 * @param emptyInSample an empty list (only in the sample)
 * @param emptyString an empty string
 * @param main an other buffer string
 * @param emptyCharList an empty char list
 * @param nonEmptyCharList an char list, non empty
 * @param structWithEmptyLine a struct containing an empty line, then a struct
 * @param aSizedStruct a sized struct containing an empty line
 * @param finish a string to finish
 */
fun emptyLines(
    emptyList: List<Int>,
    bufferString: String,
    n: Int,
    emptyInSample: List<Int>,
    emptyString: String,
    main: String,
    emptyCharList: List<Char>,
    nonEmptyCharList: List<Char>,
    structWithEmptyLine: A,
    aSizedStruct: SizedStruct,
    finish: String
) {
    /* TODO: Wow, lots of empty lines! */
}

fun main() {
    val reader = BufferedReader(InputStreamReader(System.`in`))
    var emptyList: List<Int> = reader.readLine().split(" ").filter { !it.isBlank() }.map(String::toInt)
    var bufferString: String = reader.readLine()
    var n: Int = reader.readLine().toInt()
    var emptyInSample: List<Int> = reader.readLine().split(" ").filter { !it.isBlank() }.map(String::toInt)
    var emptyString: String = reader.readLine()
    var main: String = reader.readLine()
    var emptyCharList: List<Char> = reader.readLine().toList()
    var nonEmptyCharList: List<Char> = reader.readLine().toList()
    var listInStruct: List<Int> = reader.readLine().split(" ").filter { !it.isBlank() }.map(String::toInt)
    var structInStruct: StructWithAChar = reader.readLine().split(" ").let { 
        StructWithAChar(
            char1 = it[0][0],
            int2 = it[1].toInt()
        )
    }
    var structWithEmptyLine: A = A(
        listInStruct = listInStruct,
        structInStruct = structInStruct
    )
    var size: Int = reader.readLine().toInt()
    var stringInStruct: String = reader.readLine()
    var aSizedStruct: SizedStruct = SizedStruct(
        size = size,
        stringInStruct = stringInStruct
    )
    var finish: String = reader.readLine()

    emptyLines(emptyList, bufferString, n, emptyInSample, emptyString, main, emptyCharList, nonEmptyCharList, structWithEmptyLine, aSizedStruct, finish)
}

main()
