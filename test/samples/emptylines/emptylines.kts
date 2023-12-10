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
 * @param main_ an other buffer string
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
    main_: String,
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
    val emptyList: List<Int> = reader.readLine().split(" ").filter { !it.isBlank() }.map(String::toInt)
    val bufferString: String = reader.readLine()
    val n: Int = reader.readLine().toInt()
    val emptyInSample: List<Int> = reader.readLine().split(" ").filter { !it.isBlank() }.map(String::toInt)
    val emptyString: String = reader.readLine()
    val main_: String = reader.readLine()
    val emptyCharList: List<Char> = reader.readLine().toList()
    val nonEmptyCharList: List<Char> = reader.readLine().toList()
    val listInStruct: List<Int> = reader.readLine().split(" ").filter { !it.isBlank() }.map(String::toInt)
    val structInStruct: StructWithAChar = reader.readLine().split(" ").let {
        StructWithAChar(
            char1 = it[0][0],
            int2 = it[1].toInt()
        )
    }
    val structWithEmptyLine: A = A(
        listInStruct = listInStruct,
        structInStruct = structInStruct
    )
    val size: Int = reader.readLine().toInt()
    val stringInStruct: String = reader.readLine()
    val aSizedStruct: SizedStruct = SizedStruct(
        size = size,
        stringInStruct = stringInStruct
    )
    val finish: String = reader.readLine()

    emptyLines(emptyList, bufferString, n, emptyInSample, emptyString, main_, emptyCharList, nonEmptyCharList, structWithEmptyLine, aSizedStruct, finish)
}

main()
