import java.io.BufferedReader
import java.io.InputStreamReader

/**
 * A simple struct

 * @property foo a field
 * @property bar a field
 */
data class Struct1(val foo: Int, val bar: Int)

/**
 * Represents a position

 * @property x X
 * @property y Y
 * @property z Z
 */
data class Position(val x: Int, val y: Int, val z: Int)

/**
 * A point's name and position

 * @property name the point's name (single character)
 * @property description the point's description
 * @property pos the point's position
 */
data class Point(val name: Char, val description: String, val pos: Position)

/**
 * a struct of chars

 * @property firstChar a first char
 * @property secondChar a second char
 * @property thirdChar a third char
 */
data class Chars(val firstChar: Char, val secondChar: Char, val thirdChar: Char)

/**
 * contains a big list inside

 * @property int int
 * @property bigList list nested 3 times!
 */
data class WithList(val int: Int, val bigList: List<List<List<Int>>>)

/**
 * @param struct a struct 1 instance
 * @param n a number
 * @param structList a list a struct 1
 * @param triangle a triangle
 * @param structChars a struct of chars
 * @param bigListStruct the big list struct
 */
fun structs(
    struct: Struct1,
    n: Int,
    structList: List<Struct1>,
    triangle: List<Point>,
    structChars: Chars,
    bigListStruct: WithList
) {
    /* TODO: Look at them structs. */
}

fun main() {
    val reader = BufferedReader(InputStreamReader(System.`in`))
    val struct: Struct1 = reader.readLine().split(" ").let { 
        Struct1(
            foo = it[0].toInt(),
            bar = it[1].toInt()
        )
    }
    val n: Int = reader.readLine().toInt()
    val structList: List<Struct1> = List(n) { _ ->
        reader.readLine().split(" ").let { 
            Struct1(
                foo = it[0].toInt(),
                bar = it[1].toInt()
            )
        }
    }
    val triangle: List<Point> = List(3) { _ ->
        val name: Char = reader.readLine()[0]
        val description: String = reader.readLine()
        val pos: Position = reader.readLine().split(" ").let { 
            Position(
                x = it[0].toInt(),
                y = it[1].toInt(),
                z = it[2].toInt()
            )
        }
        Point(
            name = name,
            description = description,
            pos = pos
        )
    }
    val structChars: Chars = reader.readLine().split(" ").let { 
        Chars(
            firstChar = it[0][0],
            secondChar = it[1][0],
            thirdChar = it[2][0]
        )
    }
    val int: Int = reader.readLine().toInt()
    val bigList: List<List<List<Int>>> = List(2) { _ ->
        List(2) { _ ->
            reader.readLine().split(" ").filter { !it.isBlank() }.map(String::toInt)
        }
    }
    val bigListStruct: WithList = WithList(
        int = int,
        bigList = bigList
    )

    structs(struct, n, structList, triangle, structChars, bigListStruct)
}

main()
