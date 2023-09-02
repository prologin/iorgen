import java.io.BufferedReader
import java.io.InputStreamReader

/**
 * A simple struct

 * @property foo a field
 * @property bar a field
 */
data class Struct1(
    var foo: Int,
    var bar: Int,
)

/**
 * Represents a position

 * @property x X
 * @property y Y
 * @property z Z
 */
data class Position(
    var x: Int,
    var y: Int,
    var z: Int,
)

/**
 * A point's name and position

 * @property name the point's name (single character)
 * @property description the point's description
 * @property pos the point's position
 */
data class Point(
    var name: Char,
    var description: String,
    var pos: Position,
)

/**
 * a struct of chars

 * @property firstChar a first char
 * @property secondChar a second char
 * @property thirdChar a third char
 */
data class Chars(
    var firstChar: Char,
    var secondChar: Char,
    var thirdChar: Char,
)

/**
 * contains a big list inside

 * @property int int
 * @property bigList list nested 3 times!
 */
data class WithList(
    var int: Int,
    var bigList: List<List<List<Int>>>,
)

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
    bigListStruct: WithList,
) {
    /* TODO: Look at them structs. */
}

fun main() {
    val reader = BufferedReader(InputStreamReader(System.`in`))
    var struct: Struct1 = reader.readLine().split(" ").let {
        Struct1(
            foo = it[0].toInt(),
            bar = it[1].toInt(),
        )
    }
    var n: Int = reader.readLine().toInt()
    var structList: List<Struct1> = List(n) { _ ->
        var words1: Struct1 = reader.readLine().split(" ").let {
            Struct1(
                foo = it[0].toInt(),
                bar = it[1].toInt(),
            )
        }
        words1
    }
    var triangle: List<Point> = List(3) { _ ->
        var name: Char = reader.readLine()[0]
        var description: String = reader.readLine()
        var pos: Position = reader.readLine().split(" ").let {
            Position(
                x = it[0].toInt(),
                y = it[1].toInt(),
                z = it[2].toInt(),
            )
        }
        var words3: Point = Point(
            name = name,
            description = description,
            pos = pos,
        )
        words3
    }
    var structChars: Chars = reader.readLine().split(" ").let {
        Chars(
            firstChar = it[0][0],
            secondChar = it[1][0],
            thirdChar = it[2][0],
        )
    }
    var int: Int = reader.readLine().toInt()
    var bigList: List<List<List<Int>>> = List(2) { _ ->
        var words6: List<List<Int>> = List(2) { _ ->
            var words7: List<Int> = reader.readLine().split(" ").map(String::toInt)
            words7
        }
        words6
    }
    var bigListStruct: WithList = WithList(
        int = int,
        bigList = bigList,
    )

    structs(struct, n, structList, triangle, structChars, bigListStruct)
}

main()