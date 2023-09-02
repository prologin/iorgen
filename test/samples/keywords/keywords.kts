import java.io.BufferedReader
import java.io.InputStreamReader

/**
 * may conflict in c#

 * @property a the first letter of the alphabet
 * @property static an integer
 */
data class Console(
    var a: Int,
    var static: Int,
)

/**
 * may conflict in c#

 * @property `return` not the end of the function
 * @property void not nothing
 */
data class System_(
    var `return`: Int,
    var void: List<Int>,
)

/**
 * not the main function

 * @property int not an integer
 * @property ifTrue should not cause conflict
 */
data class Main(
    var int: System_,
    var ifTrue: Int,
)

/**
 * @param `if` not a condition
 * @param `class` not a class
 * @param i just a string
 * @param `in` not in
 * @param `for` not a loop
 * @param words contains lots of things
 * @param words1 an integer
 */
fun keywords(
    `if`: Int,
    `class`: Char,
    i: String,
    `in`: Console,
    `for`: List<Int>,
    words: List<Main>,
    words1: Int,
) {
    /* TODO: If this compiles, it is already a good step! */
}

fun main() {
    val reader = BufferedReader(InputStreamReader(System.`in`))
    var `if`: Int = reader.readLine().toInt()
    var `class`: Char = reader.readLine()[0]
    var i: String = reader.readLine()
    var `in`: Console = reader.readLine().split(" ").let {
        Console(
            a = it[0].toInt(),
            static = it[1].toInt(),
        )
    }
    var `for`: List<Int> = reader.readLine().split(" ").map(String::toInt)
    var words: List<Main> = List(2) { _ ->
        var `return`: Int = reader.readLine().toInt()
        var void: List<Int> = reader.readLine().split(" ").map(String::toInt)
        var int: System_ = System_(
            `return` = `return`,
            void = void,
        )
        var ifTrue: Int = reader.readLine().toInt()
        var words3: Main = Main(
            int = int,
            ifTrue = ifTrue,
        )
        words3
    }
    var words1: Int = reader.readLine().toInt()

    keywords(`if`, `class`, i, `in`, `for`, words, words1)
}

main()