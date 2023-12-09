import java.io.BufferedReader
import java.io.InputStreamReader

/**
 * may conflict in c#

 * @property a the first letter of the alphabet
 * @property static an integer
 */
data class Console(val a: Int, val static: Int)

/**
 * may conflict in c#

 * @property `return` not the end of the function
 * @property void not nothing
 */
data class System_(val `return`: Int, val void: List<Int>)

/**
 * not the main function

 * @property int not an integer
 * @property ifTrue should not cause conflict
 */
data class Main(val int: System_, val ifTrue: Int)

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
    words1: Int
) {
    /* TODO: If this compiles, it is already a good step! */
}

fun main() {
    val reader = BufferedReader(InputStreamReader(System.`in`))
    val `if`: Int = reader.readLine().toInt()
    val `class`: Char = reader.readLine()[0]
    val i: String = reader.readLine()
    val `in`: Console = reader.readLine().split(" ").let { 
        Console(
            a = it[0].toInt(),
            static = it[1].toInt()
        )
    }
    val `for`: List<Int> = reader.readLine().split(" ").filter { !it.isBlank() }.map(String::toInt)
    val words: List<Main> = List(2) { _ ->
        val `return`: Int = reader.readLine().toInt()
        val void: List<Int> = reader.readLine().split(" ").filter { !it.isBlank() }.map(String::toInt)
        val int: System_ = System_(
            `return` = `return`,
            void = void
        )
        val ifTrue: Int = reader.readLine().toInt()
        Main(
            int = int,
            ifTrue = ifTrue
        )
    }
    val words1: Int = reader.readLine().toInt()

    keywords(`if`, `class`, i, `in`, `for`, words, words1)
}

main()
