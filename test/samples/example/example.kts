import java.io.BufferedReader
import java.io.InputStreamReader

/**
 * A struct for the example

 * @property integer an integer
 * @property character a char
 */
data class AStruct(val integer: Int, val character: Char)

/**
 * @param n a number, used as a size
 * @param list a list of structs
 */
fun example(
    n: Int,
    list: List<AStruct>
) {
    /* TODO: In a real life scenario, you will describe here what you want
    the end user to do with this generated code */
}

fun main() {
    val reader = BufferedReader(InputStreamReader(System.`in`))
    val n: Int = reader.readLine().toInt()
    val list: List<AStruct> = List(n) { _ ->
        reader.readLine().split(" ").let {
            AStruct(
                integer = it[0].toInt(),
                character = it[1][0]
            )
        }
    }

    example(n, list)
}

main()
