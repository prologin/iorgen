import java.io.BufferedReader
import java.io.InputStreamReader

/**
 * @param a a first number
 * @param b a second number
 * @param c a third number
 * @param n This one on a new line
 * @param onePerLine an integer list, one per line
 */
fun manualFormat(
    a: Int,
    b: Int,
    c: Int,
    n: Int,
    onePerLine: List<Int>
) {
    /* TODO: From the function perspective, this is just 4 integers */
}

fun main() {
    val reader = BufferedReader(InputStreamReader(System.`in`))
    val words = reader.readLine().split(" ")
    val a = words[0].toInt()
    val b = words[1].toInt()
    val c = words[2].toInt()
    val n: Int = reader.readLine().toInt()
    val onePerLine: List<Int> = List(3) { _ ->
        reader.readLine().toInt()
    }

    manualFormat(a, b, c, n, onePerLine)
}

main()
