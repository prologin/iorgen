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
    onePerLine: List<Int>,
) {
    /* TODO: From the function perspective, this is just 4 integers */
}

fun main() {
    val reader = BufferedReader(InputStreamReader(System.`in`))
    var words = reader.readLine().split(" ")
    var a = words[0].toInt()
    var b = words[1].toInt()
    var c = words[2].toInt()
    var n: Int = reader.readLine().toInt()
    var onePerLine: List<Int> = List(3) { _ ->
        var words1: Int = reader.readLine().toInt()
        words1
    }

    manualFormat(a, b, c, n, onePerLine)
}
