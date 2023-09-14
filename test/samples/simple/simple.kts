import java.io.BufferedReader
import java.io.InputStreamReader

/**
 * @param n the first number
 * @param otherNumber the second number
 */
fun simple(
    n: Int,
    otherNumber: Int,
) {
    /* TODO: Just do what you want with these numbers, like sum them. */
}

fun main() {
    val reader = BufferedReader(InputStreamReader(System.`in`))
    var n: Int = reader.readLine().toInt()
    var otherNumber: Int = reader.readLine().toInt()

    simple(n, otherNumber)
}

main()