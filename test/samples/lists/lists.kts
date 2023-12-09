import java.io.BufferedReader
import java.io.InputStreamReader

/**
 * @param n the first list's size
 * @param listInt a list containing ints
 * @param size an other size
 * @param listChar a list of char
 * @param string a string
 * @param listString4 a list of strings of size 4
 * @param listListString2 a list of list of strings of size 2 of size 2 of size 2
 * @param matrix a matrix of int
 */
fun lists(
    n: Int,
    listInt: List<Int>,
    size: Int,
    listChar: List<Char>,
    string: String,
    listString4: List<String>,
    listListString2: List<List<String>>,
    matrix: List<List<Int>>
) {
    /* TODO: Aren't these lists beautifull? */
}

fun main() {
    val reader = BufferedReader(InputStreamReader(System.`in`))
    val n: Int = reader.readLine().toInt()
    val listInt: List<Int> = reader.readLine().split(" ").filter { !it.isBlank() }.map(String::toInt)
    val size: Int = reader.readLine().toInt()
    val listChar: List<Char> = reader.readLine().toList()
    val string: String = reader.readLine()
    val listString4: List<String> = List(size) { _ ->
        reader.readLine()
    }
    val listListString2: List<List<String>> = List(2) { _ ->
        List(2) { _ ->
            reader.readLine()
        }
    }
    val matrix: List<List<Int>> = List(size) { _ ->
        reader.readLine().split(" ").filter { !it.isBlank() }.map(String::toInt)
    }

    lists(n, listInt, size, listChar, string, listString4, listListString2, matrix)
}

main()
