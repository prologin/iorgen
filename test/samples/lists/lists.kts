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
    matrix: List<List<Int>>,
) {
    /* TODO: Aren't these lists beautifull? */
}

fun main() {
    val reader = BufferedReader(InputStreamReader(System.`in`))
    var n: Int = reader.readLine().toInt()
    var listInt: List<Int> = reader.readLine().split(" ").map(String::toInt)
    var size: Int = reader.readLine().toInt()
    var listChar: List<Char> = reader.readLine().toList()
    var string: String = reader.readLine()
    var listString4: List<String> = List(size) { _ ->
        var words: String = reader.readLine()
        words
    }
    var listListString2: List<List<String>> = List(2) { _ ->
        var words1: List<String> = List(2) { _ ->
            var words2: String = reader.readLine()
            words2
        }
        words1
    }
    var matrix: List<List<Int>> = List(size) { _ ->
        var words3: List<Int> = reader.readLine().split(" ").map(String::toInt)
        words3
    }

    lists(n, listInt, size, listChar, string, listString4, listListString2, matrix)
}
