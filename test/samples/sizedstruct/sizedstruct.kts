import java.io.BufferedReader
import java.io.InputStreamReader

/**
 * contains a list

 * @property size1 the list's size
 * @property intList the integer list
 */
data class List_(val size1: Int, val intList: List<Int>)

/**
 * contains a string

 * @property size2 the list's size
 * @property stringList the string list
 */
data class String_(val size2: Int, val stringList: String)

/**
 * contains a matrix

 * @property size3 the list's size
 * @property listList the list list
 */
data class Matrix(val size3: Int, val listList: List<List<Int>>)

/**
 * this is not a 'sized struct', but a regular one!

 * @property size4 not the list's size
 * @property intListN the integer list
 */
data class NotASizedStruct(val size4: Int, val intListN: List<Int>)

/**
 * @param n the size of the lists
 * @param lists a list of list of different sizes
 * @param strings a list of strings of different sizes
 * @param matrices a list of matrices of different sizes
 * @param same a list of list of same sizes
 */
fun sizedStruct(
    n: Int,
    lists: List<List_>,
    strings: List<String_>,
    matrices: List<Matrix>,
    same: List<NotASizedStruct>
) {
    /* TODO: The is a special case. */
}

fun main() {
    val reader = BufferedReader(InputStreamReader(System.`in`))
    val n: Int = reader.readLine().toInt()
    val lists: List<List_> = List(n) { _ ->
        val size1: Int = reader.readLine().toInt()
        val intList: List<Int> = reader.readLine().split(" ").filter { !it.isBlank() }.map(String::toInt)
        List_(
            size1 = size1,
            intList = intList
        )
    }
    val strings: List<String_> = List(n) { _ ->
        val size2: Int = reader.readLine().toInt()
        val stringList: String = reader.readLine()
        String_(
            size2 = size2,
            stringList = stringList
        )
    }
    val matrices: List<Matrix> = List(2) { _ ->
        val size3: Int = reader.readLine().toInt()
        val listList: List<List<Int>> = List(size3) { _ ->
            reader.readLine().split(" ").filter { !it.isBlank() }.map(String::toInt)
        }
        Matrix(
            size3 = size3,
            listList = listList
        )
    }
    val same: List<NotASizedStruct> = List(n) { _ ->
        val size4: Int = reader.readLine().toInt()
        val intListN: List<Int> = reader.readLine().split(" ").filter { !it.isBlank() }.map(String::toInt)
        NotASizedStruct(
            size4 = size4,
            intListN = intListN
        )
    }

    sizedStruct(n, lists, strings, matrices, same)
}

main()
