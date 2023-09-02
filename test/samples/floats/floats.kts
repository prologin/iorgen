import java.io.BufferedReader
import java.io.InputStreamReader

/**
 * Represents coordinates

 * @property x X
 * @property y Y
 * @property z Z
 */
data class Coordinates(
    var x: Float,
    var y: Float,
    var z: Float,
)

/**
 * Mix of fields that go on one line

 * @property integer an integer
 * @property char a char
 * @property float a float
 */
data class InlinedMix(
    var integer: Int,
    var char: Char,
    var float: Float,
)

/**
 * a struct of chars

 * @property integer2 an other integer
 * @property string a string of size 5
 * @property float2 an other float
 */
data class MultilineMix(
    var integer2: Int,
    var string: String,
    var float2: Float,
)

/**
 * @param f a float
 * @param g a float, greater than f
 * @param point some coordinates
 * @param n a number
 * @param floatList a list of floats
 * @param otherList a list of floats
 * @param inlined some inlined structs
 * @param multiline a multiline struct
 */
fun floats(
    f: Float,
    g: Float,
    point: Coordinates,
    n: Int,
    floatList: List<Float>,
    otherList: List<Float>,
    inlined: List<InlinedMix>,
    multiline: MultilineMix,
) {
    /* TODO: Parsing is often easy, reprint mode is harder */
}

fun main() {
    val reader = BufferedReader(InputStreamReader(System.`in`))
    var f: Float = reader.readLine().toFloat()
    var g: Float = reader.readLine().toFloat()
    var point: Coordinates = reader.readLine().split(" ").let {
        Coordinates(
            x = it[0].toFloat(),
            y = it[1].toFloat(),
            z = it[2].toFloat(),
        )
    }
    var n: Int = reader.readLine().toInt()
    var floatList: List<Float> = reader.readLine().split(" ").map(String::toFloat)
    var otherList: List<Float> = reader.readLine().split(" ").map(String::toFloat)
    var inlined: List<InlinedMix> = List(3) { _ ->
        var words1: InlinedMix = reader.readLine().split(" ").let {
            InlinedMix(
                integer = it[0].toInt(),
                char = it[1][0],
                float = it[2].toFloat(),
            )
        }
        words1
    }
    var integer2: Int = reader.readLine().toInt()
    var string: String = reader.readLine()
    var float2: Float = reader.readLine().toFloat()
    var multiline: MultilineMix = MultilineMix(
        integer2 = integer2,
        string = string,
        float2 = float2,
    )

    floats(f, g, point, n, floatList, otherList, inlined, multiline)
}
