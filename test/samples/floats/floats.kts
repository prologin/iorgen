import java.io.BufferedReader
import java.io.InputStreamReader

/**
 * Represents coordinates

 * @property x X
 * @property y Y
 * @property z Z
 */
data class Coordinates(val x: Double, val y: Double, val z: Double)

/**
 * Mix of fields that go on one line

 * @property integer an integer
 * @property char a char
 * @property float a float
 */
data class InlinedMix(val integer: Int, val char: Char, val float: Double)

/**
 * a struct of chars

 * @property integer2 an other integer
 * @property string a string of size 5
 * @property float2 an other float
 */
data class MultilineMix(val integer2: Int, val string: String, val float2: Double)

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
    f: Double,
    g: Double,
    point: Coordinates,
    n: Int,
    floatList: List<Double>,
    otherList: List<Double>,
    inlined: List<InlinedMix>,
    multiline: MultilineMix
) {
    /* TODO: Parsing is often easy, reprint mode is harder */
}

fun main() {
    val reader = BufferedReader(InputStreamReader(System.`in`))
    var f: Double = reader.readLine().toDouble()
    var g: Double = reader.readLine().toDouble()
    var point: Coordinates = reader.readLine().split(" ").let { 
        Coordinates(
            x = it[0].toDouble(),
            y = it[1].toDouble(),
            z = it[2].toDouble()
        )
    }
    var n: Int = reader.readLine().toInt()
    var floatList: List<Double> = reader.readLine().split(" ").filter { !it.isBlank() }.map(String::toDouble)
    var otherList: List<Double> = reader.readLine().split(" ").filter { !it.isBlank() }.map(String::toDouble)
    var inlined: List<InlinedMix> = List(3) { _ ->
        var words1: InlinedMix = reader.readLine().split(" ").let { 
            InlinedMix(
                integer = it[0].toInt(),
                char = it[1][0],
                float = it[2].toDouble()
            )
        }
        words1
    }
    var integer2: Int = reader.readLine().toInt()
    var string: String = reader.readLine()
    var float2: Double = reader.readLine().toDouble()
    var multiline: MultilineMix = MultilineMix(
        integer2 = integer2,
        string = string,
        float2 = float2
    )

    floats(f, g, point, n, floatList, otherList, inlined, multiline)
}

main()
