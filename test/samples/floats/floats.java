import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;

/**
 * Represents coordinates
 */
class Coordinates
{
    /**
     * X
     */
    public double x;
    /**
     * Y
     */
    public double y;
    /**
     * Z
     */
    public double z;
}

/**
 * Mix of fields that go on one line
 */
class InlinedMix
{
    /**
     * an integer
     */
    public int integer;
    /**
     * a char
     */
    public char char_;
    /**
     * a float
     */
    public double float_;
}

/**
 * a struct of chars
 */
class MultilineMix
{
    /**
     * an other integer
     */
    public int integer2;
    /**
     * a string of size 5
     */
    public String string;
    /**
     * an other float
     */
    public double float2;
}

class Main {
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
    static void floats(double f, double g, Coordinates point, int n, double[] floatList, double[] otherList, InlinedMix[] inlined, MultilineMix multiline) {
        /* TODO Parsing is often easy, reprint mode is harder */
    }

    public static void main(String[] args) throws java.io.IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        double f = Double.parseDouble(reader.readLine());
        double g = Double.parseDouble(reader.readLine());
        String[] words = reader.readLine().split(" ");
        Coordinates point = new Coordinates();
        point.x = Double.parseDouble(words[0]);
        point.y = Double.parseDouble(words[1]);
        point.z = Double.parseDouble(words[2]);
        int n = Integer.parseInt(reader.readLine());
        double[] floatList = Arrays.stream(reader.readLine().split(" ")).filter(x -> !x.isEmpty()).mapToDouble(Double::parseDouble).toArray();
        double[] otherList = Arrays.stream(reader.readLine().split(" ")).mapToDouble(Double::parseDouble).toArray();
        InlinedMix[] inlined = new InlinedMix[3];
        for (int i = 0; i < 3; ++i) {
            String[] words1 = reader.readLine().split(" ");
            inlined[i] = new InlinedMix();
            inlined[i].integer = Integer.parseInt(words1[0]);
            inlined[i].char_ = words1[1].charAt(0);
            inlined[i].float_ = Double.parseDouble(words1[2]);
        }
        MultilineMix multiline = new MultilineMix();
        multiline.integer2 = Integer.parseInt(reader.readLine());
        multiline.string = reader.readLine();
        multiline.float2 = Double.parseDouble(reader.readLine());

        floats(f, g, point, n, floatList, otherList, inlined, multiline);
    }
}
