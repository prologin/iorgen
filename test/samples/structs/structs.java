import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;

/**
 * A simple struct
 */
class Struct1
{
    /**
     * a field
     */
    public int foo;
    /**
     * a field
     */
    public int bar;
}

/**
 * Represents a position
 */
class Position
{
    /**
     * X
     */
    public int x;
    /**
     * Y
     */
    public int y;
    /**
     * Z
     */
    public int z;
}

/**
 * A point's name and position
 */
class Point
{
    /**
     * the point's name (single character)
     */
    public char name;
    /**
     * the point's description
     */
    public String description;
    /**
     * the point's position
     */
    public Position pos;
}

/**
 * a struct of chars
 */
class Chars
{
    /**
     * a first char
     */
    public char firstChar;
    /**
     * a second char
     */
    public char secondChar;
    /**
     * a third char
     */
    public char thirdChar;
}

/**
 * contains a big list inside
 */
class WithList
{
    /**
     * int
     */
    public int int_;
    /**
     * list nested 3 times!
     */
    public int[][][] bigList;
}

class Main {
    /**
     * @param struct a struct 1 instance
     * @param n a number
     * @param structList a list a struct 1
     * @param triangle a triangle
     * @param structChars a struct of chars
     * @param bigListStruct the big list struct
     */
    static void structs(Struct1 struct, int n, Struct1[] structList, Point[] triangle, Chars structChars, WithList bigListStruct) {
        /* TODO Look at them structs. */
    }

    public static void main(String[] args) throws java.io.IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String[] words = reader.readLine().split(" ");
        Struct1 struct = new Struct1();
        struct.foo = Integer.parseInt(words[0]);
        struct.bar = Integer.parseInt(words[1]);
        int n = Integer.parseInt(reader.readLine());
        Struct1[] structList = new Struct1[n];
        for (int i = 0; i < n; ++i) {
            String[] words1 = reader.readLine().split(" ");
            structList[i] = new Struct1();
            structList[i].foo = Integer.parseInt(words1[0]);
            structList[i].bar = Integer.parseInt(words1[1]);
        }
        Point[] triangle = new Point[3];
        for (int i = 0; i < 3; ++i) {
            triangle[i] = new Point();
            triangle[i].name = reader.readLine().charAt(0);
            triangle[i].description = reader.readLine();
            String[] words1 = reader.readLine().split(" ");
            triangle[i].pos = new Position();
            triangle[i].pos.x = Integer.parseInt(words1[0]);
            triangle[i].pos.y = Integer.parseInt(words1[1]);
            triangle[i].pos.z = Integer.parseInt(words1[2]);
        }
        String[] words1 = reader.readLine().split(" ");
        Chars structChars = new Chars();
        structChars.firstChar = words1[0].charAt(0);
        structChars.secondChar = words1[1].charAt(0);
        structChars.thirdChar = words1[2].charAt(0);
        WithList bigListStruct = new WithList();
        bigListStruct.int_ = Integer.parseInt(reader.readLine());
        bigListStruct.bigList = new int[2][][];
        for (int i = 0; i < 2; ++i) {
            bigListStruct.bigList[i] = new int[2][];
            for (int j = 0; j < 2; ++j) {
                bigListStruct.bigList[i][j] = Arrays.stream(reader.readLine().split(" ")).mapToInt(Integer::parseInt).toArray();
            }
        }

        structs(struct, n, structList, triangle, structChars, bigListStruct);
    }
}
