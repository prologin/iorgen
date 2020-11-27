import java.util.Scanner;

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

class Main {
    /**
     * @param struct a struct 1 instance
     * @param n a number
     * @param structList a list a struct 1
     * @param triangle a triangle
     * @param structChars a struct of chars
     */
    static void structs(Struct1 struct, int n, Struct1[] structList, Point[] triangle, Chars structChars) {
        /* TODO Look at them structs. */
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        String[] words = scanner.nextLine().split(" ");
        Struct1 struct = new Struct1();
        struct.foo = Integer.parseInt(words[0]);
        struct.bar = Integer.parseInt(words[1]);
        int n = Integer.parseInt(scanner.nextLine());
        Struct1[] structList = new Struct1[n];
        for (int i = 0; i < n; ++i) {
            String[] words1 = scanner.nextLine().split(" ");
            structList[i] = new Struct1();
            structList[i].foo = Integer.parseInt(words1[0]);
            structList[i].bar = Integer.parseInt(words1[1]);
        }
        Point[] triangle = new Point[3];
        for (int i = 0; i < 3; ++i) {
            triangle[i] = new Point();
            triangle[i].name = scanner.nextLine().charAt(0);
            triangle[i].description = scanner.nextLine();
            String[] words1 = scanner.nextLine().split(" ");
            triangle[i].pos = new Position();
            triangle[i].pos.x = Integer.parseInt(words1[0]);
            triangle[i].pos.y = Integer.parseInt(words1[1]);
            triangle[i].pos.z = Integer.parseInt(words1[2]);
        }
        String[] words1 = scanner.nextLine().split(" ");
        Chars structChars = new Chars();
        structChars.firstChar = words1[0].charAt(0);
        structChars.secondChar = words1[1].charAt(0);
        structChars.thirdChar = words1[2].charAt(0);

        structs(struct, n, structList, triangle, structChars);
    }
}
