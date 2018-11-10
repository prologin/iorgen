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
     * the point's position
     */
    public Position pos;
}

class Main {
    /**
     * @param struct a struct 1 instance
     * @param n a number
     * @param structList a list a struct 1
     * @param triangle a triangle
     */
    static void structs(Struct1 struct, int n, Struct1[] structList, Point[] triangle) {
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
            String[] words2 = scanner.nextLine().split(" ");
            structList[i] = new Struct1();
            structList[i].foo = Integer.parseInt(words2[0]);
            structList[i].bar = Integer.parseInt(words2[1]);
        }
        Point[] triangle = new Point[3];
        for (int i = 0; i < 3; ++i) {
            triangle[i] = new Point();
            triangle[i].name = scanner.nextLine().charAt(0);
            String[] words3 = scanner.nextLine().split(" ");
            triangle[i].pos = new Position();
            triangle[i].pos.x = Integer.parseInt(words3[0]);
            triangle[i].pos.y = Integer.parseInt(words3[1]);
            triangle[i].pos.z = Integer.parseInt(words3[2]);
        }

        structs(struct, n, structList, triangle);
    }
}
