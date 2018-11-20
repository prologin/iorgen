import java.util.Scanner;

/**
 * A struct for the example
 */
class AStruct
{
    /**
     * an integer
     */
    public int integer;
    /**
     * a char
     */
    public char character;
}

class Main {
    /**
     * @param n a number, used as a size
     * @param list a list of structs
     */
    static void example(int n, AStruct[] list) {
        /* TODO In a real life scenario, you will describe here what you want
        the end user to do with this generated code */
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n = Integer.parseInt(scanner.nextLine());
        AStruct[] list = new AStruct[n];
        for (int i = 0; i < n; ++i) {
            String[] words = scanner.nextLine().split(" ");
            list[i] = new AStruct();
            list[i].integer = Integer.parseInt(words[0]);
            list[i].character = words[1].charAt(0);
        }

        example(n, list);
    }
}
