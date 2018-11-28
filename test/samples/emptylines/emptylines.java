import java.util.Arrays;
import java.util.Scanner;

/**
 * a char struct
 */
class StructWithAChar
{
    /**
     * a char
     */
    public char char1;
    /**
     * an integer
     */
    public int int2;
}

/**
 * a struct
 */
class A
{
    /**
     * a list in a struct
     */
    public int[] listInStruct;
    /**
     * a struct in a struct
     */
    public StructWithAChar structInStruct;
}

/**
 * a sized struct
 */
class SizedStruct
{
    /**
     * the size
     */
    public int size;
    /**
     * the string
     */
    public String stringInStruct;
}

class Main {
    /**
     * @param emptyList an empty list
     * @param bufferString here to check correct parsing of empty line above
     * @param n an integer, will be 0 in the sample input
     * @param emptyInSample an empty list (only in the sample)
     * @param emptyString an empty string
     * @param main an other buffer string
     * @param emptyCharList an empty char list
     * @param nonEmptyCharList an char list, non empty
     * @param structWithEmptyLine a struct containing an empty line, then a struct
     * @param aSizedStruct a sized struct containing an empty line
     * @param finish a string to finish
     */
    static void emptyLines(int[] emptyList, String bufferString, int n, int[] emptyInSample, String emptyString, String main, char[] emptyCharList, char[] nonEmptyCharList, A structWithEmptyLine, SizedStruct aSizedStruct, String finish) {
        /* TODO Wow, lots of empty lines! */
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int[] emptyList = Arrays.stream(scanner.nextLine().split(" ")).filter(x -> !x.isEmpty()).mapToInt(Integer::parseInt).toArray();
        String bufferString = scanner.nextLine();
        int n = Integer.parseInt(scanner.nextLine());
        int[] emptyInSample = Arrays.stream(scanner.nextLine().split(" ")).filter(x -> !x.isEmpty()).mapToInt(Integer::parseInt).toArray();
        String emptyString = scanner.nextLine();
        String main = scanner.nextLine();
        char[] emptyCharList = scanner.nextLine().toCharArray();
        char[] nonEmptyCharList = scanner.nextLine().toCharArray();
        A structWithEmptyLine = new A();
        structWithEmptyLine.listInStruct = Arrays.stream(scanner.nextLine().split(" ")).filter(x -> !x.isEmpty()).mapToInt(Integer::parseInt).toArray();
        String[] words = scanner.nextLine().split(" ");
        structWithEmptyLine.structInStruct = new StructWithAChar();
        structWithEmptyLine.structInStruct.char1 = words[0].charAt(0);
        structWithEmptyLine.structInStruct.int2 = Integer.parseInt(words[1]);
        SizedStruct aSizedStruct = new SizedStruct();
        aSizedStruct.size = Integer.parseInt(scanner.nextLine());
        aSizedStruct.stringInStruct = scanner.nextLine();
        String finish = scanner.nextLine();

        emptyLines(emptyList, bufferString, n, emptyInSample, emptyString, main, emptyCharList, nonEmptyCharList, structWithEmptyLine, aSizedStruct, finish);
    }
}
