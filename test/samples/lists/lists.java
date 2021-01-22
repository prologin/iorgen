import java.util.Arrays;
import java.util.Scanner;

class Main {
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
    static void lists(int n, int[] listInt, int size, char[] listChar, String string, String[] listString4, String[][] listListString2, int[][] matrix) {
        /* TODO Aren't these lists beautifull? */
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n = Integer.parseInt(scanner.nextLine());
        int[] listInt = Arrays.stream(scanner.nextLine().split(" ")).mapToInt(Integer::parseInt).toArray();
        int size = Integer.parseInt(scanner.nextLine());
        char[] listChar = scanner.nextLine().toCharArray();
        String string = scanner.nextLine();
        String[] listString4 = new String[size];
        for (int i = 0; i < size; ++i) {
            listString4[i] = scanner.nextLine();
        }
        String[][] listListString2 = new String[2][];
        for (int i = 0; i < 2; ++i) {
            listListString2[i] = new String[2];
            for (int j = 0; j < 2; ++j) {
                listListString2[i][j] = scanner.nextLine();
            }
        }
        int[][] matrix = new int[size][];
        for (int i = 0; i < size; ++i) {
            matrix[i] = Arrays.stream(scanner.nextLine().split(" ")).filter(x -> !x.isEmpty()).mapToInt(Integer::parseInt).toArray();
        }

        lists(n, listInt, size, listChar, string, listString4, listListString2, matrix);
    }
}
