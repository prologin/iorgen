import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;

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

    public static void main(String[] args) throws java.io.IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(reader.readLine());
        int[] listInt = Arrays.stream(reader.readLine().split(" ")).mapToInt(Integer::parseInt).toArray();
        int size = Integer.parseInt(reader.readLine());
        char[] listChar = reader.readLine().toCharArray();
        String string = reader.readLine();
        String[] listString4 = new String[size];
        for (int i = 0; i < size; ++i) {
            listString4[i] = reader.readLine();
        }
        String[][] listListString2 = new String[2][];
        for (int i = 0; i < 2; ++i) {
            listListString2[i] = new String[2];
            for (int j = 0; j < 2; ++j) {
                listListString2[i][j] = reader.readLine();
            }
        }
        int[][] matrix = new int[size][];
        for (int i = 0; i < size; ++i) {
            matrix[i] = Arrays.stream(reader.readLine().split(" ")).filter(x -> !x.isEmpty()).mapToInt(Integer::parseInt).toArray();
        }

        lists(n, listInt, size, listChar, string, listString4, listListString2, matrix);
    }
}
