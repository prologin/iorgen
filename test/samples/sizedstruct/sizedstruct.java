import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;

/**
 * contains a list
 */
class List
{
    /**
     * the list's size
     */
    public int size1;
    /**
     * the integer list
     */
    public int[] intList;
}

/**
 * contains a string
 */
class String_
{
    /**
     * the list's size
     */
    public int size2;
    /**
     * the string list
     */
    public String stringList;
}

/**
 * contains a matrix
 */
class Matrix
{
    /**
     * the list's size
     */
    public int size3;
    /**
     * the list list
     */
    public int[][] listList;
}

/**
 * this is not a 'sized struct', but a regular one!
 */
class NotASizedStruct
{
    /**
     * not the list's size
     */
    public int size4;
    /**
     * the integer list
     */
    public int[] intListN;
}

class Main {
    /**
     * @param n the size of the lists
     * @param lists a list of list of different sizes
     * @param strings a list of strings of different sizes
     * @param matrices a list of matrices of different sizes
     * @param same a list of list of same sizes
     */
    static void sizedStruct(int n, List[] lists, String_[] strings, Matrix[] matrices, NotASizedStruct[] same) {
        /* TODO The is a special case. */
    }

    public static void main(String[] args) throws java.io.IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(reader.readLine());
        List[] lists = new List[n];
        for (int i = 0; i < n; ++i) {
            lists[i] = new List();
            lists[i].size1 = Integer.parseInt(reader.readLine());
            lists[i].intList = Arrays.stream(reader.readLine().split(" ")).filter(x -> !x.isEmpty()).mapToInt(Integer::parseInt).toArray();
        }
        String_[] strings = new String_[n];
        for (int i = 0; i < n; ++i) {
            strings[i] = new String_();
            strings[i].size2 = Integer.parseInt(reader.readLine());
            strings[i].stringList = reader.readLine();
        }
        Matrix[] matrices = new Matrix[2];
        for (int i = 0; i < 2; ++i) {
            matrices[i] = new Matrix();
            matrices[i].size3 = Integer.parseInt(reader.readLine());
            matrices[i].listList = new int[matrices[i].size3][];
            for (int j = 0; j < matrices[i].size3; ++j) {
                matrices[i].listList[j] = Arrays.stream(reader.readLine().split(" ")).mapToInt(Integer::parseInt).toArray();
            }
        }
        NotASizedStruct[] same = new NotASizedStruct[n];
        for (int i = 0; i < n; ++i) {
            same[i] = new NotASizedStruct();
            same[i].size4 = Integer.parseInt(reader.readLine());
            same[i].intListN = Arrays.stream(reader.readLine().split(" ")).filter(x -> !x.isEmpty()).mapToInt(Integer::parseInt).toArray();
        }

        sizedStruct(n, lists, strings, matrices, same);
    }
}
