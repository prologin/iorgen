import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;

/**
 * may conflict in c#
 */
class Console
{
    /**
     * the first letter of the alphabet
     */
    public int a;
    /**
     * an integer
     */
    public int static_;
}

/**
 * may conflict in c#
 */
class System_
{
    /**
     * not the end of the function
     */
    public int return_;
    /**
     * not nothing
     */
    public int[] void_;
}

/**
 * not the main function
 */
class Main_
{
    /**
     * not an integer
     */
    public System_ int_;
    /**
     * should not cause conflict
     */
    public int ifTrue;
}

class Main {
    /**
     * @param if_ not a condition
     * @param class_ not a class
     * @param i just a string
     * @param in not in
     * @param for_ not a loop
     * @param words contains lots of things
     * @param words1 an integer
     */
    static void keywords(int if_, char class_, String i, Console in, int[] for_, Main_[] words, int words1) {
        /* TODO If this compiles, it is already a good step! */
    }

    public static void main(String[] args) throws java.io.IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int if_ = Integer.parseInt(reader.readLine());
        char class_ = reader.readLine().charAt(0);
        String i = reader.readLine();
        String[] words2 = reader.readLine().split(" ");
        Console in = new Console();
        in.a = Integer.parseInt(words2[0]);
        in.static_ = Integer.parseInt(words2[1]);
        int[] for_ = Arrays.stream(reader.readLine().split(" ")).filter(x -> !x.isEmpty()).mapToInt(Integer::parseInt).toArray();
        Main_[] words = new Main_[2];
        for (int j = 0; j < 2; ++j) {
            words[j] = new Main_();
            words[j].int_ = new System_();
            words[j].int_.return_ = Integer.parseInt(reader.readLine());
            words[j].int_.void_ = Arrays.stream(reader.readLine().split(" ")).mapToInt(Integer::parseInt).toArray();
            words[j].ifTrue = Integer.parseInt(reader.readLine());
        }
        int words1 = Integer.parseInt(reader.readLine());

        keywords(if_, class_, i, in, for_, words, words1);
    }
}
