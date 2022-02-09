import java.io.BufferedReader;
import java.io.InputStreamReader;

class Main {
    /**
     * @param a a first number
     * @param b a second number
     * @param c a third number
     * @param n This one on a new line
     * @param onePerLine an integer list, one per line
     */
    static void manualFormat(int a, int b, int c, int n, int[] onePerLine) {
        /* TODO From the function perspective, this is just 4 integers */
    }

    public static void main(String[] args) throws java.io.IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String[] words = reader.readLine().split(" ");
        int a = Integer.parseInt(words[0]);
        int b = Integer.parseInt(words[1]);
        int c = Integer.parseInt(words[2]);
        int n = Integer.parseInt(reader.readLine());
        int[] onePerLine = new int[3];
        for (int i = 0; i < 3; ++i) {
            onePerLine[i] = Integer.parseInt(reader.readLine());
        }

        manualFormat(a, b, c, n, onePerLine);
    }
}
