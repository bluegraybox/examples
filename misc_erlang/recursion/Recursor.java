public class Recursor {
    public static void main(String[] args) {
        System.out.println("Total: " + countdown(10));
    }

    private static int countdown(int x) {
        return countdown(x, 0);
    }

    private static int countdown(int x, int total) {
        if (0 == x) {
            return 1/0;  // generate exception
        }
        else {
            return countdown(x - 1, total + 1);  // recurse with decreased countdown and increased total
        }
    }
}
