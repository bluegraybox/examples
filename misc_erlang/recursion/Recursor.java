public class Recursor {
    public static void main(String[] args) {
        System.out.println("Total: " + countdown(10));
    }

    private static int countdown(int x) {
        if (0 == x) {
            return 1/0;
        }
        else {
            return 1 + countdown(x - 1);
        }
    }
}
