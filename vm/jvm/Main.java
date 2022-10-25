public class Main {
    int base;

    public Main(int base) {
        this.base = base;
    }

    public int processIt(int x) {
        return x + x + base;
    }

    public static void main(String[] args) {
        Main main = new Main(10);
        System.out.println(main.processIt(10));
    }
}
