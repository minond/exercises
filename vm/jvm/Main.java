public class Main {
    int base;

    public Main(int base) {
        this.base = base;
    }

    public int processIt(int x) {
        return x + x + base;
    }

    public String processIt(String x) {
        return x + x;
    }

    public static void main(String[] args) {
        Main main = new Main(10);
        try {
            System.out.println(main.processIt(10));
            System.out.println(main.processIt("10"));
        } catch (Exception err) {
            System.out.println("err");
        }
    }
}
