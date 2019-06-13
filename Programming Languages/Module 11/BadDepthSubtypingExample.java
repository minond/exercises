/*
 * This compiles even though it should not. This is because Java allows for
 * depth subtyping in arrays (just like Dan's example of the bad depth
 * subtyping in records.)
 *
 *   if t1 <: t2
 *   then t1[] <: t2[]
 *
 *
 * Given the rule above, we can now declare methods that work with elements in
 * ColorPoint[] types and treat them as Point[] types instead. And in this very
 * function we can now mutate ColorPoint back to Point elements. All of which
 * is allowed by the type system.
 *
 * Instead, we now end up with a runtime error that looks like this:
 *
 *   Exception in thread "main" java.lang.ArrayStoreException: BadDepthSubtypingExample$Point
 *           at BadDepthSubtypingExample.resetPoint(BadDepthSubtypingExample.java:41)
 *           at BadDepthSubtypingExample.main(BadDepthSubtypingExample.java:51)
 */
public class BadDepthSubtypingExample {
  static class Point {
    public int x, y;

    Point(int x, int y) {
      this.x = x;
      this.y = y;
    }
  }

  static class ColorPoint extends Point {
    public int color;

    ColorPoint(int x, int y, int color) {
      super(x, y);
      this.color = color;
    }
  }

  public static void resetPoint(Point[] xs, int i) {
    xs[i] = new Point(0, 0);
  }

  public static void main(String[] args) {
    ColorPoint[] xs = new ColorPoint[10];

    for (int i = 0, len = 10; i < len; i++) {
      xs[i] = new ColorPoint(i * i, i * i, 155);
    }

    resetPoint(xs, 0);
    resetPoint(xs, 1);
  }
}
