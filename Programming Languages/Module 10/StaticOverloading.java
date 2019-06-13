public class StaticOverloading {
  public static void main(String[] args) {
  }
}

abstract class LExp {
  abstract public LExp eval();
}

class LInt extends LExp {
  protected int i;

  public LInt(int i) {
    this.i = i;
  }

  public LExp eval() {
    return this;
  }

  public LExp add(LInt rhs) {
    return new LInt(this.i + rhs.i);
  }

  public LExp add(LString rhs) {
    return new LString(Integer.toString(this.i) + rhs.s);
  }
}

class LString extends LExp {
  protected String s;

  public LString(String s) {
    this.s = s;
  }

  public LExp eval() {
    return this;
  }

  public LString add(LInt rhs) {
    return new LString(this.s + Integer.toString(rhs.i));
  }

  public LString add(LString rhs) {
    return new LString(this.s + rhs.s);
  }
}

class LAdd extends LExp {
  protected LExp e1, e2;

  public LAdd(LExp e1, LExp e2) {
    this.e1 = e1;
    this.e2 = e2;
  }

  public LExp eval() {
    this.e1.add(this.e2);
  }
}
