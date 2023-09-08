// D lets you override operators like +, -, and even () for classes and
// structs.
import std.variant : Variant;
import std.stdio : writeln;

struct var {
  private Variant[string] values;

  @property Variant opDispatch(string name)() const {
    return values[name];
  }

  @property void opDispatch(string name, T)(T val) {
    values[name] = val;
  }
}

void main() {
  var test;
  test.foo = "hey ";
  test.bar = 123;
  writeln(test.foo, test.bar);
}
