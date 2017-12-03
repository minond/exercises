import std.stdio : writeln;

// - Classes à la Java and C++.
//
// - They implicitly inherit from the `Object` class.
//
// - They are `new`ed on to the heap.
//
// - They are passed by reference and garbage collected.
//
// - There's `override`, `final`, `abstract` on methods and on classes, and
//   `super(...)` is used to call the parent constructor.
//
// - The `==` and `!=` operators compare contents of classes, so don't use them
//   when comparing to `null`. Use `is` in those cases:
//
//   MyClass c;
//
//   if (c == null) // error
//   if (c is null) // ok
class Any {
  protected {
    string type;
  }

  abstract string convertToString();

  this(string type) {
    this.type = type;
  }

  final string getType() {
    return type;
  }
}

class Integer : Any {
  private {
    int number;
  }

  this(int number) {
    super("integer");
    this.number = number;
  }

public:

  override string convertToString() {
    import std.conv : to;

    return to!string(number);
  }
}

class Float : Any {
  private {
    float number;
  }

  this(float number) {
    super("float");
    this.number = number;
  }

public:

  override string convertToString() {
    import std.string : format;

    return format("%.1f", number);
  }
}

void main() {
  Any[] anys = [new Integer(42), new Float(324.4)];

  foreach (any; anys) {
    writeln("type: ", any.getType);
    writeln("value: ", any.convertToString);
  }
}
