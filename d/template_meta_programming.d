// `static if` is evaluated at compile time
import std.stdio : writeln;

struct Foo {
  int x;
}

void getTemplateMessage(T)(T arg) {
  static if (is(T == int)) {
    writeln("T is an int: ", arg);
  } else if (is(T == char)) {
    writeln("T is a char: ", arg);
  } else if (is(T == string)) {
    writeln("T is a string: ", arg);
  } else if (is(T == Foo)) {
    writeln("T is a Foo: ", arg);
  } else if (is(T == bool)) {
    writeln("T is a bool: ", arg);
  } else {
    writeln("I don't know what T is but it's not an int: ", arg);
  }
}

void main() {
  Foo foo;
  getTemplateMessage(foo);
  getTemplateMessage(3);
  getTemplateMessage(32.4);
  getTemplateMessage("s");
  getTemplateMessage(true);
  getTemplateMessage('a');
}
