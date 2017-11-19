import std.stdio : writeln;
import std.random : uniform;

static assert(!__traits(compiles, add(1, 2)));
static assert(!__traits(compiles, sub(1, 2)));
static assert(!__traits(compiles, mul(1, 2)));
static assert(!__traits(compiles, div(1, 2)));

// Functions can be delcared as local functions, which have lexical scope.
// Nested functions are called delegates in D.
void main() {
  string defaultThing = "World";
  string separator = ", ";
  string exclamation = "!";

  void greeting(string greet, string thing = defaultThing) {
    writeln(greet, separator, thing, exclamation);
  }

  greeting("Hello");
  greeting("Hello", "Marcos");

  auto add(int lhs, int rhs) {
    return lhs + rhs;
  }

  auto sub(int lhs, int rhs) {
    return lhs - rhs;
  }

  auto mul(int lhs, int rhs) {
    return lhs * rhs;
  }

  auto div(int lhs, int rhs) {
    return lhs / rhs;
  }

  int a = 27;
  int b = 42;

  switch (uniform(0, 4)) {
  case 0:
    writeln(add(a, b));
    break;

  case 1:
    writeln(sub(a, b));
    break;

  case 2:
    writeln(mul(a, b));
    break;

  case 3:
    writeln(div(a, b));
    break;

  default: assert(0);
  }

  static assert(__traits(compiles, add(1, 2)));
  static assert(__traits(compiles, sub(1, 2)));
  static assert(__traits(compiles, mul(1, 2)));
  static assert(__traits(compiles, div(1, 2)));
}
