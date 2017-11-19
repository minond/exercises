import std.stdio : writeln;
import std.random : uniform;

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
}
