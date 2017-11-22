import std.stdio : writefln;
import std.array : array;
import std.algorithm.iteration : filter;
import std.range : iota;
import std.uni : toLower;

// Uniform Function Call Syntax (UFCS) makes `foo(a)` the same as `a.foo()`. If
// the compiler sees this, it will first check if `a` has a `foo` member
// function and if it can't find one it'll look for a global (global, not
// local) function that has a first parameter the same type as `a`.
//
//   foo(a) == a.foo();
//   foo(bar(a)) == a.bar().foo();
//
//
// Also, parens are optional:
//
//   import std.uni : toLower;
//   "D rox".toLower() == "D rox".toLower
void main() {
  "Hello, %s".writefln("UFCS");

  assert(foo(bar("Marcos")) == "Marcos".bar().foo());
  assert("D rocks".toLower == "D rocks".toLower());
  assert(10.iota.filter!(a => a % 2 == 0).array == array(filter!(a => a % 2 == 0)(iota(10))));
}

string foo(string str) {
  return "foo" ~ str ~ "foo";
}

string bar(string str) {
  return "bar" ~ str ~ "bar";
}
