import std.stdio : writefln;

// On the difference between `function` vs. `delegate`. Not too sure about this
// one, but it seems that global functions can be typed as `function`s and
// local ones and/or member functions have to be of type `delegate`.
//
//   void doSomething(int delegate(int, int) action);
//   void doSomething(int function(int, int) action);
//
//
// - `std.function.toDelegate` turns a function into a delegate.
// - Annonymous functions and lambdas exist.
// - String literals can be used as lambdas...
//
//   auto f = (int l, int r) {
//     return l + r;
//   }
//
//   auto f = (int l, int r) => l + r;
//
//   [1, 2, 3].reduce!"a + b";

enum IntOps {
  add = 0,
  sub = 1,
  mul = 2,
  div = 3
}

auto getMathOperation(IntOps op) {
  auto add = (int l, int r) => l + r;
  auto sub = (int l, int r) => l - r;
  auto mul = (int l, int r) => l * r;
  auto div = (int l, int r) => l / r;

  final switch (op) {
  case IntOps.add:
    return add;

  case IntOps.sub:
    return sub;

  case IntOps.mul:
    return mul;

  case IntOps.div:
    return div;
  }
}

void main() {
  int a = 32;
  int b = 54;

  auto func = getMathOperation(IntOps.add);

  writefln(`Type: "%s"`, typeof(func).stringof);
  writefln(`Result: %s`, func(a, b));
}
