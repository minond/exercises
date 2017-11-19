import std.stdio : writeln;

// D has C pointers; `new` allocates on the heap; and the GC acts as expected.
//
// Example of a security level. There are three security levels:
//   - @system: this is the default.
//
//   - @safe: this is a D-ism that will prevent memory bugs. For example,
//     pointer arithmetic is not allowed, so any functions that do it and are
//     also tagged as `@safe` will result in a compiler error. `@safe`
//     functions can only call other `@safe` or `@trusted` functions.
//
//   - @trusted: this could be a safe or an unsafe function, but it is one that
//     is trusted and can therefore be called by `@safe` functions.

void safeFun() @safe {
  writeln("Hello, World!");
  int* p = new int;
}

void unsafeFun() {
  int* p = new int;
  int* fiddling = p + 5;
}

void main() {
  int a;
  int* b = &a;
  auto c = &a;

  writeln(a);
  writeln(b);
  writeln(c);

  safeFun();
  unsafeFun();
}
