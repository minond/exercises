// Some builtin attributes. From memory.d:
//
//   D has C pointers; `new` allocates on the heap; and the GC acts as
//   expected.
//
//   - @system: this is the default.
//
//   - @safe: this is a D-ism that will prevent memory bugs. For example,
//     pointer arithmetic is not allowed, so any functions that do it and are
//     also tagged as `@safe` will result in a compiler error. `@safe`
//     functions can only call other `@safe` or `@trusted` functions.
//
//   - @trusted: this could be a safe or an unsafe function, but it is one that
//     is trusted and can therefore be called by `@safe` functions.
//
//
//   New attributes:
//
//   - @property: this makes a function look like a property
//
//   - @nogc: the compiler makes sure that no memory is allocated in functions
//     annotated with @nogc. A @nogc function is allowed to only call other
//     @nogc functions.
import std.stdio : writeln;

struct Bar {
  this(int x) {
  }
}

struct Foo {
  int x;

  @property bar() {
    return x;
  }

  @property bar(int x) {
    this.x = x;
  }

  @("One") @("Two") @Bar(321) int foo() {
    return x * x;
  }
}

void main() {
  Foo foo;

  writeln("__traits(getAttributes, Foo.foo):");
  foreach (attr; __traits(getAttributes, Foo.foo)) {
    writeln(attr.stringof);
  }

  foo.bar = 123; // same as `foo.bar(123)`
  writeln(foo.bar); // save as `foo.bar()`
}
