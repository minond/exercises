// The mixin function is a compile-time eval. Strings can be dynamically
// generated, they just can't rely on runtime information.
import std.stdio : writeln;

auto calculate(string op, T)(T l, T r) {
  return mixin("l " ~ op ~ " r");
}

void main() {
  mixin("int b = 5;");
  mixin(`writeln("Value of b is ", b, "\n");`);
  assert(b == 5);

  writeln("5 + 12 = ", calculate!"+"(5, 12));
  writeln("5 - 12 = ", calculate!"-"(5, 12));
  writeln("5 * 12 = ", calculate!"*"(5, 12));
  writeln("5 / 12 = ", calculate!"/"(5, 12));
}
