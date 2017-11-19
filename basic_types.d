import std.stdio : writeln;

// bool                        8 bit
// byte, ubyte, char           8 bit
// short, ushort, wchar       16 bit
// int, uint, dchar, float    32 bit
// long, ulong, double        64 bit
// real                       >= 64 bit (80 bit on x86)

// char         UTF-8 characters
// wchar        UTF-16 characters
// dchar        UTF-32 characters

// Notes:
// - The compiler won't allot a variable conversion that results loss in precision.
// - Casting is done with `case(Type) Variable`.
// - C++'s `auto` exists.
// - Zero values are derived using the `.init` property of the type.
// - Number types have a `.`min` and a `.max` property.`
void main() {
  int thousands = 42_832_000;
  uint thousands2 = thousands; // ok
  short lessSo = cast(short) thousands; // cast required

  int g; // default value of int is 0
  assert(g == 0);

  auto pi = 3.14f;

  writeln("Type of pi is ", typeid(pi));
  writeln("Type of an int variable is ", typeid(thousands));
  writeln("String type of an int variable is ", int.stringof);
  writeln("Zero-value of int is ", int.init);
  writeln("Size of int is ", int.sizeof);
  writeln("Maximum value of int is ", int.max);
  writeln("Minimum value of int is ", int.min);
}
