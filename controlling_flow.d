import std.stdio : writeln;

// Regular stuff. But switches can use strings! They also have this cool range
// operator and multiple case values. Also, range overlaps cause a compiler
// error:
//
//   switch (1) {
//   case 0, 1, 2: break;
//   case 1: .. case 10: break;
//   }
//
//
//   > file.d(n): Error: duplicate case 1 in switch statement
//   > file.d(n): Error: duplicate case 2 in switch statement
void main() {
  if (1 == 1) {
    writeln("Yes.");
  }

  int c = 15;

  switch (c) {
  case 0, 1, 2:
    writeln("Value is 0 or 1 or 2. Good job.");
    break;

  case 3: .. case 9:
    writeln(c, " is between 0 - 9");
    break;

  case 10:
    writeln("Ten.");
    break;

  default:
    writeln("No soup for you.");
    break;
  }
}
