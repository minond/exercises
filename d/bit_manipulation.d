import std.bitmanip : bitfields;
import std.stdio : writeln;

struct BitVector {
  // Field widths must sum to 8, 16, 32, or 64.
  mixin(bitfields!(// Field 1
      uint, "x1", 2, int, "y1", 3, uint, "z1", 2, bool, "flag1", 1,// Field 2
      uint, "x2",
      2, int, "y2", 3, uint, "z2", 2, bool, "flag2", 1));
}

struct BadVector {
  bool flag;
  int x, y, z;
}

struct Vector {
  int x, y, z;
}

struct Flag {
  bool on;
}

void main() {
  writeln("Flag.sizeof = ", Flag.sizeof); // One bool = 1 8 bit field, 1 bite.
  writeln("Vector.sizeof = ", Vector.sizeof); // Three ints = 3 32 bit fields, 12 bites.

  // Three ints plus one bool but this is padded = 3 32 bit fields + 1 8 bit
  // field + 24 bit padding. From the website:
  // > As the compiler will add padding for variables with a size lower than
  // > the current OS memory layout (size_t.sizeof) e.g. bool, byte, char, it
  // > is recommended to start with fields of high alignments.
  writeln("BadVector.sizeof = ", BadVector.sizeof);

  // See notes in BitVector declration. Also watch this video:
  // http://dconf.org/2016/talks/sechet.html
  writeln("BitVector.sizeof = ", BitVector.sizeof);

  BitVector vec;
  writeln("vec = ", vec);
  writeln("vec.x1 = ", vec.x1);
  writeln("vec.y1 = ", vec.y1);
  writeln("vec.z1 = ", vec.z1);
  writeln("vec.flag1 = ", vec.flag1);
  writeln("vec.sizeof = ", vec.sizeof);

  vec.x1 = 1;
  vec.x1 = 2;
  vec.x1 = 3;
  vec.flag1 = 0;

  writeln("vec = ", vec);
  writeln("vec.x1 = ", vec.x1);
  writeln("vec.y1 = ", vec.y1);
  writeln("vec.z1 = ", vec.z1);
  writeln("vec.flag1 = ", vec.flag1);
  writeln("vec.sizeof = ", vec.sizeof);
}
