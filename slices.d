import std.stdio : writeln;

// Slices point to shared memory and are make up of the following:
//
//   T* prt;
//   size_t length;
void main() {
  int[] test1 = [3, 9, 11, 7, 2, 76, 90, 6];

  writeln("test = ", test1);
  writeln("head = ", test1[0]);
  writeln("tail = ", test1[1 .. $]);

  auto test2 = test1;
  auto test3 = test2[2 .. 5];

  test1[] *= 100;

  writeln(test1);
  writeln(test2);
  writeln(test3);

  assert(test1[0] == test2[0]);

  assert(test1.length == 8);
  assert(test2.length == 8);
  assert(test3.length == 3);
}
