import std.stdio : writefln;

// Standard stuff. `while`, `do/while`, c `for` loops, java-like `foreach`
// loops, labels that can be used in `break/continue`, etc.
void main() {
  auto testers = [[5, 15],
    [2, 3, 2, 3],
    [3, 6, 2, 9]];

  for (uint i = 0; i < testers.length; i++) {
    writefln("The average of %s is %s", testers[i], average(testers[i]));
  }
}

double average(int[] arr) {
  immutable len = arr.length;
  double accu = 0.0;

  while (arr.length) {
    accu += arr[0];
    arr = arr[1 .. $];
  }

  return accu / len;
}
