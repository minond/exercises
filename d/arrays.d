import std.stdio : writeln;

// Static arrays declared outside of a function are stored in static memory.
// Ones declared in a function are stored on the stack. Dynamic arrays are
// store on the heap.
//
// The `$` symbol denotes an array's length. `arr[$ - 1]` is shorthand for
// `arr[arr.length - 1]`.
//
// `.length` can be updated on dynamic arrays.
void main() {
  int[10] myNumbers;

  int size = 10;
  int[] numbers0 = new int[size];

  auto grid = new int[10][10];

  char[] unencrypted = ['w', 'e', 'l', 'c', 'o', 'm', 'e', 't', 'o', 'd'];
  char[] encrypted = encrypt(unencrypted, 16);

  assert(encrypted == ['m', 'u', 'b', 's', 'e', 'c', 'u', 'j', 'e', 't']);

  numbers0.length = 5;
  writeln("numbers0.length = ", numbers0.length);
  writeln("numbers0 = ", numbers0);

  numbers0.length = 10;
  writeln("numbers0.length = ", numbers0.length);
  writeln("numbers0 = ", numbers0);

  writeln("-----------------------------------------------------------------");

  int[10] numbers1 = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
  int[10] numbers2 = [9, 8, 7, 6, 5, 4, 3, 2, 1, 0];

  // adds all elements from both arrays together and stores result in new
  // array: `c[0] = a[0] + b[0]`, `c[1] = a[1] + b[1]`, etc.
  int[10] numbers3 = numbers1[] + numbers2[];
  int[10] numbers4 = numbers3[] / 3;

  writeln("numbers1 = ", numbers1);
  writeln("numbers2 = ", numbers2);
  writeln("numbers3 = ", numbers3);
  writeln("numbers4 = ", numbers4);

  numbers1[] += 3; // add 3 to all elements
  numbers2[] -= 3; // substract 3 from all elements
  numbers3[] /= 3; // divide all elements by 3
  numbers4[] *= 3; // multiply all elements by 3

  writeln("numbers1 = ", numbers1);
  writeln("numbers2 = ", numbers2);
  writeln("numbers3 = ", numbers3);
  writeln("numbers4 = ", numbers4);

  writeln("numbers1[0] = ", numbers1[0]);
  writeln("numbers1[$ - 1] = ", numbers1[$ - 1]);
}

char[] encrypt(char[] input, char shift) {
  auto result = input.dup;

  for (int i = 0; i < result.length; i++) {
    int update = result[i] + shift;

    if (update > 'z') {
      update = update - 'z' + 'a' - 1;
    }

    result[i] = cast(char) update;
  }

  return result;
}
