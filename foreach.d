import std.stdio : writeln;

// Java's `foreach` exists in D. `index, var; array` is the syntax. `index` is
// optional, the type of `var` is enduced, the type of `array` must be any
// array or a `range`. Only arrays have an index so `..` notation can only have
// a value. By default, `var` is a copy but the original reference can be used
// with the `ref` keyword being places before the variable.
void main() {
  auto arr = [[5, 15], // 20
    [2, 3, 2, 3], // 10
    [3, 6, 2, 9], // 20
    ];

  writeln("-----------------------------");
  writeln(arr);

  foreach (e; arr) {
    writeln("-----------------------------");
    writeln(e);

    foreach (ref i; e) {
      writeln(i);
      i = 0;
    }
  }

  writeln("-----------------------------");
  writeln(arr);

  foreach (v; 0 .. 10) {
    writeln("- ", v);
  }

  foreach (i, v; [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]) {
    writeln("- ", i, ":", v);
  }
}
