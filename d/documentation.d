// D lets you generate documentation using `dmd -D` when you stick the
// following rules:
//
//     /// Three slashes before a type or a function
//     /++ Multiline comment with two plus signs +/
//     /** Multiline comment with two asterik signs */
//
//
// Run `dmd -D documentation.d && open documentation.html` to see what the
// comment below turns into.

/**
  Calculates the square root of a number.

  Here could be a longer paragraph that
  elaborates on the great win for
  society for having a function that is actually
  able to calculate a square root of a given
  number.

  Example:
  -------------------
  double sq = sqrt(4);
  -------------------
  Params:
    number = the number the square root should
             be calculated from.

  License: use freely for any purpose
  Throws: throws nothing.
  Returns: the square root of the input.
*/
T sqrt(T)(T number) {
  return number * 2;
}

void main() {
  import std.stdio : writeln;
  writeln("Hi");
}
