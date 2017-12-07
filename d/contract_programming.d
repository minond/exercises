// Contracts are development time assertions. When compiling using the
// `-release` flag, this code is omited. `assert` is one form of contract
// programming in D. Another is the `in`, `out`, and blocks used in function
// declarations:
//
//   long square_root(long x)
//   in {
//     assert(x >= 0);
//   }
//   out (result) {
//     assert((result * result) <= x && (result + 1 * (result + 1) > x));
//   }
//   body {
//     resultcast(long) std.math.sqrt(cast(real) x);
//   }
//
//
// Another one of these contracts is the `invariant` function that is a part of
// structs and classes. This is called after the constructor and before the
// destructors are called; Before entring a member function; and after exiting
// a member function.
import std.stdio : writeln;
import std.format : format;

struct Date {
  private {
    int year, month, day;
  }

  this(int year, int month, int day) {
    this.year = year;
    this.month = month;
    this.day = day;
  }

  invariant() {
    assert(year >= 1900);
    assert(month >= 1 && month <= 12);
    assert(day >= 1 && day <= 31);
  }

  string toString() const
  out (result) {
    assert(result == format("%.4d-%.2d-%.2d", year, month, day));
  }
  body {
    return format("%.4d-%.2d-%.2d", year, month, day);
  }
}

void main() {
  auto date = Date(2017, 12, 6);
  date.writeln;
}
