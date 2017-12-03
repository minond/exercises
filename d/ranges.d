import std.array : array;
import std.range : take;
import std.stdio : writeln;

struct FibonacciRange {
  int curr = 1;

  int front() const @property {
    return FibonacciRange.fib(curr);
  }

  bool empty() const @property {
    return false;
  }

  void popFront() {
    curr++;
  }

  int fib(int i) const {
    if (i == 1 || i == 2) {
      return 1;
    }
    else {
      return fib(i - 1) + fib(i - 2);
    }
  }
}

// `foreach` is turned transformed by the compiler:
//
//   foreach (elem; range) {
//     ...
//   }
//
//   for (auto cp = range; !cp.empty; cp.popFront()) {
//     auto elem = cp.front;
//     ...
//   }
//
//
// This means that if the range is some sort of reference, then once it is
// consumed it cannot be consumed again since all the elements were popped off
// the list. Any type that meets the following contract is considered a range:
//
//   struct Range {
//     T front() const @property;
//     bool empty() const @property;
//     void popFront();
//   }
//
//
// - The `front` and `empty` functions do not have to be declared as `const`
// functions.
//
// - The `std.range.take(Range, N)` function returns another `Range` that
// returns `N` elements at maximum. This range is lazy and only goes back to
// the original range var is needed.
//
// - I'm guessing that `std.array.array` does a `foreach` to extract all
// elements of a range.
void main() {
  FibonacciRange fibr;

  auto fib10 = take(fibr, 10);
  int[] fib10arr = array(fib10);

  writeln("The 10 first Fibonacci numbers: ", fib10arr);
  assert(fib10arr == [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]);
}
