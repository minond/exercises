import std.bigint : BigInt;
import std.datetime : benchmark, to;
import std.functional : memoize, reverseArgs;
import std.stdio : writefln, writeln;

BigInt bigPow(uint base, uint power) pure {
  BigInt result = 1;

  foreach (_; 0 .. power) {
    result *= base;
  }

  return result;
}

void main() {
  alias fastBigPow = memoize!(bigPow);

  void test() {
    writefln(".uintLength() = %s ", fastBigPow(5, 100000).uintLength);
  }

  foreach (i; 0 .. 10) {
    benchmark!test(1)[0].to!("msecs", double).reverseArgs!writefln(" took: %.2f miliseconds");
  }
}
