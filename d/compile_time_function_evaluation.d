// static, enum, and immutable are evaluated at runtime
import std.math : abs;
import std.stdio : writeln;

auto sqrt(T)(T x) {
  auto goodNuff = 0.01;
  T z = x*x, old = 0;
  int iter;

  while (abs(z - old) > goodNuff) {
    old = z;
    z -= ((z*z)-x) / (2*z);
  }

  return z;
}

void main() {
  double n = 4.0;
  static cn = sqrt(4.0);

  writeln("The sqrt at runtime 4 = ", sqrt(n));
  writeln("The sqrt at compile time 4 = ", cn);
}
