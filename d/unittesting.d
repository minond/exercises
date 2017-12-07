// `unittest` blocks are allowed anywhere in your D code. Tests can be run by
// passing the `-unittest` flag to the compiler or with `dub test`. Pass `-cov`
// to get code coverage.
import std.stdio : writeln;

struct Vector3 {
  double x, y, z;

  double dot(Vector3 rhs) const {
    return x * rhs.x + y * rhs.y + z * rhs.z;
  }

  unittest {
    assert(Vector3(1, 0, 0).dot(Vector3(0, 1, 0)) == 0);
  }

  string toString() const {
    import std.string : format;
    return format("x:%.1f y:%.1f z:%.1f", x, y, z);
  }

  unittest {
    assert(Vector3(1, 2, 3).toString() == "x:1.0 y:2.0 z:3.0");
  }
}

void main() {
  Vector3 vec;
  writeln(vec);
}

unittest {
  import std.math : isNaN;

  Vector3 vec;

  assert(isNaN(vec.x));
  assert(isNaN(vec.y));
  assert(isNaN(vec.z));
}
