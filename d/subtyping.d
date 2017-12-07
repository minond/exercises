import std.stdio : writeln;

struct Vector3 {
  private double[3] vec;
  alias vec this;

  double dot(Vector3 rhs) {
    return vec[0] * rhs.vec[0] + vec[1] * rhs.vec[1] + vec[2] * rhs.vec[2];
  }
}

void main() {
  Vector3 vec;

  // We can do this because of `alias vec this` and `double[3] vec`.
  vec = [0.0, 1.0, 2.0];

  assert(vec.length == 3);
  assert(vec[$ - 1] == 2.0);
}
