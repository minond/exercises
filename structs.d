import std.stdio : writeln;
import std.math : sqrt;

struct Vector3 {
  double x;
  double y;
  double z;

  double length() const {
    return sqrt(x * x + y * y + z * z);
  }

  double dot(Vector3 rhs) const {
    return x * rhs.x + y * rhs.y + z * rhs.z;
  }
}

void main() {
  Vector3 vec1 = Vector3(10, 0, 0);
  Vector3 vec2;

  vec2.x = 0;
  vec2.y = 20;
  vec2.z = 0;

  auto vec3 = Vector3(1, 2, 3);

  assert(vec1.length == 10);
  assert(vec2.length == 20);
  assert(vec1.dot(vec2) == 0);
  assert(vec3.dot(Vector3(1, 1, 1)) == 6);
  assert(vec3.dot(Vector3(3, 2, 1)) == 10);
}
