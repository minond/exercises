import std.stdio : writeln;

// Generics. The type is placed in a set of params and is replaced by the
// compiler when it sees a `!type` expression.
//
//   auto add(T)(T l, T r) {
//     return l + r;
//   }
//
//   add!int(1, 2);
//   add!float(1.32f, 2.54f);
//   add!Animal(cat, dog); // Error: Animal does not implement `+`
class Animal(string noise) {
  void makeNoise() {
    writeln(noise ~ "!");
  }
}

class Dog : Animal!("woof") {
}

class Cat : Animal!("meow") {
}

void multipleMakeNoise(T)(T noiseMaker, int n) {
  for (uint i = 0; i < n; i++) {
    noiseMaker.makeNoise();
  }
}

void main() {
  auto dog = new Dog;
  auto cat = new Cat;

  multipleMakeNoise(dog, 10);
  multipleMakeNoise(cat, 10);
}
