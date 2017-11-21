import std.stdio : writeln;

interface Animal {
  void makeNoise();

  final void multipleMakeNoise(int n) {
    for (uint i = 0; i < n; i++) {
      makeNoise();
    }
  }
}

class Dog : Animal {
  override void makeNoise() {
    writeln("Woff");
  }
}

class Cat : Animal {
  override void makeNoise() {
    writeln("Meow");
  }
}

void main() {
  Animal dog = new Dog;
  Animal cat = new Cat;

  foreach (animal; [dog, cat])
    animal.multipleMakeNoise(10);
}
