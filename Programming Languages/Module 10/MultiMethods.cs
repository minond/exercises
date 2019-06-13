/**
 * Thanks to msdn for the code example.
 *
 * https://blogs.msdn.microsoft.com/laurionb/2009/08/13/multimethods-in-c-4-0-with-dynamic/
 */

using System;

namespace MultiMethods {
  class Program {
    class Thing {}
    class Asteroid : Thing {}
    class Spaceship : Thing {}

    static void CollideWithImpl(Asteroid x, Asteroid y) {
      Console.WriteLine("Asteroid hits an Asteroid");
    }

    static void CollideWithImpl(Asteroid x, Spaceship y) {
      Console.WriteLine("Asteroid hits a Spaceship");
    }

    static void CollideWithImpl(Spaceship x, Asteroid y) {
      Console.WriteLine("Spaceship hits an Asteroid");
    }

    static void CollideWithImpl(Spaceship x, Spaceship y) {
      Console.WriteLine("Spaceship hits a Spaceship");
    }

    static void CollideWith(Thing x, Thing y) {
      dynamic a = x;
      dynamic b = y;
      CollideWithImpl(a, b);
    }

    static void Main(string[] args) {
      var asteroid = new Asteroid();
      var spaceship = new Spaceship();
      CollideWith(asteroid, spaceship);
      CollideWith(spaceship, spaceship);
    }
  }
}
