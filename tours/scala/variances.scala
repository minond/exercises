// https://docs.scala-lang.org/tour/variances.html

class Foo[+A] // Covariant class
class Bar[-A] // Contravariant class
class Quux[A] // Invariant class

// == Covariant ==
//
// Making 'A covariant implies that for two types 'A and 'B where 'A <: 'B,
// then List['A] <: List['B]. Both Cat and Dog are subtypes of Animal. In
// Scala, List[Cat] is a List[Animal] and List[Dog] is a List[Animal]. This is
// because by default, List is defined as List[+A].
//
// Given Animal wen can use Dog
abstract class Animal {
  def name: String
}

case class Cat(name: String) extends Animal
case class Dog(name: String) extends Animal

def printAnimalNames(animals: List[Animal]): Unit =
  animals.foreach { animal => println(s"printAnimalNames() = ${animal.name}") }

printAnimalNames(List(Cat("Cat1"), Cat("Cat2")))


// == Contravariant ==
//
// Opposite of covariance. Making 'A contravariant implies that for two types
// 'A and 'B where 'A <: 'B, then Writer['B] <: Writer['A].
//
// Given Dog we can use Animal
abstract class Printer[-A] {
  def print(value: A): Unit
}

class AnimalPrinter extends Printer[Animal] {
  def print(animal: Animal): Unit =
    println(s"The animal's name is ${animal.name}")
}

class CatPrinter extends Printer[Cat] {
  def print(cat: Cat): Unit =
    println(s"The cat's name is ${cat.name}")
}

val catPrinter: Printer[Cat] = new CatPrinter
val animalPrinter: Printer[Animal] = new AnimalPrinter

catPrinter.print(Cat("Cat1"))
animalPrinter.print(Cat("Cat2"))
animalPrinter.print(Dog("Dog1"))



// == Invariance ==
//
// Neither covariant nor contravariant. Where 'A <: 'B, Class['A] NOT(<:)
// Class['B] and Class['B] NOT(<:) Class['A].
//
// Given Dog we can only use Dog (can't use Animal nor can we use a fictional
// BigDog class that is a subtype of Dog.)
class Container[A](init: A) {
  private var _val: A = init

  def getValue(): A =
    _val

  def setValue(v: A): Unit =
    _val = v
}

// We can't do this because Container is no a Contravariant class:
// val catContainer: Container[Cat] = new Container(Cat("Felix"))
// val animalContainer: Container[Animal] = catContainer
// animalContainer.setValue(Dog("Spot"))
// val cat: Cat = catContainer.getValue


// Another example: Let's take Scala's `trait Function1[-T, +R]` and notice how
// we are contravariant on the argument type and covariant on the return type.
// In other words, we can take a parent type and we can return a sub type.
// Assuming we need a function that looks like this:
//
//     'a => 'b
//
// We can pass a function with this type:
//
//     'superA => 'subB
//
// Where:
//
//     'a <: 'superA
//     'subB <: 'b
abstract class SmallAnimal extends Animal
case class Mouse(name: String) extends SmallAnimal

type CanEatMice[Eater, Eaten] = Function1[Eater, Eaten]

def somethingThatTakeCanEayMice[A, B](fn: CanEatMice[A, B]): Unit = ()
def takesAnimalReturnsMouse(a: Animal): Mouse = Mouse("m1")
def takesCatReturnsSmallAnimal(c: Cat): SmallAnimal = Mouse("m2")

somethingThatTakeCanEayMice[Animal, Mouse](takesAnimalReturnsMouse)
somethingThatTakeCanEayMice[Cat, SmallAnimal](takesCatReturnsSmallAnimal)

// Given that:
//
//     Cat <: Animal
//     Mouse <: SmallAnimal <: Animal
//
// And:
//
//     -A => +B
//
// Then:
//
//     (Animal => Mouse) <: (Cat => SmallAnimal)
//
// Because Cat (or A) is contravariant, meaning we can use Animal or other
// super types. And SmallAnimal (or B) is covariant, meaning we can use Mouse
// or other sub types. Leaving us with:
//
//     (Animal => Mouse) <: (Cat => SmallAnimal)
//
//
// Remember, argument types are *always* contravariant and return types are
// *always* covariant.


// All together now
final case class MySet[+Elem](data: Elem*) {
  def fold[Typ >: Elem](id: MySet[Typ])(op: (Typ, MySet[Typ]) => MySet[Typ]): MySet[Typ] =
    data.foldRight(id)(op)

  def add[Typ >: Elem](elem: Typ): MySet[Typ] =
    MySet((data :+ elem) :_*)

  def contains[Typ >: Elem](elem: Typ): Boolean =
    data.exists { _ == elem }

  def ∪[Typ >: Elem](other: MySet[Typ]): MySet[Typ] =
    other.fold(this) { (elem, set) =>
      if (!set.contains(elem)) set.add(elem)
      else set
    }
}

case class CatDog(name: String) extends Animal

val dogs: MySet[Dog] = MySet(Dog("goodboy"))
val cats: MySet[Cat] = MySet(Cat("kitty"))
val catdog = CatDog("alone in this world lives the little catdog")

val animals: MySet[Animal] = cats ∪ dogs

printf(s"animals.contains(Dog(goodboy)): ${animals.contains(Dog("goodboy"))}\n")
printf(s"dogs.contains(Dog(goodboy)): ${dogs.contains(Dog("goodboy"))}\n")
printf(s"cats.contains(Dog(goodboy)): ${cats.contains(Dog("goodboy"))}\n")
printf(s"cats ∪ dogs: ${cats ∪ dogs }\n")
printf(s"(cats ∪ dogs).contains(catdog): ${(cats ∪ dogs).contains(catdog)}\n")
printf(s"(cats ∪ dogs ∪ MySet(catdog)).contains(catdog): ${(cats ∪ dogs ∪ MySet(catdog)).contains(catdog)}\n")
