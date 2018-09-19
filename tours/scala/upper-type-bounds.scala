// https://docs.scala-lang.org/tour/upper-type-bounds.html
// An upper type bound `T <: A` says that type `T` refers to a subtype of type
// `A`.

abstract class Animal {
  def name: String
}

abstract class Pet extends Animal

class Cat extends Pet {
  override def name = "Cat"
}

class Dog extends Pet {
  override def name = "Dog"
}

class Lion extends Animal {
  override def name = "Lion"
}

class PetContainer[P <: Pet](p: P) {
  def pet: P = p
}

val dogContainer = new PetContainer[Dog](new Dog)
val catContainer = new PetContainer[Cat](new Cat)

// The next line will not compile and results in the following error:
//
//     error: type arguments [this.Lion] do not conform to class PetContainer's
//     type parameter bounds [P <: this.Pet]
//
// This is because PetContainer.pet is of type `P` which is declared as
// `P <: Pet`, meaning that P has to be a subtype of Pet. If `P` instead were
// `P <: Animal`, then PetContainer[Lion] would work.
val lionContainer = new PetContainer[Lion](new Lion)
