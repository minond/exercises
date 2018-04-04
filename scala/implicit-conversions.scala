// https://docs.scala-lang.org/tour/implicit-conversions.html
// An implicit conversion form type `S` to type `T` is defined by an implicit
// value which has function type `S => T`, or by an implicit method converible
// to a value of that type.

object Main {
  import scala.language.implicitConversions

  case class Dog(name: String)
  case class Cat(name: String)

  implicit def catDog(c: Cat): Dog = Dog(s"${c.name} but now a dog")
  implicit def dogCat(d: Dog): Cat = Cat(s"${d.name} but now a cat")

  def main(args: Array[String]): Unit = {
    val dog = Dog("I'm a dog")
    val cat = Cat("I'm a cat")

    val cat2: Cat = dog
    val dog2: Dog = cat

    val cat3: Cat = dog2
    val dog3: Dog = cat2

    println(cat3)
    println(dog3)
  }
}
