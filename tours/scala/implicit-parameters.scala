// https://docs.scala-lang.org/tour/implicit-parameters.html
object Evaler {
  def lookup(thing: Symbol)(implicit env: Map[Symbol, String]): String =
    env.get(thing).getOrElse("Undefined")
}

object Main {
  implicit val ages = Map[Symbol, Int]('marcos -> 28)

  // NOTE If this were an implicit value, we would end up with a conflict when
  // calling `Evaler.lookup('marcos)` since there's another implicit
  // `Map[Symbol, String]` in scope.
  val names = Map[Symbol, String]('marcos -> "Marcos")

  def info(of: Symbol)(implicit agesMap: Map[Symbol, Int]): String =
    s"${names.get(of)} is ${agesMap.get(of)} years old."

  def main(args: Array[String]): Unit = {
    println(info('marcos)) // Passes `ages` implicitly
    println(info('marcos)(Map('marcos -> 99))) // Passes `Map('marcos -> 99)` explicitly

    implicit val env = Map('marcos -> "Marcos Marcos Marcos")
    println(Evaler.lookup('marcos)) // Passes `env` implicit
    println(Evaler.lookup('marcos)(names)) // Passes `names` explicitly
  }
}
