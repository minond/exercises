// https://docs.scala-lang.org/tour/for-comprehensions.html

case class User(val name: String, val age: Int)

val users = List(
  User("Hannah", 22),
  User("Marcos", 28),
  User("Mohamed", 22),
  User("Andi", 24),
  User("Ryan", 20)
)

val allNamesv1 = for (user <- users) yield user.name
val allNamesv2 = users.map(_.name)

val over23v1 = for (user <- users if (user.age > 23)) yield user.name
val over23v2 = users.filter(_.age > 23).map(_.name)

println(allNamesv1)
println(allNamesv2)
println(over23v1)
println(over23v2)


def permutationsWhereSumEquals(max: Int, value: Int): IndexedSeq[(Int, Int)] =
  for (i <- 0 until max; j <- i until max if i + j == value)
    yield (i, j)


println(permutationsWhereSumEquals(100, 43))

permutationsWhereSumEquals(100, 43) foreach {
  case (i, j) =>
    println(s"$i + $j = 43")
}


for (
  i1 <- List(0, 1);
  i2 <- List(0, 1);
  i3 <- List(0, 1);
  i4 <- List(0, 1)
) println(s"$i1$i2$i3$i4")
