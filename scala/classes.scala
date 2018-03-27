// https://docs.scala-lang.org/tour/classes.html

// Getters and setters
case class Coordinates(
    bounds: Int,
    private var _x: Int = 0,
    private var _y: Int = 0
) {
  def x = _x

  def x_=(update: Int): Unit = {
    if (update <= bounds) _x = update
    else println(s"WARNING: x coordinate must be less or equal to $bounds.")
  }

  def y = _y

  def y_=(update: Int): Unit = {
    if (update <= bounds) _y = update
    else println(s"WARNING: y coordinate must be less or equal to $bounds.")
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val p = Coordinates(100)

    p.y = 101
    println(p.y)

    p.y = 10
    println(p.y)

    println(p)
  }
}
