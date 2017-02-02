object SumOfMultiples {
  def sumOfMultiples(factors: Set[Int], limit: Int): Int =
    (0 to limit - 1).filter { num =>
      factors.find { num % _ == 0 } isDefined
    } sum
}
