object SumOfMultiples {
  def sumOfMultiples(factors: Set[Int], limit: Int): Int =
    (0 until limit).filter { num =>
      factors.find { num % _ == 0 } isDefined
    } sum
}
