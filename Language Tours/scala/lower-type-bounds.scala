// https://docs.scala-lang.org/tour/lower-type-bounds.html
// A lower type bound declares a type as a supertype of another type. If we
// have `B >: A` we're saying that type parameter `B` or the abstract type `B`
// refers to a supertype of type `A`. In most cases, `A` will be the type
// parameter of the class and `B` will be the type parameter of a method

trait Node[+B] {
  def prepend(elem: B): Node[B]
}

case class ListNode[+B](h: B, t: Node[B]) extends Node[B] {
  def prepend(elem: B): ListNode[B] = ListNode(elem, this)
  def head: B = h
  def tail: Node[B] = t
}

// case class Nil[+B]() extends Node[B] {
//   def prepend(elem: B): ListNode[B] = ListNode(elem, this)
// }
