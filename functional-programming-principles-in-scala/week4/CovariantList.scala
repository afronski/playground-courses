package week4

trait CovariantList[+T] {
    def isEmpty: Boolean
    def head: T
    def tail: CovariantList[T]
    
    def prepend[U >: T] (elem: U) : CovariantList[U] = new CovariantCons(elem, this)
}

class CovariantCons[T](val head: T, val tail: CovariantList[T]) extends CovariantList[T] {
  def isEmpty: Boolean = false
}

object CovariantNil extends CovariantList[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object Test {
  val x: CovariantList[String] = CovariantNil
}