package week5

object Lists {
  val fruit = List("apples", "oranges", "pears")
  val nums = List(1, 2, 3)
  val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))

  def removeAt(n: Int, xs: List[Int]) = (xs take n) ::: (xs drop n + 1)

  removeAt(1, nums)
}