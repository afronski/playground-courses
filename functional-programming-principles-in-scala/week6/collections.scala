package week6

object CollectionTest {
  val xs = Array(1, 2, 3, 44)
  xs map (x => x * 2)

  val s = "Hello World"
  s filter (c => c.isUpper)

  s exists (c => c.isUpper)
  s forall (c => c.isUpper)

  val pairs = List(1,2,3) zip s
  pairs unzip

  s flatMap (c => List('.', c))

  xs.sum
  xs.max
  xs.product

  def isPrime(n: Int): Boolean =
    (2 until n) forall (n % _ != 0)

  isPrime(7)
  isPrime(10)

  def primeSumOfPairs(n: Int) =
    (1 until n) flatMap (i =>
      (1 until i) map (j => (i, j))) filter
        (pair => isPrime(pair._1 + pair._2))


  primeSumOfPairs(3)

  def scalarProduct(xs: List[Double], ys: List[Double]): Double =
    (for((x,y) <- xs zip ys) yield x * y).sum
}