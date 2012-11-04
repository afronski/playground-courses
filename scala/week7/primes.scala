package week7

object primes {
  def from(n: Int): Stream[Int] = n #:: from(n + 1)

  val N = from(0)
  val times4 = N map (_ * 4)

  (times4 take 1000).toList

  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter (_ % s.head != 0))

  val primes = sieve(from(2))
  primes.take(100).toList

  def sqrtStream(x: Double): Stream[Double] = {
    def improve(guess: Double) = (guess + x / guess) / 2
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
  }

  sqrtStream(2).take(10).toList

  def isGoodEnough(guess: Double, x: Double) =
    math.abs((guess * guess - x) / x) < 0.0001

  sqrtStream(4).filter(isGoodEnough(_, 4)).take(10).toList
}