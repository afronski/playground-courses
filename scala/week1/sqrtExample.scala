object sqrtExample {

  def abs(x: Double): Double =
    if (x < 0.0) -x else x                        //> abs: (x: Double)Double

  def isGoodEnough(guess: Double, x: Double): Boolean =
    abs((guess * guess) - x) / x < 0.0001         //> isGoodEnough: (guess: Double, x: Double)Boolean
    
  def improve(guess: Double, x: Double): Double =
    ((x / guess) + guess) / 2.0                   //> improve: (guess: Double, x: Double)Double

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)           //> sqrtIter: (guess: Double, x: Double)Double

  def sqrt(x: Double): Double =
    sqrtIter(1, x)                                //> sqrt: (x: Double)Double

  sqrt(3)                                         //> res0: Double = 1.7320508100147274
  sqrt(1e-6)                                      //> res1: Double = 0.0010000001533016628
  sqrt(1e60)                                      //> res2: Double = 1.0000000031080746E30
}