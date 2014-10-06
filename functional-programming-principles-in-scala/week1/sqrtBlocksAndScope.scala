object sqrtBlocksAndScope {
  
  def abs(x: Double): Double =
  	if (x < 0.0) -x else x                    

  def sqrt(x: Double): Double = {
    def isGoodEnough(guess: Double): Boolean =
      abs((guess * guess) - x) / x < 0.0001
      
    def improve(guess: Double): Double =
      ((x / guess) + guess) / 2.0

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    sqrtIter(1)
  }   

  sqrt(3)                                       
  sqrt(1e-6)                                   
  sqrt(1e60)                                  
}
