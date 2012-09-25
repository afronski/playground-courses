import scala.annotation.tailrec

object funsets {

  def factorial(n : Int): Int = {
  	def factorialStep(mul: Int, n : Int): Int =
  		if (n == 0) mul else factorialStep(mul * n, n - 1)
  		
  	factorialStep(1, n)
  }
  
  factorial(5) 
}
