import scala.annotation.tailrec

object funsets {

  def factorial(n: Int): Int = {
    def factorialStep(mul: Int, n: Int): Int =
      if (n == 0) mul else factorialStep(mul * n, n - 1)

    factorialStep(1, n)
  }

  factorial(5)

  def sum(f: (Int => Int), a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    
    loop(a, 0)
  }          
  
  sum(x => x * x, 1, 4)
  
  def product(f : Int => Int)(a : Int, b : Int) : Int =
  	if (a > b) 1
  	else f(a) * product(f)(a + 1, b)          
  	
  product(x => x * x)(3, 4)                  
  
  def fact(n : Int): Int =
  	product(x => x)(1, n)                   
  	
  fact(5)                                  
  
  def mapReduce(f : Int => Int, combine : (Int, Int) => Int, zero: Int)(a : Int, b : Int) : Int =
  	if (a > b) zero
  	else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
                                          
 	mapReduce(x => x, (x,y) => x * y, 1)(1, 5)
}
