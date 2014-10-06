object objects {
    val x = new Rational(1, 2)

    x.numerator                
    x.denominator      

    val y = new Rational(2, 3)                     
    x.add(y)                                      

    val x1 = new Rational(1, 3)                 
    val y1 = new Rational(5, 7)                
    val z1 = new Rational(3, 2)               

    x1.sub(y1).sub(z1)                       
    y1.add(y1)                              
  
    x.less(y)                              
    x < y                                 
  
    x.max(y)                             
  
    val whole = new Rational(2)         
    -whole                             
}

// Primary constructor.
class Rational(N: Int, D: Int) {
	require(D != 0, "Denominator cannot be equal to zero.")

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

	// Different constructor.
	def this(x: Int) = this(x, 1)

    def numerator = N
    def denominator = D

    def less(rhs: Rational) =
		numerator * rhs.denominator < rhs.numerator * denominator

	def < (rhs: Rational) =
		numerator * rhs.denominator < rhs.numerator * denominator

	def max(rhs: Rational) =
		if (this < rhs) rhs else this

    def add(rhs: Rational) =
        new Rational(numerator * rhs.denominator + rhs.numerator * denominator,
                     denominator * rhs.denominator)

    def neg = new Rational(-numerator, denominator)
    def unary_- = new Rational(-numerator, denominator)

    def sub(rhs: Rational) = add(rhs.neg)

    override def toString() = {
  	    val g = gcd(N, D)
        numerator / g + "/" + denominator / g
    }
}
