package week6

object polynomials {
    class Poly(terms0: Map[Int, Double]) {
        val terms = terms0 withDefaultValue 0.0

        def this(bindings: (Int, Double)*) =
            this(bindings.toMap)

      def adjust(term: (Int, Double)): (Int, Double) = {
        val (exp, coeff) = term
        exp -> (coeff + terms(exp))
      }

        def inefficientAdd (other: Poly) =
            new Poly(terms ++ (other.terms map adjust))

        def + (other: Poly) =
            new Poly((other.terms foldLeft terms)(addTerm))

        def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
            val (exp, coeff) = term
            terms + (exp -> (coeff + terms(exp)))
        }

        override def toString =
            (for ((exp, coeff) <- terms.toList.sorted.reverse)
                yield (exp match {
                    case 0 => coeff
                    case 1 => coeff + "x"
                    case _ => coeff + "x^" + exp
                })) mkString " + "
    }

    val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
    val p2 = new Poly(0 -> 3.0, 3 -> 7.0)

    p1 + p2
    p1.terms(7)
}