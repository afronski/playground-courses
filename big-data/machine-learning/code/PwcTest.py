import unittest

from Pwc import pwc

# Testy dla metody PWC
# wyliczającej wektor prawdopodobieństwa klas (p)
# poprzez przeprowadzenie iteracji przybliżających prawdopodobieństwa
# warunkowe par klas (mi) do prawdopodobieństw wejściowych (r).
class TestClass(unittest.TestCase):
	def test_PWC_method_for_first_dataset(self):		
		r = [ 
			[ 0.0, 0.9, 0.4 ],
			[ 0.1, 0.0, 0.7 ],
			[ 0.6, 0.3, 0.0 ]
		    ]
		n = [
			[ 0.0, 1.0, 1.0 ],
			[ 1.0, 0.0, 1.0 ],
			[ 1.0, 1.0, 0.0 ]
		    ]
		maxit = 1000

		p, c, it = pwc(r, n, maxit)

		assert p != None
		assert c != None
		assert it == 18, str(it) + " != 18"
	
	def test_PWC_method_for_second_dataset(self):		
		r = [ 
			[ 0.00, 0.51, 0.53, 0.51 ],
			[ 0.49, 0.00, 0.54, 0.55 ],
			[ 0.47, 0.46, 0.00, 0.59 ],
			[ 0.49, 0.45, 0.41, 0.00 ]
		    ]
		n = [
			[ 0.0, 1.0, 1.0, 1.0 ],
			[ 1.0, 0.0, 1.0, 1.0 ],
			[ 1.0, 1.0, 0.0, 1.0 ],
			[ 1.0, 1.0, 1.0, 0.0 ]
		    ]
		maxit = 1000

		p, c, it = pwc(r, n, maxit)

		assert it == 16, str(it) + " != 16"

		assert round(p[0], 4) == 0.2618
		assert round(p[1], 4) == 0.2698
		assert round(p[2], 4) == 0.2541
		assert round(p[3], 4) == 0.2142

		assert round(c[0][1], 4) == 0.4925
		assert round(c[3][2], 4) == 0.4574
	
	def test_PWC_method_for_third_dataset(self):		
		r = [ 
			[ 0.00, 0.56, 0.51, 0.60 ],
			[ 0.44, 0.00, 0.96, 0.44 ],
			[ 0.49, 0.04, 0.00, 0.59 ],
			[ 0.40, 0.56, 0.41, 0.00 ]
		    ]
		n = [
			[ 0.0, 1.0, 1.0, 1.0 ],
			[ 1.0, 0.0, 1.0, 1.0 ],
			[ 1.0, 1.0, 0.0, 1.0 ],
			[ 1.0, 1.0, 1.0, 0.0 ]
		    ]
		maxit = 1000

		p, c, it = pwc(r, n, maxit)

		assert it == 24, str(it) + " != 24"
		
		assert round(p[0], 4) == 0.2860
		assert round(p[1], 4) == 0.3412
		assert round(p[2], 4) == 0.1624
		assert round(p[3], 4) == 0.2105

		assert round(c[0][1], 4) == 0.4560
		assert round(c[3][2], 4) == 0.5645
