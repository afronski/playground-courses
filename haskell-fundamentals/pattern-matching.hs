-- Redefining 'fst' and 'snd' functions from standard library, using pattern matching.
fst' (a, b) = a
snd' (a, b) = b

-- Redefining 'null' function from standard library using pattern matching.
null' [] = True
null' (x : xs) = False

-- Dealing with problematic cases.
head' (x : xs) = x
head' [] = error "Head of empty list."

-- Removing problematic 'head' and 'tail' functions which can crash
-- with use of pattern matching - suggested form of 'double' function implementation.
double [] = []
double (x : xs) = (2 * x) : (double xs)

-- Implementing 'pow2' with pattern matching and guards mechanism.
pow2 n
  | n == 0      = 1
  | otherwise   = 2 * (pow2 (n - 1))

-- Another implementation of 'double' with case expression
-- used for pattern maching inside function.
-- You cannot use guards in case expression!
double' nums = case nums of
  []        -> []
  (x : xs)  -> (2 * x) : (double xs)